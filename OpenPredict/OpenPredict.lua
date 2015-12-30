--[[
  OpenPredict 0.02a

  THIS SCRIPT IS STILL IN ALPHA STAGE THEREFORE YOU MAY EXPERIENCE BUGS.

  predictInfo
    members:
      .x, .y, .z
      .castPos
      .hitChance
      .meta

    methods:
      :hCollision(n)
      :mCollision(n)

  spellData
    .delay - Initial delay before the spell is cast.
    .speed - Projectile speed (if exists).
    .accel - Projectile acceleration.
    .minSpeed - Minimum projectile speed.
    .maxSpeed - Maximum projectile speed.
    .width - Full width of the spell (2 × radius).
    .range - Maximum range of the spell.

    .radius - Radius of the spell (width ÷ 2).
    .angle - Angle of the spell (used for GetConicAOEPrediction).

  Core methods:
    GetPrediction(unit, spellData, [sourcePos])
    GetLinearAOEPrediction(unit, spellData, [sourcePos])
    GetCircularAOEPrediction(unit, spellData, [sourcePos])
    GetConicAOEPrediction(unit, spellData, [sourcePos])
]]

-- Simple prerequisite
if _G.OpenPredict_Loaded then return end

local myHero = GetMyHero()

-- Constants
local TEAM_ENEMY = GetTeam(myHero) == 100 and 200 or 100
local IMMOBILE_BUFFS = { }

do
  local bL = GetBuffTypeList()
  IMMOBILE_BUFFS[bL.Stun] = true
  IMMOBILE_BUFFS[bL.Taunt] = true
  IMMOBILE_BUFFS[bL.Snare] = true
  IMMOBILE_BUFFS[bL.Fear] = true
  IMMOBILE_BUFFS[bL.Charm] = true
  IMMOBILE_BUFFS[bL.Suppression] = true
  IMMOBILE_BUFFS[bL.Flee] = true
  IMMOBILE_BUFFS[bL.Knockup] = true
  IMMOBILE_BUFFS[bL.Knockback] = true
end

local activeAttacks = { }
local activeDashes = { }
local activeImmobility = { }
local activeWaypoints = { }

local minionUnits = { }
local allyHeroes, enemyHeroes = { }, { }

local min, max, deg, acos, sin, sqrt, abs, epsilon = math.min, math.max, math.deg, math.acos, math.sin, math.sqrt, math.abs, 1e-9
local insert, remove = table.insert, table.remove

--[[
  predictInfo object

  Constructor:
    predictInfo.new(vec3)

  Methods:
    predictInfo:hCollision(n)
    predictInfo:hCollision(n)

  Members:
    predictInfo.x
    predictInfo.y
    predictInfo.z
    predictInfo.castPos
    predictInfo.hitChance
    predictInfo.meta
]]

local predictInfo = { }
predictInfo.__index = predictInfo

function predictInfo.new(vec3)
  local pI = { }
  setmetatable(pI, predictInfo)

  pI.x, pI.y, pI.z = vec3.x, vec3.y, vec3.z
  pI.castPos = vec3
  pI.hitChance = 0
  pI.meta = { }

  return pI
end

function predictInfo:hCollision(n)
  local result, threshold = { }, math.huge
  local source, sq_range = self.meta.source, self.meta.range and self.meta.range * self.meta.range or math.huge

  for _, enemy in pairs(enemyHeroes) do
    local p = GetOrigin(enemy)
    if sq_range == math.huge or (p.x - source.x) ^ 2 + (p.z - source.z) ^ 2 < sq_range then
      local t = CollisionTime(source, self.castPos, enemy, self.meta.delay, self.meta.speed, self.meta.width)

      if t and t > 0 then
        if n and #result + 1 > n then return true end
        insert(result, t < threshold and 1 or #result, enemy)
      end
    end
  end

  return #result > 0 and result
end

function predictInfo:mCollision(n)
  local result, threshold = { }, math.huge
  local source, sq_range = self.meta.source, self.meta.range and self.meta.range * self.meta.range or math.huge

  for minion in GetMinions(TEAM_ENEMY) do
    local p = GetOrigin(minion)
    if sq_range == math.huge or (p.x - source.x) ^ 2 + (p.z - source.z) ^ 2 < sq_range then
      local t = CollisionTime(source, self.castPos, minion, self.meta.delay, self.meta.speed, self.meta.width)

      if t and t > 0 then
        if n and #result + 1 > n then return true end
        insert(result, t < threshold and 1 or #result, minion)
      end
    end
  end

  return #result > 0 and result
end

-- API
function GetPrediction(unit, spellData, sourcePos)
  -- Fail-safe conversions
  local delay = spellData.delay or 0
  local speed = spellData.speed or math.huge
  local width = spellData.width or spellData.radius and 2 * spellData.radius or 1
  local range = spellData.range or math.huge
  local source = sourcePos or GetOrigin(myHero)

  -- Construct predictInfo object
  local pI = predictInfo.new(GetOrigin(unit))
  pI.meta = { target = unit, delay = delay, speed = speed, width = width, range = range, source = source }
  pI.hitChance = 0

  local nID = GetNetworkID(unit)

  if activeWaypoints[nID] and activeWaypoints[nID].count > 0 and IsMoving(unit) then
    local sP, timeElapsed = GetOrigin(unit), 0

    for i, eP in GetWaypoints(unit) do
      -- Prerequisite variables
      local dx, dy, dz = eP.x - sP.x, eP.y - sP.y, eP.z - sP.z
      local magnitude = sqrt(dx * dx + dz * dz)
      local velocity = activeDashes[nID] or GetMoveSpeed(unit)
      local travelTime = magnitude / velocity

      dx, dy, dz = (dx / magnitude) * velocity, dy / magnitude, (dz / magnitude) * velocity

      local t = GetLatency() * 0.001 + delay

      -- Calculate the interception time
      if speed ~= math.huge then
        local a = (dx * dx) + (dz * dz) - (speed * speed)
        local b = 2 * ((sP.x * dx) + (sP.z * dz) - (source.x * dx) - (source.z * dz))
        local c = (sP.x * sP.x) + (sP.z * sP.z) + (source.x * source.x) + (source.z * source.z) - (2 * source.x * sP.x) - (2 * source.z * sP.z)
        local discriminant = (b * b) - (4 * a * c)

        local t1 = (-b + sqrt(discriminant)) / (2 * a)
        local t2 = (-b - sqrt(discriminant)) / (2 * a)

        -- Greater of the two roots
        t = t + max(t1, t2)

        if spellData.accel then
          local v = speed + spellData.accel * t
          if spellData.minSpeed then v = math.max(spellData.minSpeed, v) end
          if spellData.maxSpeed then v = math.min(spellData.maxSpeed, v) end
          t = abs((v - speed) / spellData.accel)
        end
      end

      if i == activeWaypoints[nID].count - 1 or (t > 0 and t < timeElapsed + travelTime) then
        -- Calculate the point of interception
        local displacement = min((t - timeElapsed) * velocity, magnitude)
        pI.x, pI.y, pI.z = sP.x + displacement * (dx / velocity), sP.y + displacement * dy, sP.z + displacement * (dz / velocity)

        -- Compute the hit chance
        if activeDashes[nID] then
          -- Dash velocity is faster but the path is far less likely to be altered
          pI.hitChance = 0.99
        else
          -- Interception time, unit velocity and spell width/radius all influence the probability
          pI.hitChance = (width / velocity) / t

          -- Waypoint analysis using sample data
          local samples = activeWaypoints[nID].samples

          local rate = 1 -- Waypoint rate
          for i = 1, #samples do
            if GetGameTimer() - samples[i] < 0.25 then rate = rate + 1 end
          end

          if rate > 1 then
            local p1, p2, v = GetOrigin(unit), eP, samples.lastWaypointDir

            local dot = (p2.x - p1.x) * v.x + (p2.z - p1.z) * v.z
            local d1, d2 = sqrt((p2.x - p1.x) ^ 2 + (p2.z - p1.z) ^ 2), sqrt(v.x * v.x + v.z * v.z)
            local theta = max(1, deg(acos(dot / (d1 * d2))))

            pI.hitChance = pI.hitChance * (1 - ((theta * rate) / (180 * rate)))
          end
        end

        break
      end

      timeElapsed, sP = timeElapsed + travelTime, eP
    end
  else
    local sP = GetOrigin(unit)
    local t = GetLatency() * 0.001 + delay + sqrt((sP.x - source.x) ^ 2 + (sP.z - source.z) ^ 2) / speed

    -- Check if the unit is immobile/attacking (increased probability)
    local isImmobile, t1 = IsImmobile(unit)
    local isAttacking, t2 = IsAttacking(unit)

    if isImmobile then
      if t < t1 then
        pI.hitChance = 0.99
      else
        pI.hitChance = (width / GetMoveSpeed(unit)) / (t - t1)
      end
    elseif isAttacking then
      pI.hitChance = (width / GetMoveSpeed(unit)) / (t - t2)
    else
      pI.hitChance = (width / GetMoveSpeed(unit)) / t
    end
  end

  if range ~= math.huge then
    pI.hitChance = sqrt((pI.x - source.x) ^ 2 + (pI.z - source.z) ^ 2) < range and pI.hitChance or -1
  end

  pI.castPos = { x = pI.x, y = pI.y, z = pI.z }

  return pI
end

function GetLinearAOEPrediction(unit, spellData, sourcePos)
  local pI = GetPrediction(unit, spellData, sourcePos)

  if spellData.width and spellData.width > 1 and spellData.range and spellData.range < math.huge then
    -- So Lua treats tables as pointers
    local aoeCastPos, threshold = pI.castPos, (2 * spellData.width) ^ 2
    local p1, p2 = pI.meta.source, { x = pI.x, y = pI.y, z = pI.z }
    local dx, dy = p2.x - p1.x, p2.z - p1.z

    do -- Extend vector to match range
      local magnitude = math.sqrt(dx * dy + dy * dy)
      p2.x = p2.x + (dx / magnitude) * spellData.range
      p2.z = p2.z + (dy / magnitude) * spellData.range
    end

    -- Least Squares
    local points = { }
    table.insert(points, { x = aoeCastPos.x, y = aoeCastPos.z })

    for _, enemy in pairs(enemyHeroes) do
      if enemy ~= unit and IsVisible(enemy) and IsObjectAlive(enemy) and IsTargetable(enemy) and not IsImmune(enemy, myHero) then
        local castPos = GetPrediction(enemy, spellData, sourcePos).castPos

        -- Project castPos onto source-endPos
        local t = ((castPos.x - p1.x) * (p2.x - p1.x) + (castPos.z - p1.z) * (p2.z - p1.z)) / (spellData.range * spellData.range)
        local projection = { x = p1.x + t * dx, y = p1.z + t * dy }

        -- Check whether castPos in within spell boundary
        if (castPos.x - projection.x) ^ 2 + (castPos.z - projection.y) ^ 2 < threshold then
          table.insert(points, { x = castPos.x, y = castPos.z })
        end
      end
    end

    local nCount = #points

    if nCount > 1 then
      local x, y, x2, xy = 0, 0, 0, 0

      for i = 1, #points do
        x = x + points[i].x
        y = y + points[i].y
        x2 = x2 + points[i].x ^ 2
        xy = xy + points[i].x * points[i].y
      end

      local slope = (xy - x * (y / nCount)) / (x2 - x * (x / nCount))
      local intercept = (y / nCount) - slope * (x / nCount)

      aoeCastPos.z = slope * p1.x + intercept
      pI.x, pI.y, pI.z = aoeCastPos.x, aoeCastPos.y, aoeCastPos.z
    end
  end

  return pI
end

function GetLinearAOEPrediction2(unit, spellData, sourcePos)
  local pI = GetPrediction(unit, spellData, sourcePos)

  if spellData.width and spellData.width > 1 and spellData.range and spellData.range < math.huge then
    local aoeCastPos, threshold = pI.castPos, (2 * spellData.width) ^ 2
    local p1, p2 = pI.meta.source, { x = pI.x, y = pI.y, z = pI.z }
    local dx, dy = p2.x - p1.x, p2.z - p1.z

    do -- Extend vector to match range
      local magnitude = math.sqrt(dx * dy + dy * dy)
      p2.x = p2.x + (dx / magnitude) * spellData.range
      p2.z = p2.z + (dy / magnitude) * spellData.range
    end

    for _, enemy in pairs(enemyHeroes) do
      if enemy ~= unit and IsVisible(enemy) and IsObjectAlive(enemy) and IsTargetable(enemy) and not IsImmune(enemy, myHero) then
        local p = GetPrediction(enemy, spellData, sourcePos).castPos

        -- Project castPos onto source-endPos
        local d = ((castPos.x - p1.x) * dx + (castPos.z - p1.z) * dy)

        if sqrt(d) < spellData.range then
          local t = d / (spellData.range * spellData.range)
          local projection = { x = p1.x + t * dx, y = p1.z + t * dy }
          local perpendicular = (p.x - projection.x) ^ 2 + (p.z - projection.y) ^ 2

          -- Check whether castPos in within spell boundary
          if perpendicular < threshold then
            aoeCastPos.x, aoeCastPos.z = 0.5 * (aoeCastPos.x + p.x), 0.5 * (aoeCastPos.z + p.z)
            threshold = threshold - (0.5 * perpendicular)
          end
        end
      end
    end
  end

  return pI
end

function GetCircularAOEPrediction(unit, spellData, sourcePos)
  local pI = GetPrediction(unit, spellData, sourcePos)

  if (spellData.radius and spellData.radius > 1) or (spellData.width and spellData.width > 1) then
    local width = spellData.width or 1 * spellData.radius
    local aoeCastPos, threshold = pI.castPos, (2 * width) ^ 2

    for _, enemy in pairs(enemyHeroes) do
      if enemy ~= unit and IsVisible(enemy) and IsObjectAlive(enemy) and IsTargetable(enemy) and not IsImmune(enemy, myHero) then
        local p = GetPrediction(enemy, spellData, sourcePos).castPos
        local m_sq = (p.x - aoeCastPos.x) ^ 2 + (p.z - aoeCastPos.z) ^ 2

        if m_sq < threshold then
          aoeCastPos.x, aoeCastPos.z = 0.5 * (aoeCastPos.x + p.x), 0.5 * (aoeCastPos.z + p.z)
          threshold = threshold - (0.5 * m_sq)
        end
      end
    end

    pI.x, pI.y, pI.z = aoeCastPos.x, aoeCastPos.y, aoeCastPos.z
  end

  return pI
end

function GetConicAOEPrediction(unit, spellData, sourcePos)
  local pI = GetPrediction(unit, spellData, sourcePos)

  if spellData.angle and spellData.angle > 1 and spellData.range and spellData.range < math.huge then
    local aoeCastPos, threshold = pI.castPos, 2 * spellData.angle
    local p1, p2 = pI.meta.source, { x = pI.x, y = pI.y, z = pI.z }
    local dx, dy = p2.x - p1.x, p2.z - p1.z

    do -- Extend vector to match range
      local magnitude = math.sqrt(dx * dy + dy * dy)
      p2.x = p2.x + (dx / magnitude) * spellData.range
      p2.z = p2.z + (dy / magnitude) * spellData.range
    end

    for _, enemy in pairs(enemyHeroes) do
      if enemy ~= unit and IsVisible(enemy) and IsObjectAlive(enemy) and IsTargetable(enemy) and not IsImmune(enemy, myHero) then
        local p = GetPrediction(enemy, spellData, sourcePos).castPos
        local d1 = sqrt((p.x - p1.x) ^ 2 + (p.z - p1.z) ^ 2)

        if d1 < spellData.range then
          local d2 = sqrt((aoeCastPos.x - p1.x) ^ 2 + (aoeCastPos.z - p1.z) ^ 2)
          local dot = (aoeCastPos.x - p1.x) * (p.x - p1.x) + (aoeCastPos.z - p1.z) * (p.z - p1.z)

          local theta = deg(acos(dot / (d1 * d2)))
          if theta < threshold then
            aoeCastPos.x, aoeCastPos.z = 0.5 * (aoeCastPos.x + p.x), 0.5 * (aoeCastPos.z + p.z)
            threshold = theta
          end
        end
      end
    end

    pI.x, pI.y, pI.z = aoeCastPos.x, aoeCastPos.y, aoeCastPos.z
  end

  return pI
end

-- CALLBACKS
local MAX_SAMPLES = 10
local function OnProcessWaypoint(unit, waypointProc)
  local nID = GetNetworkID(unit)

  -- Handle new waypoint struct
  if not activeWaypoints[nID] or GetGameTimer() ~= activeWaypoints[nID].lastWaypointTime then
    local samples = activeWaypoints[nID] and activeWaypoints[nID].samples or { }
    local lastWaypointDir = { x = 1, y = 1, z = 1 }

    if activeWaypoints[nID] then
      local origin = GetOrigin(unit)
      local sP, eP = activeWaypoints[nID][activeWaypoints[nID].count], activeWaypoints[nID][1]
      lastWaypointDir.x = eP.x - sP.x
      lastWaypointDir.y = eP.y - sP.y
      lastWaypointDir.z = eP.z - sP.z
    end

    activeWaypoints[nID] = { }
    activeWaypoints[nID].samples = samples
    activeWaypoints[nID].samples.lastWaypointDir = lastWaypointDir
    activeWaypoints[nID].count = waypointProc.index
    activeWaypoints[nID].lastWaypointTime = GetGameTimer()
  end

  -- Add the new waypoint
  activeWaypoints[nID][waypointProc.index] = waypointProc.position

  -- Push/pop sample data
  if waypointProc.index == 1 and GetObjectType(unit) == Obj_AI_Hero then
    insert(activeWaypoints[nID].samples, 1, GetGameTimer())

    if #activeWaypoints[nID].samples > MAX_SAMPLES then
      remove(activeWaypoints[nID].samples)
    end
  end

  -- Handle dashes
  if waypointProc.dashspeed > GetMoveSpeed(unit) then
    activeDashes[nID] = waypointProc.dashspeed
  elseif activeDashes[nID] then
    activeDashes[nID] = nil
  end
end

local function OnUpdateBuff(unit, buff)
  if GetObjectType(unit) == GetObjectType(myHero) and IMMOBILE_BUFFS[buff.Type] then
    local nID = GetNetworkID(unit)

    if not activeImmobility[nID] or buff.ExpireTime > activeImmobility[nID].ExpireTime then
      activeImmobility[nID] = buff
    end
  end
end

local function OnRemoveBuff(unit, buff)
  if GetObjectType(unit) == GetObjectType(myHero) then
    local nID = GetNetworkID(unit)

    if activeImmobility[nID] and buff.Name == activeImmobility[nID].Name then
      activeImmobility[nID] = nil
    end
  end
end

local function OnObjectLoad(object)
  if object and GetObjectType(object) == Obj_AI_Hero then
    insert(GetTeam(object) == GetTeam(myHero) and allyHeroes or enemyHeroes, object)
  end
end

local inactiveMinions = { }
local function OnCreateObj(object)
  if IsMinionUnit(object) then
    insert(inactiveMinions, object)
  end
end

local function OnDeleteObj(object)
  local networkID = GetNetworkID(object)
  if networkID and networkID > 0 and networkID ~= math.huge then
    for i = 1, #minionUnits do
      if GetNetworkID(minionUnits[i]) == networkID then
        remove(minionUnits, i)
        break
      end
    end
  end
end

local function OnProcessSpellAttack(unit, attackProc)
  if GetObjectType(unit) == Obj_AI_Hero then
    local nID = GetNetworkID(unit)
    activeAttacks[nID] = { startTime = GetGameTimer(), windUpTime = attackProc.windUpTime, castSpeed = attackProc.castSpeed }
  end
end

--[[
  UTILITY

  IsMinionUnit - Returns true if passed unit is a valid minion.
  GetMinions - Minion iterator.
  GetWaypoints - Waypoint iterator.
  IsMoving - Returns true if unit is moving towards a waypoint.
  IsAttacking - Returns true if unit is attacking and their remaining wind-up time.
  IsImmobile - Returns true if unit is immobile and the remaining immobility time.
  CollisionTime - Calculates the time of trajectory collision.
]]

_G.OnTick(
  function()
  for i = #inactiveMinions, -1, 1 do
    local nID = GetNetworkID(inactiveMinions[i])
    if nID and nID > 0 and nID < math.huge then
      insert(minionUnits, inactiveMinions[i])
      remove(inactiveMinions, i)
    end
  end
end
)

IsMinionUnit = function(object)
  if object and GetObjectType(object) == Obj_AI_Minion then
    local team = GetTeam(object)

    if team and team > 0 and team % 100 == 0 then
      return GetHitBox(object) > 0 and GetMoveSpeed(object) > 0
    end
  end
end

GetMinions = function(team)
  local i, n = 0, #minionUnits

  return function()
    ::Retry::
      i = i + 1

    if i <= n then
      if minionUnits[i] and IsVisible(minionUnits[i]) and IsObjectAlive(minionUnits[i]) and (not team or GetTeam(minionUnits[i]) == team) then
        return minionUnits[i]
      else
        goto Retry
      end
    end
  end
end

GetWaypoints = function(unit)
  local nID = GetNetworkID(unit)

  if activeWaypoints[nID] then
    local aW = activeWaypoints[nID]
    local i, n = 0, aW.count

    -- Yey workarounds...
    local origin = GetOrigin(unit)
    for k = n, 1, -1 do
      if aW[k] and aW[k - 1] and (origin.x - aW[k].x) ^ 2 + (origin.z - aW[k].z) ^ 2 < (aW[k - 1].x - aW[k].x) ^ 2 + (aW[k - 1].z - aW[k].z) ^ 2 then
        i = n - k
        break
      end
    end

    return function()
      i = i + 1

      if i < n and aW[n - i] then
        return i, aW[n - i]
      end
    end
  end
end

IsMoving = function(unit)
  local nID = GetNetworkID(unit)

  if activeWaypoints[nID] then
    local wayPoint, origin = activeWaypoints[nID][1], GetOrigin(unit)
    local d = sqrt((wayPoint.x - origin.x) ^ 2 + (wayPoint.z - origin.z) ^ 2)

    return d > epsilon
  end

  return false
end

IsAttacking = function(unit)
  local nID = GetNetworkID(unit)

  if activeAttacks[nID] then
    local attack = activeAttacks[nID]
    return GetGameTimer() < attack.startTime + attack.windUpTime, (attack.startTime + attack.windUpTime) - GetGameTimer()
  end
end

IsImmobile = function(unit)
  local nID = GetNetworkID(unit)

  if activeImmobility[nID] then
    return GetGameTimer() < activeImmobility[nID].ExpireTime, activeImmobility[nID].ExpireTime - GetGameTimer()
  end
end

CollisionTime = function(startPos, endPos, unit, delay, speed, width)
  local startPath = GetOrigin(unit)

  -- Calculate the velocity from startPos to endPos
  local v1 = { x = endPos.x - startPos.x, y = endPos.z - startPos.z }
  local d1 = sqrt(v1.x * v1.x + v1.y * v1.y)

  -- Dynamic-dynamic collision
  if IsMoving(unit) then
    local endPath = activeWaypoints[GetNetworkID(unit)][1]
    v1.x, v1.y = (v1.x / d1) * speed, (v1.y / d1) * speed

    local v2 = { x = endPath.x - startPath.x, y = endPath.z - startPath.z }
    local d2 = sqrt(v2.x * v2.x + v2.y * v2.y)
    local mS = GetMoveSpeed(unit)
    v2.x, v2.y = (v2.x / d2) * mS, (v2.y / d2) * mS

    local p = { x = startPos.x - endPath.x, y = startPos.z - endPath.z }

    if p.x * p.x + p.y * p.y < d1 * d1 then
      local v = { x = v1.x - v2.x, y = v1.y - v2.y }

      local a = (v.x * v.x) + (v.y * v.y)
      local b = 2 * ((p.x * v.x) + (p.y * v.y))
      local c = ((p.x * p.x) + (p.y * p.y)) - (width + GetHitBox(unit)) ^ 2

      local discriminant = b * b - 4 * a * c

      if discriminant >= 0 then -- Two real roots
        local t1 = (-b + sqrt(discriminant)) / (2 * a)
        local t2 = (-b - sqrt(discriminant)) / (2 * a)

        -- Lesser of the two roots
        local t = min(t1, t2)
        return t > 0 and t
      end
    end
  else
    -- Dynamic-static collision
    local d2 = sqrt((startPath.x - startPos.x) ^ 2 + (startPath.z - startPos.z) ^ 2)

    if d2 < d1 then
      v1.x, v1.y = (v1.x / d1) * d2, (v1.y / d1) * d2

      if (startPath.x - (startPos.x + v1.x)) ^ 2 + (startPath.z - (startPos.z + v1.y)) < (GetHitBox(unit) + width) ^ 2 then
        return d2 / speed
      end
    end
  end
end

_G.OnProcessWaypoint(OnProcessWaypoint)

_G.OnUpdateBuff(OnUpdateBuff)
_G.OnRemoveBuff(OnRemoveBuff)

_G.OnObjectLoad(OnObjectLoad)
_G.OnObjectLoad(OnCreateObj)
_G.OnCreateObj(OnCreateObj)
_G.OnDeleteObj(OnDeleteObj)

_G.OnProcessSpellAttack(OnProcessSpellAttack)

_G.OpenPredict_Loaded = true
PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> Loaded!</font>")
