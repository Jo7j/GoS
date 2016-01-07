--[[
  OpenPredict 0.03a

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
local SCRIPT_VERSION = 0.03
local DEV_STAGE = "a"
local TEAM_ENEMY = GetTeam(myHero) == 100 and 200 or 100
local IMMOBILE_BUFFS = { }

-- Script globals
local activeAttacks = { }
local activeDashes = { }
local activeImmobility = { }
local activeWaypoints = { }

--
local minionUnits = { }
local allyHeroes, enemyHeroes = { }, { }

--
local abs, acos, deg, epsilon, huge, max, min, pi, sin, sqrt = math.abs, math.acos, math.deg, 1e-9, math.huge, math.max, math.min, math.pi, math.sin, math.sqrt
local insert, remove = table.insert, table.remove

do -- Populate the IMMOBILE_BUFFS table
  local bL = GetBuffTypeList()
  IMMOBILE_BUFFS = { [bL.Stun] = true, [bL.Taunt] = true, [bL.Snare] = true, [bL.Fear] = true, [bL.Charm] = true, [bL.Suppression] = true, [bL.Flee] = true, [bL.Knockup] = true, [bL.Knockback] = true }
end

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
  local result, threshold = { }, huge
  local source, sq_range = self.meta.source, self.meta.range and self.meta.range * self.meta.range or huge

  for i = 1, #enemyHeroes do
    local p = GetOrigin(enemyHeroes[i])
    if sq_range == huge or (p.x - source.x) ^ 2 + (p.z - source.z) ^ 2 < sq_range then
      local t = CollisionTime(source, self.castPos, enemyHeroes[i], self.meta.delay, self.meta.speed, self.meta.width)

      if t and t > 0 then
        if n and #result + 1 > n then return true end
        insert(result, t < threshold and 1 or #result, enemyHeroes[i])
      end
    end
  end

  return #result > 0 and result
end

function predictInfo:mCollision(n)
  local result, threshold = { }, huge
  local source, sq_range = self.meta.source, self.meta.range and self.meta.range * self.meta.range or huge

  for minion in GetMinions(TEAM_ENEMY) do
    local p = GetOrigin(minion)
    if sq_range == huge or (p.x - source.x) ^ 2 + (p.z - source.z) ^ 2 < sq_range then
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
  local speed = spellData.speed or huge
  local width = spellData.width or spellData.radius and 2 * spellData.radius or 1
  local range = spellData.range or huge
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
      if speed ~= huge then
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
          if spellData.minSpeed then v = max(spellData.minSpeed, v) end
          if spellData.maxSpeed then v = min(spellData.maxSpeed, v) end
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
          pI.hitChance = min(1, ((1.5 * width) / velocity) / t)

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
        pI.hitChance = min(1, (width / GetMoveSpeed(unit)) / (t - t1))
      end
    elseif isAttacking then
      pI.hitChance = min(1, (width / GetMoveSpeed(unit)) / (t - t2))
    else
      pI.hitChance = min(1, (width / GetMoveSpeed(unit)) / t)
    end
  end

  if range ~= huge then
    pI.hitChance = sqrt((pI.x - source.x) ^ 2 + (pI.z - source.z) ^ 2) < range and pI.hitChance or -1
  end

  pI.castPos = { x = pI.x, y = pI.y, z = pI.z }

  return pI
end

function GetLinearAOEPrediction(unit, spellData, sourcePos)
  local pI = GetPrediction(unit, spellData, sourcePos)

  if spellData.width and spellData.width > 1 and spellData.range and spellData.range < huge then
    local aoeCastPos, threshold = pI.castPos, (2 * spellData.width) ^ 2
    local p1, p2 = pI.meta.source, { x = pI.x, y = pI.y, z = pI.z }

    do -- Extend vector to match range
      local dx, dy = p2.x - p1.x, p2.z - p1.z
      local magnitude = math.sqrt(dx * dy + dy * dy)
      p2.x = p2.x + (dx / magnitude) * spellData.range
      p2.z = p2.z + (dy / magnitude) * spellData.range
    end

    for i = 1, #enemyHeroes do
      local enemy = enemyHeroes[i]

      if enemy ~= unit and IsVisible(enemy) and IsObjectAlive(enemy) and IsTargetable(enemy) and not IsImmune(enemy, myHero) then
        local p = GetPrediction(enemy, spellData, sourcePos).castPos
        local c = (p.x - p1.x) * (p2.x - p1.x) + (p.z - p1.z) * (p2.z - p1.z)

        if sqrt((p.x - p1.x) ^ 2 + (p.z - p1.z) ^ 2) < spellData.range then
          local t = c / (spellData.range * spellData.range)

          if t > 0 and t < 1 then
            local projection = { x = p1.x + t * (p2.x - p1.x), y = p1.z + t * (p2.z - p1.z) }
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
  end

  return pI
end

function GetCircularAOEPrediction(unit, spellData, sourcePos)
  local pI = GetPrediction(unit, spellData, sourcePos)

  if (spellData.radius and spellData.radius > 1) or (spellData.width and spellData.width > 1) then
    local width = spellData.width or 1 * spellData.radius
    local aoeCastPos, threshold = pI.castPos, (2 * width) ^ 2

    for i = 1, #enemyHeroes do
      local enemy = enemyHeroes[i]

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

  if spellData.angle and spellData.angle > 1 and spellData.range and spellData.range < huge then
    local aoeCastPos, threshold = pI.castPos, 2 * spellData.angle
    local p1, p2 = pI.meta.source, { x = pI.x, y = pI.y, z = pI.z }
    local dx, dy = p2.x - p1.x, p2.z - p1.z

    do -- Extend vector to match range
      local magnitude = math.sqrt(dx * dy + dy * dy)
      p2.x = p2.x + (dx / magnitude) * spellData.range
      p2.z = p2.z + (dy / magnitude) * spellData.range
    end

    for i = 1, #enemyHeroes do
      local enemy = enemyHeroes[i]

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

local queueObjects = { }
local function OnCreateObj(object)
  if GetObjectType(object) == Obj_AI_Minion then
    insert(queueObjects, object)
  end
end

local function OnDeleteObj(object)
  local networkID = GetNetworkID(object)
  if networkID and networkID > 0 and networkID ~= huge then
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

local function ObjectHandler()
  for i = #queueObjects, 1, -1 do
    local nID = GetNetworkID(queueObjects[i])

    if nID and nID > 0 and nID < huge then
      local team = GetTeam(queueObjects[i])

      if team and team > 0 and team % 100 == 0 then
        insert(minionUnits, queueObjects[i])
      end

      remove(queueObjects, i)
    end
  end
end

--[[
  UTILITY

  GetMinions - Minion iterator.
  GetWaypoints - Waypoint iterator.
  IsMoving - Returns true if unit is moving towards a waypoint.
  IsAttacking - Returns true if unit is attacking and their remaining wind-up time.
  IsImmobile - Returns true if unit is immobile and the remaining immobility time.
  CollisionTime - Calculates the time of trajectory collision.
]]

GetMinions = function(team)
  local i, n = 0, #minionUnits

  return function()
    ::Retry::
      i = i + 1

    if i <= n then
      if minionUnits[i] and IsVisible(minionUnits[i]) and IsObjectAlive(minionUnits[i]) and (not team or (GetTeam(minionUnits[i]) == team or GetTeam(minionUnits[i]) == 300)) then
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

_G.OnTick(ObjectHandler)

_G.OpenPredict_Loaded = true
PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> Loaded!</font>")

-- Auto Update
do
  local b='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

  function Base64Decode(data)
    data = string.gsub(data, '[^'..b..'=]', '')
    return (data:gsub('.', function(x)
    if (x == '=') then return '' end
    local r,f='',(b:find(x)-1)
    for i=6,1,-1 do r=r..(f%2^i-f%2^(i-1)>0 and '1' or '0') end
    return r;
    end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
    if (#x ~= 8) then return '' end
    local c=0
    for i=1,8 do c=c+(x:sub(i,i)=='1' and 2^(8-i) or 0) end
    return string.char(c)
    end))
  end

  function math.round(num, idp)
    assert(type(num) == "number", "math.round: wrong argument types (<number> expected for num)")
    assert(type(idp) == "number" or idp == nil, "math.round: wrong argument types (<integer> expected for idp)")
    local mult = 10 ^ (idp or 0)
    if num >= 0 then return math.floor(num * mult + 0.5) / mult
    else return math.ceil(num * mult - 0.5) / mult
    end
  end

  function string.split(str, delim, maxNb)
    -- Eliminate bad cases...
    if not delim or delim == "" or string.find(str, delim) == nil then
      return { str }
    end
    maxNb = (maxNb and maxNb >= 1) and maxNb or 0
    local result = {}
    local pat = "(.-)" .. delim .. "()"
    local nb = 0
    local lastPos
    for part, pos in string.gmatch(str, pat) do
      nb = nb + 1
      if nb == maxNb then
        result[nb] = lastPos and string.sub(str, lastPos, #str) or str
        break
      end
      result[nb] = part
      lastPos = pos
    end
    -- Handle the last field
    if nb ~= maxNb then
      result[nb + 1] = string.sub(str, lastPos)
    end
    return result
  end

  class "ScriptUpdate"
  function ScriptUpdate:__init(LocalVersion,UseHttps, Host, VersionPath, ScriptPath, SavePath, CallbackUpdate, CallbackNoUpdate, CallbackNewVersion,CallbackError)
    self.LocalVersion = LocalVersion
    self.Host = Host
    self.VersionPath = '/GOS/TCPUpdater/GetScript'..(UseHttps and '5' or '6')..'.php?script='..self:Base64Encode(self.Host..VersionPath)..'&rand='..math.random(99999999)
    self.ScriptPath = '/GOS/TCPUpdater/GetScript'..(UseHttps and '5' or '6')..'.php?script='..self:Base64Encode(self.Host..ScriptPath)..'&rand='..math.random(99999999)
    self.SavePath = SavePath
    self.CallbackUpdate = CallbackUpdate
    self.CallbackNoUpdate = CallbackNoUpdate
    self.CallbackNewVersion = CallbackNewVersion
    self.CallbackError = CallbackError
    OnDraw(function() self:OnDraw() end)
    self:CreateSocket(self.VersionPath)
    self.DownloadStatus = 'Connecting to Server for VersionInfo'
    OnTick(function() self:GetOnlineVersion() end)
  end

  function ScriptUpdate:print(str)
    print('<font color="#FFFFFF">'..os.clock()..': '..str)
  end

  function ScriptUpdate:OnDraw()
    if self.DownloadStatus ~= 'Downloading Script (100%)' and self.DownloadStatus ~= 'Downloading VersionInfo (100%)'then
      DrawText('Download Status: '..(self.DownloadStatus or 'Unknown'),50,10,50,ARGB(0xFF,0xFF,0xFF,0xFF))
    end
  end

  function ScriptUpdate:CreateSocket(url)
    if not self.LuaSocket then
      self.LuaSocket = require("socket")
    else
      self.Socket:close()
      self.Socket = nil
      self.Size = nil
      self.RecvStarted = false
    end
    self.LuaSocket = require("socket")
    self.Socket = self.LuaSocket.tcp()
    self.Socket:settimeout(0, 'b')
    self.Socket:settimeout(99999999, 't')
    self.Socket:connect('plebleaks.com', 80)
    self.Url = url
    self.Started = false
    self.LastPrint = ""
    self.File = ""
  end

  function ScriptUpdate:Base64Encode(data)
    local b='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    return ((data:gsub('.', function(x)
    local r,b='',x:byte()
    for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
    return r;
    end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
    if (#x < 6) then return '' end
    local c=0
    for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
    return b:sub(c+1,c+1)
    end)..({ '', '==', '=' })[#data%3+1])
  end

  function ScriptUpdate:GetOnlineVersion()
    if self.GotScriptVersion then return end

    self.Receive, self.Status, self.Snipped = self.Socket:receive(1024)
    if self.Status == 'timeout' and not self.Started then
      self.Started = true
      self.Socket:send("GET "..self.Url.." HTTP/1.1\r\nHost: plebleaks.com\r\n\r\n")
    end
    if (self.Receive or (#self.Snipped > 0)) and not self.RecvStarted then
      self.RecvStarted = true
      self.DownloadStatus = 'Downloading VersionInfo (0%)'
    end

    self.File = self.File .. (self.Receive or self.Snipped)
    if self.File:find('</s'..'ize>') then
      if not self.Size then
        self.Size = tonumber(self.File:sub(self.File:find('<si'..'ze>')+6,self.File:find('</si'..'ze>')-1))
      end
      if self.File:find('<scr'..'ipt>') then
        local _,ScriptFind = self.File:find('<scr'..'ipt>')
        local ScriptEnd = self.File:find('</scr'..'ipt>')
        if ScriptEnd then ScriptEnd = ScriptEnd - 1 end
        local DownloadedSize = self.File:sub(ScriptFind+1,ScriptEnd or -1):len()
        self.DownloadStatus = 'Downloading VersionInfo ('..math.round(100/self.Size*DownloadedSize,2)..'%)'
      end
    end
    if self.File:find('</scr'..'ipt>') then
      self.DownloadStatus = 'Downloading VersionInfo (100%)'
      local a,b = self.File:find('\r\n\r\n')
      self.File = self.File:sub(a,-1)
      self.NewFile = ''
      for line,content in ipairs(self.File:split('\n')) do
        if content:len() > 5 then
          self.NewFile = self.NewFile .. content
        end
      end
      local HeaderEnd, ContentStart = self.File:find('<scr'..'ipt>')
      local ContentEnd, _ = self.File:find('</sc'..'ript>')
      if not ContentStart or not ContentEnd then
        if self.CallbackError and type(self.CallbackError) == 'function' then
          self.CallbackError()
        end
      else
        self.OnlineVersion = (Base64Decode(self.File:sub(ContentStart + 1,ContentEnd-1)))
        self.OnlineVersion = tonumber(self.OnlineVersion)
        if self.OnlineVersion > self.LocalVersion then
          if self.CallbackNewVersion and type(self.CallbackNewVersion) == 'function' then
            self.CallbackNewVersion(self.OnlineVersion,self.LocalVersion)
          end
          self:CreateSocket(self.ScriptPath)
          self.DownloadStatus = 'Connect to Server for ScriptDownload'
          OnTick(function() self:DownloadUpdate() end)
        else
          if self.CallbackNoUpdate and type(self.CallbackNoUpdate) == 'function' then
            self.CallbackNoUpdate(self.LocalVersion)
          end
        end
      end
      self.GotScriptVersion = true
    end
  end

  function ScriptUpdate:DownloadUpdate()
    if self.GotScriptUpdate then return end
    self.Receive, self.Status, self.Snipped = self.Socket:receive(1024)
    if self.Status == 'timeout' and not self.Started then
      self.Started = true
      self.Socket:send("GET "..self.Url.." HTTP/1.1\r\nHost: plebleaks.com\r\n\r\n")
    end
    if (self.Receive or (#self.Snipped > 0)) and not self.RecvStarted then
      self.RecvStarted = true
      self.DownloadStatus = 'Downloading Script (0%)'
    end

    self.File = self.File .. (self.Receive or self.Snipped)
    if self.File:find('</si'..'ze>') then
      if not self.Size then
        self.Size = tonumber(self.File:sub(self.File:find('<si'..'ze>')+6,self.File:find('</si'..'ze>')-1))
      end
      if self.File:find('<scr'..'ipt>') then
        local _,ScriptFind = self.File:find('<scr'..'ipt>')
        local ScriptEnd = self.File:find('</scr'..'ipt>')
        if ScriptEnd then ScriptEnd = ScriptEnd - 1 end
        local DownloadedSize = self.File:sub(ScriptFind+1,ScriptEnd or -1):len()
        self.DownloadStatus = 'Downloading Script ('..math.round(100/self.Size*DownloadedSize,2)..'%)'
      end
    end
    if self.File:find('</scr'..'ipt>') then
      self.DownloadStatus = 'Downloading Script (100%)'
      local a,b = self.File:find('\r\n\r\n')
      self.File = self.File:sub(a,-1)
      self.NewFile = ''
      for line,content in ipairs(self.File:split('\n')) do
        if content:len() > 5 then
          self.NewFile = self.NewFile .. content
        end
      end
      local HeaderEnd, ContentStart = self.NewFile:find('<sc'..'ript>')
      local ContentEnd, _ = self.NewFile:find('</scr'..'ipt>')
      if not ContentStart or not ContentEnd then
        if self.CallbackError and type(self.CallbackError) == 'function' then
          self.CallbackError()
        end
      else
        local newf = self.NewFile:sub(ContentStart+1,ContentEnd-1)
        local newf = newf:gsub('\r','')
        if newf:len() ~= self.Size then
          if self.CallbackError and type(self.CallbackError) == 'function' then
            self.CallbackError()
          end
          return
        end
        local newf = Base64Decode(newf)
        local f = io.open(self.SavePath,"w+b")
        f:write(newf)
        f:close()
        if self.CallbackUpdate and type(self.CallbackUpdate) == 'function' then
          self.CallbackUpdate(self.OnlineVersion,self.LocalVersion)
        end
      end
      self.GotScriptUpdate = true
    end
  end

  local ToUpdate = { }
  ToUpdate.Version = SCRIPT_VERSION
  ToUpdate.UseHttps = true
  ToUpdate.Host = "raw.githubusercontent.com"
  ToUpdate.VersionPath = "/Jo7j/GoS/master/OpenPredict/OpenPredict.version"
  ToUpdate.ScriptPath =  "/Icesythe7/GOS/master/OpenPredict.lua"
  ToUpdate.SavePath = COMMON_PATH.."/OpenPredict.lua"
  ToUpdate.CallbackUpdate = function(NewVersion,OldVersion) PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> Updated to (" ..NewVersion.. ") Please Reload with 2x F6.</font>") end
  ToUpdate.CallbackNoUpdate = function(OldVersion) PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> No Updates Found! Version " .. ToUpdate.Version .. DEV_STAGE .. " Loaded!</font>") end
  ToUpdate.CallbackNewVersion = function(NewVersion) PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> New Version found (" ..NewVersion.. "). Please wait until its downloaded</font>") end
  ToUpdate.CallbackError = function(NewVersion) PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> Error while Downloading. Please try again.</font>") end

  ScriptUpdate(ToUpdate.Version,ToUpdate.UseHttps, ToUpdate.Host, ToUpdate.VersionPath, ToUpdate.ScriptPath, ToUpdate.SavePath, ToUpdate.CallbackUpdate,ToUpdate.CallbackNoUpdate, ToUpdate.CallbackNewVersion,ToUpdate.CallbackError)
end
