--[[
  OpenPredict 0.04a

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

  Extra methods:
    GetHealthPrediction(unit, timeDelta)
]]

-- Simple prerequisite
if _G.OpenPredict_Loaded then return end

local SCRIPT_VERSION = 0.04
local DEV_STAGE = "a"

do -- Auto Update
  function ResourceRequest(host, requestURI, timeout)
    -- Require the LuaSocket module
    LuaSocket = LuaSocket or require("socket")
    client = client or LuaSocket.tcp() -- Create the client socket

    client:connect(host, 80) -- Connect to host on port 80

    -- Send HTTP request
    client:send(requestURI)

    -- Receive the first line
    local line, error = client:receive()
    client:settimeout(timeout)

    if not error and line == "HTTP/1.1 200 OK" then
      -- Read HTTP headers
      local headers = { }

      while line ~= "" do
        line, error = client:receive()
        if error then PrintChat(error) end

        local name, value = LuaSocket.skip(2, string.find(line, "^(.-):%s*(.*)"))

        if name and value then
          headers[name] = value
        end
      end

      if headers["Content-Length"] then
        data, error = client:receive(tonumber(headers["Content-Length"]))
        if error then return PrintChat(error) end
        return data
      elseif headers["Transfer-Encoding"] and headers["Transfer-Encoding"] == "chunked" then
        local buffer = ""
        local chunkSize, data

        chunkSize, error = client:receive()
        if error then return PrintChat(error) end
        chunkSize = tonumber(chunkSize, 16)

        while chunkSize > 0 do
          data, error = client:receive(chunkSize)
          if error then return PrintChat(error) end
          buffer = buffer .. data

          chunkSize, error = client:receive()
          if error then return PrintChat(error) end
          chunkSize = tonumber(chunkSize, 16) or 0
        end

        return buffer
      end
    end
  end

  local latestVersion = ResourceRequest("LeagueofLua.com", "GET /OpenPredict/VERSION HTTP/1.1\r\nHost: LeagueofLua.com\r\n\r\n", 1)

  if latestVersion and tonumber(latestVersion) > SCRIPT_VERSION then
    local scriptData = ResourceRequest("LeagueofLua.com", "GET /OpenPredict/main.lua HTTP/1.1\r\nHost: LeagueofLua.com\r\n\r\n", 5)

    if scriptData and pcall(loadstring(scriptData)) then
      local file = assert(io.open(debug.getinfo(1).source:sub(2), "w"))
      file:write(scriptData)
      file:close()
      return false
    end
  end
end

local myHero = GetMyHero()

-- Constants
local TEAM_ALLY = GetTeam(myHero)
local TEAM_ENEMY = (TEAM_ALLY == 100 and 200 or 100)
local IMMOBILE_BUFFS = { }
local MISSILE_SPEEDS = { }

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

-- Populate the MISSILE_SPEED table
MISSILE_SPEEDS["HA_AP_ChaosTurret"] = 1200.0000
MISSILE_SPEEDS["HA_AP_ChaosTurret2"] = 1200.0000
MISSILE_SPEEDS["HA_AP_ChaosTurret3"] = 1200.0000
MISSILE_SPEEDS["HA_AP_ChaosTurretShrine"] = 500.0000

MISSILE_SPEEDS["HA_AP_OrderTurret"] = 1200.0000
MISSILE_SPEEDS["HA_AP_OrderTurret2"] = 1200.0000
MISSILE_SPEEDS["HA_AP_OrderTurret3"] = 1200.0000
MISSILE_SPEEDS["HA_AP_OrderTurretShrine"] = 500.0000

MISSILE_SPEEDS["SRU_ChaosMinionMelee"] = math.huge
MISSILE_SPEEDS["SRU_ChaosMinionRanged"] = 650.0000
MISSILE_SPEEDS["SRU_ChaosMinionSiege"] = 1200.0000
MISSILE_SPEEDS["SRU_ChaosMinionSuper"] = math.huge

MISSILE_SPEEDS["SRU_OrderMinionMelee"] = math.huge
MISSILE_SPEEDS["SRU_OrderMinionRanged"] = 650.0000
MISSILE_SPEEDS["SRU_OrderMinionSiege"] = 1200.0000
MISSILE_SPEEDS["SRU_OrderMinionSuper"] = math.huge

MISSILE_SPEEDS["SRUAP_Turret_Chaos1"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Chaos2"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Chaos3"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Chaos4"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Chaos5"] = 500.0000

MISSILE_SPEEDS["SRUAP_Turret_Order1"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Order2"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Order3"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Order4"] = 1200.0000
MISSILE_SPEEDS["SRUAP_Turret_Order5"] = 500.0000

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
    if sq_range == huge or (p.x - source.x) ^ 2 + (p.z - source.z) ^ 2 - GetHitBox(minion) ^ 2 < sq_range then
      local t = CollisionTime(source, self.castPos, minion, self.meta.delay, self.meta.speed, self.meta.width)

      if t and t > 0 and GetHealthPrediction(minion, self.meta.delay + t) > 0 then
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

function GetHealthPrediction(unit, timeDelta)
  local networkID, totalDamage = GetNetworkID(unit), 0

  for nID, attackProc in pairs(activeAttacks) do
    if attackProc.targetNetworkID == networkID and IsObjectAlive(attackProc.source) and not IsMoving(attackProc.source) then
      -- Distance can alter during missile flight
      local sP, eP = GetOrigin(attackProc.source), GetOrigin(unit)
      local distance = math.sqrt((sP.x - eP.x) ^ 2 + (sP.z - eP.z) ^ 2)

      -- Calculate the time of minion damage
      local timeToHit = attackProc.startTime + attackProc.windUpTime
      if attackProc.missileSpeed < math.huge then
        timeToHit = timeToHit + distance / attackProc.missileSpeed
      end

      if GetGameTimer() < attackProc.startTime + timeToHit then
        if GetGameTimer() + timeDelta > attackProc.startTime + attackProc.animationTime then
          -- Calculate the timeDelta remaining after animationTime
          local newDelta = timeDelta - max(0, (attackProc.animationTime - (GetGameTimer() - attackProc.startTime)))
          local nAttacks = math.floor(newDelta / attackProc.animationTime) + (newDelta % attackProc.animationTime > timeToHit % attackProc.animationTime and 1 or 0)

          totalDamage = totalDamage + GetBaseDamage(attackProc.source) * nAttacks
        elseif GetGameTimer() + timeDelta > timeToHit then
          totalDamage = totalDamage + GetBaseDamage(attackProc.source)
        end
      end

      -- Prevent overcalculating
      if totalDamage > GetCurrentHP(unit) then break end
    end
  end

  return GetCurrentHP(unit) - totalDamage
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

local function OnProcessSpell(unit, spellProc)
  if spellProc.target and spellProc.name:find("BasicAttack") and MISSILE_SPEEDS[GetObjectName(unit)] then
    activeAttacks[GetNetworkID(unit)] = {
      startTime = GetGameTimer(),
      windUpTime = spellProc.windUpTime,
      missileSpeed = MISSILE_SPEEDS[GetObjectName(unit)],
      animationTime = spellProc.animationTime,
      source = unit,
      targetNetworkID = GetNetworkID(spellProc.target)
    }
  end
end

local function OnProcessSpellAttack(unit, attackProc)
  if GetObjectType(unit) == Obj_AI_Hero then
    activeAttacks[GetNetworkID(unit)] = {
      startTime = GetGameTimer(),
      windUpTime = attackProc.windUpTime,
      missileSpeed = MISSILE_SPEEDS[GetObjectName(unit)] or math.huge,
      animationTime = attackProc.animationTime,
      source = unit,
      targetNetworkID = GetNetworkID(attackProc.target)
    }
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

_G.OnProcessSpell(OnProcessSpell)
_G.OnProcessSpellAttack(OnProcessSpellAttack)

_G.OnTick(ObjectHandler)

_G.OpenPredict_Loaded = true
PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> " .. SCRIPT_VERSION .. DEV_STAGE .. " loaded!</font>")
