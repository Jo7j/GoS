OpenPredict 0.04a
===========

#####Core Functions:
```lua
GetPrediction(unit, spellData, sourcePos)
GetLinearAOEPrediction(unit, spellData, sourcePos)
GetCircularAOEPrediction(unit, spellData, sourcePos)
GetConicAOEPrediction(unit, spellData, sourcePos)
  
All core function parameters are as follows:
	unit        - Object (required)
	spellData   - Table (required)
	sourcePos   - Vec3 (optional)
  
All core functions return a predictInfo object (see below).
```

#####Extra functions:
```lua
GetHealthPrediction(unit, timeDelta)            -- Returns predicted health (as number).
```

#####predictInfo:

```lua
members:
	predictInfo.x
	predictInfo.y
	predictInfo.z
    
	predictInfo.castPos                       -- 3D vector containing above values.
	predictInfo.hitChance                     -- Probability of skillshot hitting target (0.0f - 1.0f)
	predictInfo.meta                          -- Internal use only.
  
methods:
	predictInfo:hCollision(nMaxCollision)     -- Traces for hero collision through a linear trajectory and returns table.
	predictInfo:mCollision(nMaxCollision)     -- Traces for minion collision through a linear trajectory and returns table. 
	-- Setting the nMaxCollision parameter to a number will casue the function to only check for n collision.
```

#####spellData:
```lua
spellData
	.delay                                    -- Initial delay before the spell is cast.
	.speed                                    -- Projectile speed (if exists).
	.accel                                    -- Projectile acceleration.
	.minSpeed                                 -- Minimum projectile speed.
	.maxSpeed                                 -- Maximum projectile speed.
	.width                                    -- Full width of the spell (2 × radius).
	.range                                    -- Maximum range of the spell.

	.radius                                   -- Radius of the spell (width ÷ 2).
	.angle                                    -- Angle of the spell (used for GetConicAOEPrediction).
```

#####Simple example of casting Ryze's Overload (Q):

```lua
local Overload = { delay = 0.25, speed = 1700, width = 55, range = 900 }
local pI = GetPrediction(unit, Overload)

if pI and pI.hitChance >= 0.25 and not pI:mCollision(1) then
	CastSkillShot(_Q, pI.castPos)
end
```
