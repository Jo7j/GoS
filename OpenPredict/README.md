OpenPredict 0.01a
===========

#####Core Functions:
  ```lua
  GetPrediction(unit, spellData, sourcePos)
  GetLinearAOEPrediction(unit, spellData, sourcePos)
  GetCircularAOEPrediction(unit, spellData, sourcePos)
  GetConicAOEPrediction(unit, spellData, sourcePos)
  ```

#####All core functions return a predictInfo object:

```lua
  members:
    predictInfo.x
    predictInfo.y
    predictInfo.z
    
    predictInfo.castPos                       -- 3D vector containing above values.
    predictInfo.hitChance                     -- Probability of skillshot hitting target (0.0f - 1.0f)
  
  methods:
    predictInfo:hCollision(bOnlyCheck)        -- Traces for minion collision through a linear trajectory and returns table. 
    predictInfo:mCollision(bOnlyCheck)        -- Traces for hero collision through a linear trajectory and returns table. 
    -- Setting the "bOnlyCheck" flag will cause the above functions to return a boolean value.
```

#####Simple example of casting Ryze's Overload (Q):

```lua
local Overload = { delay = 0.25, speed = 1700, width = 55, range = 900 }
local pI = GetPrediction(unit, Overload)

if pI and pI.hitChance >= 0.25 and not pI:mCollision(true) then
	CastSkillShot(_Q, pI.castPos)
end
```
