# Changelog

## [0.05a] - 2016-01-25
### Added
- Fog-of-war prediction (for brief time).
- Added member predictInfo.timeToHit.
- Added global _G.OpenPredict_Version.
- GetHealthPrediction now return predicted health and aggro count.
### Fixed
- Health Prediction (a lot of bugs with previous version due to lack of testing).
- Hit chance for conic spells (dynamic width based on distance & angle).
### Removed
- Auto updater.
- _G.OpenPredict_Loaded (replaced with _G.OpenPredict_Version).

## [0.04a] - 2016-01-14
### Added
- Health prediction (GetHealthPrediction).

### Removed
- Extra load message.

## [0.03a] - 2016-01-07
### Fixed
- Minion collision (minion iteration issue).

## [0.02a] - 2015-12-30
### Added
- Accelerated projectile prediction (spellData: .accel, .minSpeed, .maxSpeed).

### Changed
- Reworked AOE functions, should be far less prone to errors now.
- Changed predictInfo:mCollision and predictInfo:hCollision parameter to integer.
- Improvements to minion manager.

## [0.01a] - 2015-12-28
### Initial
- Initial alpha release.
