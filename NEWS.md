---
title: "NEWS"
output: html_document
---

# SIRItoGTFS 0.2.5 17/08/2018

minor bug fix in an internal function

# SIRItoGTFS 0.2.4 21/05/2018

several bug fixes, now works on all platforms and adapted to changes in data.table,
curently not tested for mac osx on travis due to problems on travis with preinstalling geos and gdal
does work however on tested mac osx on vm.


# SIRItoGTFS 0.2.3 25/02/2018

rebuild of internal finction,
SIRI input can now be a table built from SIRI SM calls which only call through the last stops in each line.


# SIRItoGTFS 0.2.2 05/02/2018

fixed SIRIsample dataset to include data that is actually in the subset of the GTFS tables

