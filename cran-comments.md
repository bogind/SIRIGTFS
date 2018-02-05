## Test environments
* local windows 7 enterprise, R 3.4.2
* local windows 10 home, R 3.4.3
* local Ubuntu 16.04.3, R 3.4.3
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* win-builder (devel and release)
* Mac OS X 10.11 (on VirtualBox), R 3.4.3
* Mac OS X 10.12 (on travis-ci), R 3.4.3


## R CMD check results

0 errors | 0 warnings | 0 notes


---
I know it's only been a week but i got a bug report about the datasets, 
which led to a bug in the example of the main function.
both fixed now with no errors, warnings or notes on all systems.


problem with devtools::build() in RStudio in windows 10,
R CMD build and R CMD check (plus --as-cran) come back pefect from terminal.
another problem while building on windows 10 is a repeated rd warning:
"missing file link 'SpatialPointsDataFrame'"
the link's in the documentation work and it only only shows that warning in windows 10.   
    
problem with devtools::release() first check,
ERROR: Error in 'git2r_remote_fetch': error authenticating: failed connecting agent
happend with my previous package *easycsv* on the same computer too,    
all connections are fine and it only happens with the first time release checks.    
    
The problem that cam up with the invalid URI apperently was adding the "SIRI/GTFS Documentation:" string in front of it.
removed that and the link are just added as useful links.
