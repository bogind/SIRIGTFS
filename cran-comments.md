## Test environments
* local windows 7 enterprise, R 3.4.3
* local windows 10 home, R 3.4.3
* local Ubuntu 16.04.3, R 3.4.3
* Ubuntu 18.04 LTS (on VirtualBox), 
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* win-builder (devel and release)
* Mac OS X 10.11 (on VirtualBox), R 3.4.3



## R CMD check results

0 errors | 0 warnings | 0 notes


---
 
problem with devtools::release() first check,
ERROR: Error in 'git2r_remote_fetch': error authenticating: failed connecting agent
happend with my previous package *easycsv* on the same computer too,    
all connections are fine and it only happens with the first time release checks.    

