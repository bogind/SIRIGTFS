## Test environments
* local windows 7 enterprise, R 3.4.1
* local windows 10 home, R 3.4.1
* local ubuntu 16.04.3, R 3.4.1
* ubuntu 16.04.3 (on VirtualBox), R 3.2.3
* Mac OS X 10.11 (on VirtualBox), R 3.4.1
* Mac OS X 10.11 (on travis-ci), R 3.4.1
* ubuntu 14.04 (on travis-ci), R 3.4.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking installed package size ... NOTE
  installed size is 29.5Mb
  sub-directories of 1Mb or more:
    data  29.1Mb

---

I know the dataset "sirisample" is very large,
but it is preferable to provide a full day of data for any and all researchers wishing to test the library,
if possible i would like to keep it the way it is.

problem with devtools::release() first check:

keeps returning this error:
"ERROR: Error in 'git2r_remote_fetch': error authenticating: failed connecting agent"

checked the connection and authorization with git manually, and it's working fine.

utils::choose.dir() is the only namespace call within the code.
that is to only call it on windows systems where it is meaningful.

*NOTE: I realise it has been a relatively short time since my last rlease for CRAN but this will be the last time for the following months, probably until the end of 2017.
