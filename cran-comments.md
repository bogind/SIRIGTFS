## Test environments
* local windows 7 enterprise, R 3.4.2
* local windows 10 home, R 3.4.3
* local Ubuntu 16.04.3, R 3.4.3
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* win-builder (devel and release)
* Mac OS X 10.11 (on VirtualBox), R 3.4.3


## R CMD check results

0 errors | 0 warnings | 1 notes

* checking installed package size ... NOTE
  installed size is 29.5Mb
  sub-directories of 1Mb or more:
    data  29.1Mb

---

I realise the dataset "sirisample" is large,
but it is preferable to provide a full day of data for any and all researchers wishing to test the library,
if possible i would like to keep it the way it is.

problem with devtools::build() in RStudio in windows 10,
R CMD build and R CMD check (plus --as-cran) come back pefect from terminal.

