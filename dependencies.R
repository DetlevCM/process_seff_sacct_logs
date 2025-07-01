
## To more efficiently process the initial data, the script has been parallelised.
## These are not strictly necessary to run the scripts, but do speed up the processing
## of files, which is especially helpful if lots of data is available.

## You may need to run this in an interactive session to confirm the installation, 
## select the mirror and select the approriate library path.



## the R library archive relies on archive.h from
##     zypper install libarchive-devel
## on OpenSUSE Tumbleweed
## to read files in archives without unpacking
install.packages("archive")

## to run in parallel
install.packages("foreach")
install.packages("doParallel")
