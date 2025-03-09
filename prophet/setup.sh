#!/bin/bash

set -ex

# sudo apt-get update
# sudo apt-get install libsodium-dev

R -e "install.packages('plumber', repos='http://cran.us.r-project.org')"
R -e "install.packages('MASS', repos='http://cran.us.r-project.org')"
R -e "install.packages('caret', repos='http://cran.us.r-project.org')"
R -e "install.packages('mgcv', repos='http://cran.us.r-project.org')"
R -e "install.packages('arm', repos='http://cran.us.r-project.org')"
R -e "install.packages('jsonlite', repos='http://cran.us.r-project.org')"
R -e "install.packages('tictoc', repos='http://cran.us.r-project.org')"
R -e "install.packages('plyr', repos='http://cran.us.r-project.org')"
R -e "install.packages('gbm', repos='http://cran.us.r-project.org')"
R -e "install.packages('testthat', repos='http://cran.us.r-project.org')"
R -e "install.packages('here', repos='http://cran.us.r-project.org')"