library(testthat);
source(here::here('core', 'prophet.R'))

for (test_dir in list.dirs(here::here('tests'), recursive = TRUE, full.names = TRUE)) {
  test_files <- list.files(test_dir, pattern = '\\.test\\.R$', full.names = TRUE)
  lapply(test_files, testthat::test_file)
}

