source("scripts/load.R")
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
