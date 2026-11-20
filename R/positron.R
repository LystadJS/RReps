practice_folder <- function(s) {
  file.path(getOption("rreps.practice", "practice"), sprintf("session-%06d", s$id),
            sprintf("rep-%03d-%s-%d", s$position, s$template_id, s$seed))
}

prepare_files <- function(s, ex) {
  path <- practice_folder(s)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  answer <- file.path(path, "answer.R")
  if (!file.exists(answer)) writeLines(s$draft, answer, useBytes = TRUE)
  if (!file.exists(file.path(path, "inputs.rds"))) saveRDS(ex$inputs, file.path(path, "inputs.rds"))
  context <- file.path(path, "context.R")
  # Only displayed fixture objects are exported. Learner code stays in answer.R.
  if (!file.exists(context)) {
    data_path <- encodeString(file.path(normalizePath(path, winslash = "/"), "inputs.rds"), quote = '"')
    library_path <- encodeString(file.path(getOption("rreps.root"), ".local-library"), quote = '"')
    writeLines(c(paste("#", strwrap(ex$instructions)),
       paste0(".libPaths(c(", library_path, ", .libPaths()))"),
       "library(dplyr)", "library(tidyr)", "library(purrr)", "library(ggplot2)",
       paste0("list2env(readRDS(", data_path, "), envir = .GlobalEnv)")), context)
  }
  answer
}

read_practice_file <- function(s) {
  path <- file.path(practice_folder(s), "answer.R")
  size <- file.info(path)$size
  if (is.na(size)) stop("Open the exercise folder and save answer.R first.")
  if (size > 1024^2) stop("The answer file exceeds the 1 MB editor limit.")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
