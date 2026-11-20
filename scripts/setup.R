# Run from the repository root with Rscript --vanilla scripts/setup.R.
stopifnot(file.exists("renv.lock"), file.exists("DESCRIPTION"))
project <- normalizePath(".", winslash = "/")
project_library <- file.path(project, ".local-library")
dir.create(project_library, showWarnings = FALSE)
.libPaths(c(project_library, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Keep renv's downloaded packages and installation metadata inside the project.
Sys.setenv(RENV_PATHS_ROOT = file.path(project, "renv", "local"),
           RENV_CONFIG_CACHE_ENABLED = "FALSE")
if (!dir.exists(file.path(project_library, "renv"))) {
  previous <- find.package("renv", quiet = TRUE)
  if (length(previous)) {
    stopifnot(file.copy(previous, project_library, recursive = TRUE))
  } else {
    install.packages("renv", lib = project_library)
  }
}
lock <- renv::lockfile_read("renv.lock")
if (as.character(getRversion()) != lock$R$Version)
  message("The tested R version is ", lock$R$Version, ". This R is ", getRversion(), ".")

# A workstation with all locked builds already installed can set up offline.
matches_lock <- function(record, libraries) {
  location <- find.package(record$Package, lib.loc = libraries, quiet = TRUE)
  if (!length(location)) return(FALSE)
  version <- read.dcf(file.path(location, "DESCRIPTION"), fields = "Version")[[1L]]
  identical(version, record$Version)
}
present <- vapply(lock$Packages, matches_lock, logical(1), libraries = project_library)
if (!all(present) && all(vapply(lock$Packages, matches_lock, logical(1), libraries = .libPaths()))) {
  renv::hydrate(project = project, library = project_library, packages = names(lock$Packages),
                sources = .libPaths(), prompt = FALSE)
}
present <- vapply(lock$Packages, matches_lock, logical(1), libraries = project_library)
if (!all(present)) {
  renv::restore(project = project, library = project_library,
                packages = names(present)[!present], prompt = FALSE)
}
verified <- vapply(lock$Packages, matches_lock, logical(1), libraries = project_library)
if (!all(verified)) stop("Locked versions unavailable: ", paste(names(verified)[!verified], collapse = ", "))
message("Verified ", length(verified), " locked packages in ", project_library, ".")
