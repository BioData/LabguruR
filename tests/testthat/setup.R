# Setup file - runs before all tests
# Load all package functions

# Find package root directory
find_pkg_root <- function() {
  # Try different relative paths
  paths <- c(
    "../..",           # from tests/testthat
    "..",              # from tests
    "."                # current dir
  )
  for (p in paths) {
    r_dir <- file.path(p, "R")
    if (dir.exists(r_dir) && length(list.files(r_dir, pattern = "\\.R$")) > 0) {
      return(normalizePath(p))
    }
  }
  stop("Could not find package root directory")
}

pkg_dir <- find_pkg_root()
r_files <- list.files(file.path(pkg_dir, "R"), pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f)
}
