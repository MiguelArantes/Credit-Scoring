#!/usr/bin/env Rscript
Sys.setenv(R_COMPILE_PKGS = TRUE)
Sys.setenv(R_ENABLE_JIT = 3)

packages <- list(
  var_analysis = c('data.table'),
  utils = c('data.table'),
  models = c('data.table', 'classInt', 'NbClust'),
  tests = c('testthat')
)

for (package in unique(unlist(packages))) {
  installed <-
    suppressMessages(suppressWarnings(require(package, character.only = TRUE)))
  
  tryCatch({
    if (!installed) {
      cat("Installing", package, "\n")
      install.packages(package, repos = "http://cran.rstudio.com")
    } else {
      cat("Checking for new version of", package, "\n")
      update.packages(package, repos = "http://cran.rstudio.com")
    }
  }, warning = function(w)
    stop(w))
}
