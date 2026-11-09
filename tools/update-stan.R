# Run from the package root: Rscript --vanilla tools/update-stan.R [--write|--check]
# Generation is isolated so rstantools cannot replace our loader or Makevars.
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[1] else "--write"
if (!mode %in% c("--write", "--check") || length(args) > 2L)
  stop("Usage: Rscript --vanilla tools/update-stan.R [--write|--check] [package-dir]",
       call. = FALSE)
root <- normalizePath(if (length(args) == 2L) args[2] else ".", mustWork = TRUE)

updateStan <- function() {
  stage <- tempfile("stan-generation-")
  dir.create(stage)
  originalDirectory <- getwd()
  on.exit({
    setwd(originalDirectory)
    unlink(stage, recursive = TRUE)
  }, add = TRUE)
  setwd(root)

  models <- sub("\\.stan$", "", list.files("inst/stan", pattern = "\\.stan$"))
  if (!identical(sort(models), c("complex", "simple")))
    stop("Expected both inst/stan/simple.stan and inst/stan/complex.stan, and no other models.",
         call. = FALSE)

  inputs <- c("DESCRIPTION", "NAMESPACE",
              list.files("inst/stan", recursive = TRUE, full.names = TRUE),
              list.files("inst/include", recursive = TRUE, full.names = TRUE),
              list.files("src", pattern = "\\.(c|cc|cpp|h|hpp)$",
                         recursive = TRUE, full.names = TRUE))
  for (path in inputs) {
    target <- file.path(stage, path)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(path, target)) stop("Cannot stage ", path, call. = FALSE)
  }
  dir.create(file.path(stage, "R"), showWarnings = FALSE)
  dir.create(file.path(stage, "src"), showWarnings = FALSE)

  versions <- vapply(c("rstan", "StanHeaders", "rstantools", "Rcpp"),
                     function(pkg) as.character(utils::packageVersion(pkg)), character(1))
  message("Stan generator: ", paste(names(versions), versions, collapse = "; "))
  setwd(stage)
  rstantools::rstan_config(stage)
  setwd(root)

  generated <- c(paste0("src/stanExports_", models, ".h"),
                 paste0("src/stanExports_", models, ".cc"),
                 "src/RcppExports.cpp", "R/RcppExports.R")
  required <- generated[generated != "R/RcppExports.R"]
  if (!all(file.exists(file.path(stage, required))))
    stop("The generator did not produce all expected native files.", call. = FALSE)
  # The R wrapper file is absent when only Rcpp modules are exported.
  readCode <- function(path) if (file.exists(path)) readLines(path, warn = FALSE) else NULL
  changed <- generated[!vapply(generated, function(path)
    identical(readCode(file.path(stage, path)), readCode(path)), logical(1))]
  if (!length(changed)) {
    message("Generated Stan files are current.")
    return(invisible(NULL))
  }
  if (mode == "--check")
    stop("Generated Stan files are stale: ", paste(changed, collapse = ", "),
         ". Run Rscript --vanilla tools/update-stan.R --write with the project toolchain,",
         " then include the regenerated files in the same commit as the model change.",
         call. = FALSE)

  for (path in changed) {
    source <- file.path(stage, path)
    if (file.exists(source)) {
      if (!file.copy(source, path, overwrite = TRUE))
        stop("Cannot update ", path, call. = FALSE)
    } else if (unlink(path) != 0L) {
      stop("Cannot remove obsolete generated file ", path, call. = FALSE)
    }
  }
  message("Updated: ", paste(changed, collapse = ", "))
}

updateStan()
