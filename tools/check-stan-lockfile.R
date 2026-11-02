# Check the selected Stan toolchain's hard-dependency pins before restoring it.
# Usage: Rscript --vanilla tools/check-stan-lockfile.R [renv.lock]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L)
  stop("Usage: Rscript --vanilla tools/check-stan-lockfile.R [renv.lock]", call. = FALSE)
lockfile <- if (length(args)) args[1L] else "renv.lock"
records <- renv::lockfile_read(lockfile)$Packages
systemPackages <- rownames(utils::installed.packages(lib.loc = .Library,
                                             priority = c("base", "recommended")))
pending <- c("rstan", "StanHeaders", "rstantools", "Rcpp", "RcppEigen")
checked <- character()
while (length(pending)) {
  package <- pending[1L]
  record <- records[[package]]
  if (is.null(record))
    stop("The Stan toolchain dependency ", package, " is missing from ", lockfile,
         ". Capture the complete verified dependency closure before restoring.", call. = FALSE)
  if (is.null(record$Version) || !nzchar(record$Version))
    stop("The Stan toolchain dependency ", package, " has no pinned version in ", lockfile,
         ".", call. = FALSE)
  if (!"Requirements" %in% names(record))
    stop("The Stan toolchain dependency ", package, " has no Requirements metadata in ", lockfile,
         ". Capture its hard dependencies before restoring.", call. = FALSE)
  checked <- c(checked, package)
  pending <- setdiff(unique(c(pending[-1L], unlist(record$Requirements, use.names = FALSE))),
                     c("R", systemPackages, checked))
}
cat("Stan toolchain lockfile covers", length(checked), "non-base packages.\n")
