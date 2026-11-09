# Stan model loader for Bayesian SSM acceptance sampling.
# Prefer precompiled rstantools modules (stable in JASP engine), with
# runtime stan_model() fallback for source-only development workflows.
# This file is kept separate from a future generated R/stanmodels.R so the
# package can adopt the standard rstantools layout without name conflicts.

.asSSM_createCppModuleFactory <- function(moduleName, className) {
  # Socket workers must initialize their own Rcpp class. Capture only its names.
  force(moduleName)
  force(className)
  function(x) {
    module <- Rcpp::Module(moduleName, PACKAGE = "jaspAcceptanceSampling", mustStart = TRUE)
    eval(substitute(M$X, list(M = module, X = as.name(className))))
  }
}

.asSSM_getStanModel <- local({
  cache <- new.env(parent = emptyenv())
  .getStanFile <- function(modelName) {
    path <- system.file("stan", paste0(modelName, ".stan"), package = "jaspAcceptanceSampling")
    if (!nzchar(path))
      path <- file.path("inst", "stan", paste0(modelName, ".stan"))

    if (!nzchar(path) || !file.exists(path))
      stop("Stan model file not found for model: ", modelName, call. = FALSE)

    path
  }

  .loadPrecompiledModel <- function(modelName, stanFile) {
    expectedSuffix <- paste0("_", modelName, "_mod")
    moduleCandidates <- paste0("stan_fit4", modelName, "_mod")

    reg <- tryCatch(getDLLRegisteredRoutines("jaspAcceptanceSampling"), error = function(e) NULL)
    if (!is.null(reg) && !is.null(reg[[".Call"]])) {
      callNames <- names(reg[[".Call"]])
      bootNames <- grep("^_rcpp_module_boot_.*_mod$", callNames, value = TRUE)
      if (length(bootNames) > 0L) {
        discovered <- sub("^_rcpp_module_boot_", "", bootNames)
        discovered <- discovered[endsWith(discovered, expectedSuffix)]
        if (length(discovered) > 0L)
          moduleCandidates <- unique(c(discovered, moduleCandidates))
      }
    }

    moduleObj <- NULL
    moduleClass <- NULL

    for (moduleName in moduleCandidates) {
      className <- NULL
      moduleObj <- tryCatch(
        Rcpp::Module(moduleName, PACKAGE = "jaspAcceptanceSampling", mustStart = TRUE),
        error = function(e) NULL
      )
      if (is.null(moduleObj))
        next

      modulePrint <- capture.output(print(moduleObj))
      classLine <- grep("^\\s*\\d+\\s+classes\\s*:\\s*$", modulePrint)
      if (length(classLine) > 0L) {
        after <- trimws(modulePrint[(classLine[1] + 1L):length(modulePrint)])
        after <- after[nzchar(after)]
        if (length(after) > 0L)
          className <- after[1L]
      }
      if (is.null(className) || !nzchar(className))
        className <- paste0("rstantools_model_", modelName)

      moduleClass <- tryCatch(
        eval(substitute(M$X, list(M = moduleObj, X = as.name(className)))),
        error = function(e) NULL
      )
      if (!is.null(moduleClass))
        break
    }

    if (is.null(moduleClass))
      stop("Precompiled Stan module class not available for model: ", modelName, call. = FALSE)

    stanfit <- rstan::stanc_builder(
      stanFile,
      allow_undefined = TRUE,
      obfuscate_model_name = FALSE
    )
    stanfit$model_cpp <- list(
      model_cppname = stanfit$model_name,
      model_cppcode = stanfit$cppcode
    )

    methods::new(
      Class = "stanmodel",
      model_name = stanfit$model_name,
      model_code = stanfit$model_code,
      model_cpp = stanfit$model_cpp,
      mk_cppmodule = .asSSM_createCppModuleFactory(moduleName, className)
    )
  }

  function(modelName) {
    if (!modelName %in% c("complex", "simple"))
      stop("Unknown Stan model: ", modelName, call. = FALSE)

    if (exists(modelName, envir = cache, inherits = FALSE))
      return(get(modelName, envir = cache, inherits = FALSE))

    stanFile <- .getStanFile(modelName)

    model <- tryCatch(
      .loadPrecompiledModel(modelName, stanFile),
      error = function(e) {
        if (isTRUE(getOption("jaspAcceptanceSampling.allowStanFallback", FALSE))) {
          header <- normalizePath(file.path(dirname(stanFile), "..", "include", "mixture_transport.hpp"),
                                  winslash = "/", mustWork = TRUE)
          return(rstan::stan_model(
            file = stanFile,
            model_name = paste0("jaspAcceptanceSampling_", modelName),
            allow_undefined = TRUE,
            includes = paste0('\n#include "', header, '"\n'),
            auto_write = FALSE
          ))
        }
        stop(
          "Precompiled Stan model could not be loaded (",
          modelName, "): ", e$message,
          call. = FALSE
        )
      }
    )

    assign(modelName, model, envir = cache)
    model
  }
})
