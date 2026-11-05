# Stan model loader for Bayesian SSM acceptance sampling.
# Prefer precompiled rstantools modules (stable in JASP engine), with
# runtime stan_model() fallback for source-only development workflows.
# This file is kept separate from a future generated R/stanmodels.R so the
# package can adopt the standard rstantools layout without name conflicts.

.asSSM_cppModuleFactory <- function(module_name, class_name) {
  # Socket workers must initialize their own Rcpp class. Capture only its names.
  force(module_name)
  force(class_name)
  function(x) {
    module <- Rcpp::Module(module_name, PACKAGE = "jaspAcceptanceSampling", mustStart = TRUE)
    eval(substitute(M$X, list(M = module, X = as.name(class_name))))
  }
}

.asSSM_getStanModel <- local({
  cache <- new.env(parent = emptyenv())
  .stanFile <- function(model_name) {
    path <- system.file("stan", paste0(model_name, ".stan"), package = "jaspAcceptanceSampling")
    if (!nzchar(path))
      path <- file.path("inst", "stan", paste0(model_name, ".stan"))

    if (!nzchar(path) || !file.exists(path))
      stop("Stan model file not found for model: ", model_name, call. = FALSE)

    path
  }

  .precompiled <- function(model_name, stan_file) {
    expected_suffix <- paste0("_", model_name, "_mod")
    module_candidates <- paste0("stan_fit4", model_name, "_mod")

    reg <- tryCatch(getDLLRegisteredRoutines("jaspAcceptanceSampling"), error = function(e) NULL)
    if (!is.null(reg) && !is.null(reg[[".Call"]])) {
      call_names <- names(reg[[".Call"]])
      boot_names <- grep("^_rcpp_module_boot_.*_mod$", call_names, value = TRUE)
      if (length(boot_names) > 0L) {
        discovered <- sub("^_rcpp_module_boot_", "", boot_names)
        discovered <- discovered[endsWith(discovered, expected_suffix)]
        if (length(discovered) > 0L)
          module_candidates <- unique(c(discovered, module_candidates))
      }
    }

    module_obj <- NULL
    module_class <- NULL

    for (module_name in module_candidates) {
      class_name <- NULL
      module_obj <- tryCatch(
        Rcpp::Module(module_name, PACKAGE = "jaspAcceptanceSampling", mustStart = TRUE),
        error = function(e) NULL
      )
      if (is.null(module_obj))
        next

      module_print <- capture.output(print(module_obj))
      class_line <- grep("^\\s*\\d+\\s+classes\\s*:\\s*$", module_print)
      if (length(class_line) > 0L) {
        after <- trimws(module_print[(class_line[1] + 1L):length(module_print)])
        after <- after[nzchar(after)]
        if (length(after) > 0L)
          class_name <- after[1L]
      }
      if (is.null(class_name) || !nzchar(class_name))
        class_name <- paste0("rstantools_model_", model_name)

      module_class <- tryCatch(
        eval(substitute(M$X, list(M = module_obj, X = as.name(class_name)))),
        error = function(e) NULL
      )
      if (!is.null(module_class))
        break
    }

    if (is.null(module_class))
      stop("Precompiled Stan module class not available for model: ", model_name, call. = FALSE)

    stanfit <- rstan::stanc_builder(
      stan_file,
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
      mk_cppmodule = .asSSM_cppModuleFactory(module_name, class_name)
    )
  }

  function(model_name) {
    if (!model_name %in% c("complex", "simple"))
      stop("Unknown Stan model: ", model_name, call. = FALSE)

    if (exists(model_name, envir = cache, inherits = FALSE))
      return(get(model_name, envir = cache, inherits = FALSE))

    stan_file <- .stanFile(model_name)

    model <- tryCatch(
      .precompiled(model_name, stan_file),
      error = function(e) {
        if (isTRUE(getOption("jaspAcceptanceSampling.allowStanFallback", FALSE))) {
          header <- normalizePath(file.path(dirname(stan_file), "..", "include", "mixture_transport.hpp"),
                                  winslash = "/", mustWork = TRUE)
          return(rstan::stan_model(
            file = stan_file,
            model_name = paste0("jaspAcceptanceSampling_", model_name),
            allow_undefined = TRUE,
            includes = paste0('\n#include "', header, '"\n'),
            auto_write = FALSE
          ))
        }
        stop(
          "Precompiled Stan model could not be loaded (",
          model_name, "): ", e$message,
          call. = FALSE
        )
      }
    )

    assign(model_name, model, envir = cache)
    model
  }
})
