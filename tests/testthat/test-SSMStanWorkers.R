test_that("precompiled models retain their draws when transferred to fresh workers", {
  old <- options(jaspAcceptanceSampling.allowStanFallback = FALSE)
  on.exit(options(old), add = TRUE)
  getModel <- getFromNamespace(".asSSM_getStanModel", "jaspAcceptanceSampling")
  initialize <- getFromNamespace(".asSSM_createMixtureInitialValues", "jaspAcceptanceSampling")

  sampleModel <- function(model, dat, initial) {
    fit <- rstan::sampling(model, data = dat, init = initial,
                           chains = 1, cores = 1, chain_id = 1,
                           iter = 8, warmup = 0, algorithm = "Fixed_param",
                           seed = 42, refresh = 0)
    list(mode = fit@mode,
         draws = if (fit@mode == 0L)
           rstan::extract(fit, pars = "y_pred", permuted = FALSE, inc_warmup = FALSE)
         else NULL)
  }
  # Send only the sampler and its arguments, not the surrounding test environment.
  environment(sampleModel) <- baseenv()
  workerLogs <- vapply(c("simple", "complex"), function(name) tempfile(paste0(name, "-worker-")), character(1))
  on.exit(unlink(workerLogs), add = TRUE)

  for (name in c("simple", "complex")) {
    dat <- list(T = 3L, s = array(rep(1000L, 3), 3),
                n = array(rep(78L, 3), 3), y = array(c(1L, 5L, 8L), 3),
                prior_theta1_a = 1, prior_theta1_b = 1, prior_sigma_sd = 1)
    if (name == "complex") {
      dat$K <- 1L
      dat$X <- matrix(c(-1, 0, 1), ncol = 1)
      dat$prior_beta_scale <- 0.707
    }
    initial <- initialize(dat, 1L, predictor = name == "complex")
    model <- getModel(name)
    expected <- sampleModel(model, dat, initial)
    expect_equal(expected$mode, 0L, info = name)
    expect_identical(dim(expected$draws), c(8L, 1L, 1L), info = name)

    # Windows parallel chains start fresh R processes. Never initialize our model
    # in the worker before passing it the model created in the parent process.
    cluster <- parallel::makePSOCKcluster(1L, outfile = workerLogs[[name]])
    actual <- tryCatch({
      parallel::clusterCall(cluster, .libPaths, .libPaths())
      parallel::clusterEvalQ(cluster, {
        library(rstan)
        options(jaspAcceptanceSampling.allowStanFallback = FALSE)
        NULL
      })
      parallel::clusterCall(cluster, sampleModel, model, dat, initial)[[1L]]
    }, finally = parallel::stopCluster(cluster))
    info <- paste(name, paste(readLines(workerLogs[[name]], warn = FALSE), collapse = "\n"), sep = "\n")
    expect_equal(actual$mode, 0L, info = info)
    expect_identical(dim(actual$draws), c(8L, 1L, 1L), info = info)
    expect_equal(actual$draws, expected$draws, tolerance = 0, info = info)
  }
})
