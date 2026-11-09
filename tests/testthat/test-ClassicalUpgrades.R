test_that("released classical option migrations retain settings and metadata", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is unavailable for executing the migration JavaScript")
  script <- test_path("fixtures", "check-classical-upgrades.js")
  upgrades <- test_path("..", "..", "inst", "Upgrades.qml")
  fixtures <- test_path("fixtures", "classical-upgrades.json")
  output <- suppressWarnings(system2(node, shQuote(c(script, upgrades, fixtures)),
                                     stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  expect_true(is.null(status) || status == 0L, info = paste(output, collapse = "\n"))
  expect_true(any(grepl("^PASS: .* migration scenarios", output)),
              info = paste(output, collapse = "\n"))
})
