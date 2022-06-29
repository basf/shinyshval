# Original ----------------------------------------------------------------
context("Test-case: original")

file <- system.file("test_reference/original/original.csv", package = "shinyshval")
dt <- parse_raw(file)

param <- get_param(dt$raw, dt$est)
test_that("Gives the same param", {
  expect_equal(param$a, c(2.57, 0.91, 30.3, 111, 0.15), tolerance = 1e-1)
  expect_equal(param$b, c(1.36, 1.13, 0, 16.33, 0), tolerance = 1e-1)
})

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 9.5, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(8.6, 10.6),
               tolerance = 1e-1)
})


stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 2.7, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(2.4, 3),
               tolerance = 1e-1)
})

# only_bg ----------------------------------------------------------------
context("Test-case: only_bg")
file <- system.file("test_reference/only_bg/only_bg.csv", package = "shinyshval")
dt <- parse_raw(file)

param <- get_param(dt$raw, dt$est)
test_that("Gives the same param", {
  expect_equal(param$a, c(5, 5, 30.3, 80, 0.15), tolerance = 1e-1)
  expect_equal(param$b, c(0, 0, 0, 0, 0), tolerance = 1e-1)
})

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 83.7, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(83.7, 83.7),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 23.4, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(23.4, 23.4),
               tolerance = 1e-1)
})


# only_meansd ----------------------------------------------------------------
context("Test-case: only_meansd")
file <- system.file("test_reference/only_meansd/only_meansd.csv", package = "shinyshval")
dt <- parse_raw(file)

param <- get_param(dt$raw, dt$est)
test_that("Gives the same param", {
  expect_equal(param$a, c(3, 5, 30, 80, 51.02), tolerance = 1e-1)
  expect_equal(param$b, c(1, 1.5, 4, 2, 76.37), tolerance = 1e-1)
})

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 205.7, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(175.8, 240.6),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 57.5, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(49.1, 67.2),
               tolerance = 1e-1)
})


# only_range ----------------------------------------------------------------
context("Test-case: only_range")
file <- system.file("test_reference/only_range/only_range.csv", package = "shinyshval")
dt <- parse_raw(file)

param <- get_param(dt$raw, dt$est)
test_that("Gives the same param", {
  expect_equal(param$a, c(2, 5, 30, 111, 6.69), tolerance = 1e-1)
  expect_equal(param$b, c(0.43, 0.43, 4.3, 16.33, 17.78), tolerance = 1e-1)
})


snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 130, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(123, 138),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 36.3, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(34.4, 38.6),
               tolerance = 1e-1)
})


# only_raw ----------------------------------------------------------------
context("Test-case: only_raw")
file <- system.file("test_reference/only_raw/only_raw.csv", package = "shinyshval")
dt <- parse_raw(file)

param <- get_param(dt$raw, dt$est)
test_that("Gives the same param", {
  expect_equal(param$a, c(2.58, 1.36, 18.69, 9.77, 0.15), tolerance = 1e-1)
  expect_equal(param$b, c(1.3, 1.39, 22.57, 10.84, 0), tolerance = 1e-1)
})

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 2.7, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(2.4, 3),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 0.7, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(0.7, 0.8),
               tolerance = 1e-1)
})


# original_dtchanged -------------------------------------------------------
context("Test-case: original_dtchanged")
file <- system.file("test_reference/original_dtchanged/original_dtchanged.csv",
                    package = "shinyshval")
dt <- parse_raw(file)

param <- get_param(dt$raw, dt$est)
test_that("Gives the same param", {
  expect_equal(param$a, c(2.57, 0.91, 30.3, 111, 0.15), tolerance = 1e-1)
  expect_equal(param$b, c(1.36, 1.13, 0, 16.33, 0), tolerance = 1e-1)
})

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 9.5, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(8.6, 10.6),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 3.9, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(3.5, 4.3),
               tolerance = 1e-1)
})


# original_dtchanged_tchanged -----------------------------------------------
context("Test-case: original_dtchanged_tchanged")
file <- system.file(
  "test_reference/original_dtchanged_tchanged/original_dtchanged_tchanged.csv",
  package = "shinyshval")
dt <- parse_raw(file)

param <- get_param(dt$raw, dt$est)
test_that("Gives the same param", {
  expect_equal(param$a, c(2.57, 0.91, 30.3, 111, 0.15), tolerance = 1e-1)
  expect_equal(param$b, c(1.36, 1.13, 0, 16.33, 0), tolerance = 1e-1)
})

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 9.5, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(8.6, 10.6),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 2.2, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(2, 2.4),
               tolerance = 1e-1)
})

