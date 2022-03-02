# Original ----------------------------------------------------------------
context("Test-case: original")

file <- system.file("test_reference/original/original.csv", package = "shinyshval")
dt <- parse_raw(file)
param <- get_param(dt$raw, dt$est)

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

# EFSA_j4_forager_acute -----------------------------------------------------
context("Test-case: EFSA_j4_hb_forager_acute")
file <- system.file(
  "test_reference/EFSA_j4_forager_acute/EFSA_j4_forager_acute.csv",
  package = "shinyshval")
dt <- parse_raw(file)
param <- get_param(dt$raw, dt$est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 36.2, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(32.1, 40.8),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 10.1, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(9, 11.4),
               tolerance = 1e-1)
})

# EFSA_j4_forager_chronic ----------------------------------------------------
context("Test-case: EFSA_j4_forager_chronic")

file <- system.file(
  "test_reference/EFSA_j4_forager_chronic/EFSA_j4_forager_chronic.csv",
  package = "shinyshval")
dt <- parse_raw(file)
param <- get_param(dt$raw, dt$est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 27.8, tolerance = 1e-1)
  expect_equal(unname(quantile(snotwa, c(0.025, 0.975))), c(24.8, 31.4),
               tolerance = 1e-1)
})

stwa <- sim(param, twa = TRUE, dt50_p = dt$set$DT50_p,
            dt50_n = dt$set$DT50_n, t = dt$set$t)
test_that("TWA gives the same SV", {
  expect_equal(mean(stwa), 7.78, tolerance = 1e-1)
  expect_equal(unname(quantile(stwa, c(0.025, 0.975))), c(6.9, 8.8),
               tolerance = 1e-1)
})
