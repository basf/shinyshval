context("SHVAL Monte Carlo")

raw <- structure(list(RUD_p = c(-0.87, -0.43, -0.26, 2.02, -0.27),
      RUD_n = c(1, 2, 3, 4, 5),
      CONS_p = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CONS_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CNT_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
  .Names = c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s"),
  row.names = c(NA, 5L),
  class = "data.frame")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"),
      mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      sd = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      min = c(NA, NA, 1, 4, 0.1),
      max = c(NA, NA, 3, 6, 0.5),
      bg = c(NA, NA, NA, NA, 0.4)),
  .Names = c("param", "mean", "sd", "min", "max", "bg"),
  row.names = c(NA, -5L),
  class = "data.frame")
p <- get_param(raw, est)
s <- sim(p)


test_that("simulation works", {
  expect_is(s, "numeric")
  expect_equal(length(s), 1000)
  expect_equal(mean(s), 1.6, tolerance = 1e-1)
  expect_equivalent(quantile(s, 0.025), 1.3, tolerance = 1e-1)
  expect_equivalent(quantile(s, 0.975), 1.8, tolerance = 1e-1)
})

s <- sim(p, twa = TRUE)
test_that("simulation with twa works", {
  expect_is(s, "numeric")
  expect_equal(length(s), 1000)
  expect_equal(mean(s), 0.4, tolerance = 1e-1)
  expect_equivalent(quantile(s, 0.025), 0.4, tolerance = 1e-1)
  expect_equivalent(quantile(s, 0.975), 0.5, tolerance = 1e-1)
})

raw <- structure(list(RUD_p = c(-0.87, -0.43, -0.26, 2.02, -0.27),
      RUD_n = c(1, 2, 3, 4, 5),
      CONS_p = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CONS_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CNT_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
  .Names = c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s"),
  row.names = c(NA, 5L),
  class = "data.frame")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"),
      mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      sd = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      min = c(NA, NA, 1, 4, NA),
      max = c(NA, NA, 3, 6, NA),
      bg = c(NA, NA, NA, NA, 0.4)),
  .Names = c("param", "mean", "sd", "min", "max", "bg"),
  row.names = c(NA, -5L),
  class = "data.frame")
p <- get_param(raw, est)
s <- sim(p)

test_that("simulation works with bg", {
  expect_is(s, "numeric")
  expect_equal(length(s), 1000)
  expect_equal(mean(s), 1.5, tolerance = 1e-1)
  expect_equivalent(quantile(s, 0.025), 1.3, tolerance = 1e-1)
  expect_equivalent(quantile(s, 0.975), 1.8, tolerance = 1e-1)
})
