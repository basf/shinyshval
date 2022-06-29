context("Parameter estimation")

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

test_that("returns correct object", {
  expect_is(p, "data.frame")
  expect_equal(names(p), c("param", "a", "b", "dist", "source"))
  expect_equal(nrow(p), 5)
  expect_true(all(as.character(p$param) %in%
                    c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s")))
})

test_that("returns same results as shvalv1.1.R", {
  expect_equal(p$a[p$param == "RUD_p"], 0.04, tolerance = 1e-2)
  expect_equal(p$b[p$param == "RUD_p"], 1.02, tolerance = 1e-2)
  expect_equal(p$dist[p$param == "RUD_p"], "norm")
  expect_equal(p$source[p$param == "RUD_p"], "raw")

  expect_equal(p$a[p$param == "RUD_n"], 3, tolerance = 1e-2)
  expect_equal(p$b[p$param == "RUD_n"], 1.414, tolerance = 1e-2)
  expect_equal(p$dist[p$param == "RUD_n"], "norm")
  expect_equal(p$source[p$param == "RUD_n"], "raw")

  expect_equal(p$a[p$param == "CONS_p"], 2, tolerance = 1e-2)
  expect_equal(p$b[p$param == "CONS_p"], 0.43, tolerance = 1e-2)
  expect_equal(p$dist[p$param == "CONS_p"], "norm")
  expect_equal(p$source[p$param == "CONS_p"], "est_range")

  expect_equal(p$a[p$param == "CONS_s"], 5, tolerance = 1e-2)
  expect_equal(p$b[p$param == "CONS_s"], 0.43, tolerance = 1e-2)
  expect_equal(p$dist[p$param == "CONS_s"], "norm")
  expect_equal(p$source[p$param == "CONS_s"], "est_range")

  expect_equal(p$a[p$param == "CNT_s"], 51.02, tolerance = 1e-2)
  expect_equal(p$b[p$param == "CNT_s"], 76.37, tolerance = 1e-2)
  expect_equal(p$dist[p$param == "CNT_s"], "beta")
  expect_equal(p$source[p$param == "CNT_s"], "est_rangebg")
})

raw <- structure(list(RUD_p = c(-0.87, -0.43, NA, NA, NA),
      RUD_n = c(1, 2, NA, NA, NA),
      CONS_p = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CONS_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CNT_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
  .Names = c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s"),
  row.names = c(NA, 5L),
  class = "data.frame")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"),
      mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      sd = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      min = c(NA, NA, 1, NA, NA),
      max = c(NA, NA, NA, NA, NA),
      bg = c(NA, NA, NA, NA, NA)),
  .Names = c("param", "mean", "sd", "min", "max", "bg"),
  row.names = c(NA, -5L),
  class = "data.frame")
p <- get_param(raw, est)

test_that("works with missings", {
  expect_is(p, "data.frame")
  expect_equal(nrow(p), 0)
})


raw <- structure(list(
      RUD_p = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      RUD_n = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CONS_p = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CONS_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CNT_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
  .Names = c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s"),
  row.names = c(NA, 5L),
  class = "data.frame")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"),
      mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      sd = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      min = c(NA, NA, NA, NA, NA),
      max = c(NA, NA, NA, NA, NA),
      bg = c(NA, NA, NA, 5, 0.4)),
  .Names = c("param", "mean", "sd", "min", "max", "bg"),
  row.names = c(NA, -5L),
  class = "data.frame")
p <- get_param(raw, est)

test_that("works with bg & returns same as shvalv1.1.R", {
  expect_equal(p$a[p$param == "CONS_s"], 5, tolerance = 1e-2)
  expect_equal(p$b[p$param == "CONS_s"], 0, tolerance = 1e-2)
  expect_equal(p$dist[p$param == "CONS_s"], "norm")
  expect_equal(p$source[p$param == "CONS_s"], "est_bg")

  expect_equal(p$a[p$param == "CNT_s"], 0.4, tolerance = 1e-2)
  expect_equal(p$b[p$param == "CNT_s"], 0, tolerance = 1e-2)
  expect_equal(p$dist[p$param == "CNT_s"], "beta")
  expect_equal(p$source[p$param == "CNT_s"], "est_bg")
})

raw <- structure(list(RUD_p = c(-0.87, -0.43, NA, NA, NA),
      RUD_n = c(1, 2, NA, NA, NA),
      CONS_p = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CONS_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      CNT_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
  .Names = c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s"),
  row.names = c(NA, 5L),
  class = "data.frame")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"),
      mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      sd = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      min = c(NA, NA, 1, 2, NA),
      max = c(NA, NA, 2, 1, NA),
      bg = c(NA, NA, NA, NA, -0.15)),
  .Names = c("param", "mean", "sd", "min", "max", "bg"),
  row.names = c(NA, -5L),
  class = "data.frame")

p <- get_param(raw, est)
test_that("Catches wrong inputs in est", {
  expect_equal(p$param, c("CONS_p"))
})
