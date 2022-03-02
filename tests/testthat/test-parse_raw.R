context("Parse original inputs")

res <- parse_raw(system.file("extdata/original.csv", package = "shinyshval"))

test_that("parsing original files works", {
  expect_is(res, "list")
  expect_equal(names(res), c("raw", "est", "set"))

  expect_is(res$raw, "data.frame")
  expect_equal(names(res$raw), c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"))

  expect_is(res$est, "data.frame")
  expect_equal(names(res$est), c("param", "mean", "sd", "min", "max", "bg"))
  expect_equal(res$est$param, c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"))
  expect_equal(res$est$min[res$est$param == "CONS_s"], 73)
  expect_equal(res$est$bg[res$est$param == "CNT_s"], 0.15)
  expect_true(is.na(res$est$mean[res$est$param == "CNT_s"]))

  expect_is(res$set, "data.frame")
  expect_equal(names(res$set), c("DT50_p",	"DT50_n", "t"))
  expect_equal(res$set$DT50_p, 2)
})
