context("Default values")

d <- default_estimates("Honey Bee", "adult", "forager", "acute")
d1 <- default_estimates("Honey Bee", "adult", "forager", "chronic")
# tests correct dispatch to "none"
d2 <- default_estimates("Honey Bee", "larvae")
d3 <- default_estimates("Bumble Bee", "larvae", "forager")
d4 <- default_estimates("Honey Bee", "larvae", "forager")
d5 <- default_estimates("Honey Bee", "adult", "forager", "acute")
d6 <- default_estimates("Honey Bee", "adult", "forager", "chronic")
d7 <- default_estimates("Honey Bee", "larvae", "forager", "acute")
d8 <- default_estimates("Bumble Bee", "larvae", "forager", "acute")
d10 <- default_estimates("Bumble Bee", "adult", "forager", "chronic")
d11 <- default_estimates("Bumble Bee", "adult", "forager", "acute")
# test application arguments
d9 <- default_estimates(
  "Honey Bee",
  "adult",
  "forager",
  "acute",
  "up/sidewards"
)
d12 <- default_estimates("Honey Bee", "adult", "nurse", "chronic")
d13 <- default_estimates(
  "Bumble Bee",
  "adult",
  "nurse",
  "acute",
  "up/sidewards"
)

test_that("gives correct values", {
  expect_equal(d$min[d$param == "CONS_s"], 80)
  expect_equal(d$mean[d$param == "RUD_p"], 2.56648)
  expect_equal(d$sd[d$param == "RUD_p"], 1.386)
  expect_equal(d$mean[d$param == "RUD_n"], 0.90752)
  expect_equal(d$sd[d$param == "RUD_n"], 1.153)
  expect_equal(d1$min[d$param == "CONS_s"], 32)
  expect_equal(d2$bg[d2$param == "CONS_p"], 2)
  expect_equal(d2$bg[d2$param == "CONS_s"], 59.4)
  expect_equal(d3$bg[d3$param == "CONS_s"], 23.8)
  expect_equal(d4$bg[d4$param == "CONS_p"], 2)
  expect_equal(d5$min[d5$param == "CONS_s"], 80)
  expect_equal(d6$min[d6$param == "CONS_s"], 32)
  expect_equal(d7$bg[d7$param == "CONS_s"], 59.4)
  expect_equal(d8$bg[d8$param == "CONS_s"], 23.8)
  expect_equal(d9$mean[d9$param == "RUD_p"], 0.1655144)
  expect_equal(d9$sd[d9$param == "RUD_p"], 1.127)
  expect_equal(d9$mean[d9$param == "RUD_n"], 1.390784)
  expect_equal(d9$sd[d9$param == "RUD_n"], 1.044)
  expect_equal(d10$min[d10$param == "CONS_s"], 73)
  expect_equal(d11$min[d11$param == "CONS_s"], 111)
  expect_equal(d12$bg[d12$param == "CONS_p"], 12)
  expect_equal(d13$min[d13$param == "CONS_s"], 111)
})
