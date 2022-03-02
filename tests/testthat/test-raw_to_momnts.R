context("Measurement to Moments")

x1 <- c(-0.87, -0.43, -0.26, 2.02, -0.27)
t1 <- raw_to_momnts(x1, "RUD_p")

x1b <- c(0.94, 0.91, 0.79, 0.62, 0.83)
t1b <- raw_to_momnts(x1b, "CNT_s")

test_that("gives same a original script", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 0.04, tolerance = 1e-2)
  expect_equal(t1$b, 1.02, tolerance = 1e-2)
  expect_equal(t1$dist, "norm")

  expect_is(t1b, "data.frame")
  expect_equal(names(t1b), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1b), 1)
  expect_equal(t1b$a, 9.77, tolerance = 1e-2)
  expect_equal(t1b$b, 2.18, tolerance = 1e-2)
  expect_equal(t1b$dist, "beta")
})


set.seed(1234)
x2 <- rnorm(100000, 2, 1)
t2 <- raw_to_momnts(x2, "RUD_p")

set.seed(1234)
x2b <- rbeta(100000, 3, 1)
t2b <- raw_to_momnts(x2b, "CNT_s")

test_that("gives correct result", {
  expect_is(t2, "data.frame")
  expect_equal(names(t2), c("a", "b", "dist", "source"))
  expect_equal(nrow(t2), 1)
  expect_equal(t2$a, 2, tolerance = 1e-2)
  expect_equal(t2$b, 1, tolerance = 1e-2)

  expect_is(t2b, "data.frame")
  expect_equal(names(t2b), c("a", "b", "dist", "source"))
  expect_equal(nrow(t2b), 1)
  expect_equal(t2b$a, 3, tolerance = 1e-2)
  expect_equal(t2b$b, 1, tolerance = 1e-2)
})
