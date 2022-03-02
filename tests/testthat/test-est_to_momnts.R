context("Estimates to Moments")

t1 <- est_to_momnts(data.frame(param = "RUD_p",
                               mean = 1,
                               sd = 2,
                               min = NA,
                               max = NA,
                               bg = NA),
                    param = "RUD_p")

test_that("works with estimates for normal", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 1)
  expect_equal(t1$b, 2)
  expect_equal(t1$dist, "norm")
  expect_equal(t1$source, "est_mmnts")
})

t1 <- est_to_momnts(data.frame(param = "RUD_p",
                               mean = NA,
                               sd = NA,
                               min = 1,
                               max = 3,
                               bg = NA),
                    param = "RUD_p")

test_that("works with range for normal", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 2)
  expect_equal(t1$b, 0.43, tolerance = 1e-2)
  expect_equal(t1$dist, "norm")
  expect_equal(t1$source, "est_range")
})



t1 <- est_to_momnts(data.frame(param = "RUD_p",
                               mean = NA,
                               sd = NA,
                               min = NA,
                               max = NA,
                               bg = 2),
                    param = "RUD_p")

test_that("works with bg for normal", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 2)
  expect_equal(t1$b, 0)
  expect_equal(t1$dist, "norm")
  expect_equal(t1$source, "est_bg")
})

t1 <- est_to_momnts(data.frame(param = "RUD_p",
                               mean = 5,
                               sd = NA,
                               min = 1,
                               max = 3,
                               bg = NA),
                    param = "RUD_p")

test_that("makes correct decision with normal", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 2)
  expect_equal(t1$b, 0.43, tolerance = 1e-2)
  expect_equal(t1$dist, "norm")
  expect_equal(t1$source, "est_range")
})





# CNT_s -------------------------------------------------------------------
t1 <- est_to_momnts(data.frame(param = "CNT_s",
                               mean = NA,
                               sd = NA,
                               min = 0.1,
                               max = 0.5,
                               bg = 0.4),
                    param = "CNT_s")

test_that("works with range + bg for beta", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 51.02, tolerance = 1e-2)
  expect_equal(t1$b, 76.37, tolerance = 1e-2)
  expect_equal(t1$dist, "beta")
  expect_equal(t1$source, "est_rangebg")
})

t1 <- est_to_momnts(data.frame(param = "CNT_s",
                               mean = NA,
                               sd = NA,
                               min = 0.1,
                               max = 0.5,
                               bg = NA),
                    param = "CNT_s")

test_that("works with range for beta", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 6.69, tolerance = 1e-2)
  expect_equal(t1$b, 17.78, tolerance = 1e-2)
  expect_equal(t1$dist, "beta")
  expect_equal(t1$source, "est_range")
})

t1 <- est_to_momnts(data.frame(param = "CNT_s",
                               mean = NA,
                               sd = NA,
                               min = NA,
                               max = NA,
                               bg = 0.5),
                    param = "CNT_s")

test_that("works with bg for beta", {
  expect_is(t1, "data.frame")
  expect_equal(names(t1), c("a", "b", "dist", "source"))
  expect_equal(nrow(t1), 1)
  expect_equal(t1$a, 0.5, tolerance = 1e-2)
  expect_equal(t1$b, 0, tolerance = 1e-2)
  expect_equal(t1$dist, "beta")
  expect_equal(t1$source, "est_bg")
})
