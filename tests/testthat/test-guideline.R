# Tests for Tables j4-J9 of the guideline
# Note that RUD input parameters are provided on the raw scale and not the log-scale.
# 


# EFSA_j4_forager_acute -----------------------------------------------------
# test case to reprodcuce table J4
context("Test-case: EFSA_j4_forager_acute")
# test file "test_reference/EFSA_j4_forager_acute/EFSA_j4_forager_acute.csv"

param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "adult", "forager",
                          "acute", "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 7.6, tolerance = 1e-1)
})


# EFSA_j4_forager_chronic ----------------------------------------------------
context("Test-case: EFSA_j4_forager_chronic")

param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "adult", "forager", "chronic",
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 5.8, tolerance = 1e-1)
})


# EFSA_j4_nurse ----------------------------------------------------
context("Test-case: EFSA_j4_hb_nurse")

param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "adult", "nurse", NULL,
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 3.8, tolerance = 1e-1)
})

# EFSA_j4_larva ----------------------------------------------------
context("Test-case: EFSA_j4_hb_larva")

param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "larvae", NULL, NULL,
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 4.4, tolerance = 1e-1)
})

# EFSA_j4_bb_adult_acute ----------------------------------------------------
context("Test-case: EFSA_j4_bb_adult_acute")

param <- get_param(raw = NULL,
  est = default_estimates("Bumble Bee", "adult", NULL, "acute",
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 11.2, tolerance = 1e-1)
})

# EFSA_j4_bb_adult_chronic ----------------------------------------------------
context("Test-case: EFSA_j4_bb_adult_chronic")

param <- get_param(raw = NULL,
  est = default_estimates("Bumble Bee", "adult", NULL, "chronic",
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 9.9, tolerance = 1e-1)
})

# EFSA_j4_bb_larvae ----------------------------------------------------
context("Test-case: EFSA_j4_bb_larvae")

param <- get_param(raw = NULL,
  est = default_estimates("Bumble Bee", "larvae", NULL, NULL,
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 4.5, tolerance = 1e-1)
})

# EFSA_j4_sb_adult ----------------------------------------------------
context("Test-case: EFSA_j4_sb_adult")

param <- get_param(raw = NULL,
  est = default_estimates("Solitary Bee", "adult", NULL, NULL,
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 5.7, tolerance = 1e-1)
})

# EFSA_j4_sb_larvae ----------------------------------------------------
context("Test-case: EFSA_j4_sb_larvae")

param <- get_param(raw = NULL,
  est = default_estimates("Solitary Bee", "larvae", NULL, NULL,
                          "downwards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 33.6, tolerance = 1e-1)
})





# EFSA_j5_hb_forager_acute ----------------------------------------------------
context("Test-case: EFSA_j5_forager_acute_sidewards")
param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "adult", "forager", "acute",
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 10.6, tolerance = 1e-1)
})

# EFSA_j5_hb_forager_chronic----------------------------------------------------
context("Test-case: EFSA_j5_forager_chronic_sidewards")
param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "adult", "forager", "chronic",
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 8.2, tolerance = 1e-1)
})

# EFSA_j5_nurse ----------------------------------------------------
context("Test-case: EFSA_j5_hb_nurse")

param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "adult", "nurse", NULL,
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 4.3, tolerance = 1e-1)
})

# EFSA_j5_larva ----------------------------------------------------
context("Test-case: EFSA_j5_hb_larva")

param <- get_param(raw = NULL,
  est = default_estimates("Honey Bee", "larvae", NULL, NULL,
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 6.1, tolerance = 1e-1)
})

# EFSA_j5_bb_adult_acute ----------------------------------------------------
context("Test-case: EFSA_j5_bb_adult_acute")

param <- get_param(raw = NULL,
  est = default_estimates("Bumble Bee", "adult", NULL, "acute",
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 13.3, tolerance = 1e-1)
})

# EFSA_j5_bb_adult_chronic ----------------------------------------------------
context("Test-case: EFSA_j5_bb_adult_chronic")

param <- get_param(raw = NULL,
  est = default_estimates("Bumble Bee", "adult", NULL, "chronic",
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 11.4, tolerance = 1e-1)
})

# EFSA_j5_bb_larvae ----------------------------------------------------
context("Test-case: EFSA_j5_bb_larvae")

param <- get_param(raw = NULL,
  est = default_estimates("Bumble Bee", "larvae", NULL, NULL,
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 2.5, tolerance = 1e-1)
})

# EFSA_j5_sb_adult ----------------------------------------------------
context("Test-case: EFSA_j5_sb_adult")

param <- get_param(raw = NULL,
  est = default_estimates("Solitary Bee", "adult", NULL, NULL,
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 7.3, tolerance = 1e-1)
})

# EFSA_j5_sb_larvae ----------------------------------------------------
context("Test-case: EFSA_j5_sb_larvae")

param <- get_param(raw = NULL,
  est = default_estimates("Solitary Bee", "larvae", NULL, NULL,
                          "up/sidewards"))

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 9.6, tolerance = 1e-1)
})



# EFSA_j6_hb_forager_acute ----------------------------------------------------
context("Test-case: EFSA_j6_forager_acute")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 80, NA), 
max = c(NA, NA, NA, 128, NA), 
bg = c(NA, NA, NA, NA, 0.15)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.7, tolerance = 1e-2)
})

# EFSA_j6_hb_forager_chronic----------------------------------------------------
context("Test-case: EFSA_j6_forager_chronic")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 32, NA), 
max = c(NA, NA, NA, 128, NA), 
bg = c(NA, NA, NA, NA, 0.15)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.54, tolerance = 1e-2)
})

# EFSA_j6_nurse ----------------------------------------------------
context("Test-case: EFSA_j6_hb_nurse")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 34, NA), 
max = c(NA, NA, NA, 50, NA), 
bg = c(NA, NA, 12, NA, 0.15)), 
row.names = c(NA,  5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.28, tolerance = 1e-2)
})

# EFSA_j6_larva ----------------------------------------------------
context("Test-case: EFSA_j6_hb_larva")


est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, NA, NA), 
max = c(NA, NA, NA, NA, NA), 
bg = c(NA, NA, 2, 59.4, 0.15)), 
row.names = c(NA,  5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.4, tolerance = 1e-2)
})

# EFSA_j6_bb_adult_acute ----------------------------------------------------
context("Test-case: EFSA_j6_bb_adult_acute")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 111, NA), 
max = c(NA, NA, NA, 149, NA), 
bg = c(NA, NA, 30.3, NA, 0.15)), 
row.names = c(NA,  5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.87, tolerance = 1e-2)
})

# EFSA_j6_bb_adult_chronic ----------------------------------------------------
context("Test-case: EFSA_j6_bb_adult_chronic")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 73, NA), 
max = c(NA, NA, NA, 149, NA), 
bg = c(NA, NA, 30.3, NA, 0.15)), 
row.names = c(NA,  5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.75, tolerance = 1e-2)
})



# EFSA_j6_bb_larvae ----------------------------------------------------
context("Test-case: EFSA_j6_bb_larvae")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, NA, NA), 
max = c(NA, NA, NA, NA, NA), 
bg = c(NA, NA, 39.5, 23.8, 0.15)), 
row.names = c(NA,  5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.16, tolerance = 1e-2)
})

# EFSA_j6_sb_adult ----------------------------------------------------
context("Test-case: EFSA_j6_sb_adult")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 18, NA), 
max = c(NA, NA, NA, 77, NA), 
bg = c(NA, NA, 10.2, NA, 0.10)), 
row.names = c(NA,  5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.48, tolerance = 1e-2)
})


# EFSA_j6_sb_larvae ----------------------------------------------------
context("Test-case: EFSA_j6_sb_larva")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, NA, NA), 
max = c(NA, NA, NA, NA, NA), 
bg = c(NA, NA, 387, 54, 0.10)), 
row.names = c(NA,  5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.54, tolerance = 1e-2) 
})


# EFSA_j7_hb_forager_acute ----------------------------------------------------
context("Test-case: EFSA_j7_forager_acute")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0.9074518, 0, NA, NA), 
sd = c(0, 1.153, 0, NA, NA), 
min = c(NA, NA, NA, 80, NA), 
max = c(NA, NA, NA, 128, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 3.7, tolerance = 1e-1)
})

# EFSA_j7_hb_forager_chronic----------------------------------------------------
context("Test-case: EFSA_j7_forager_chronic")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0.9074518, 0, NA, NA), 
sd = c(0, 1.153, 0, NA, NA), 
min = c(NA, NA, NA, 32, NA), 
max = c(NA, NA, NA, 128, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 2.9, tolerance = 1e-1)
})

# EFSA_j7_nurse ----------------------------------------------------
context("Test-case: EFSA_j7_hb_nurse")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(2.566487, 0.9074518, NA, NA, NA), 
sd = c(1.386, 1.153, NA, NA, NA), 
min = c(NA, NA, 6.5, 34, NA), 
max = c(NA, NA, 12, 50, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 2.1, tolerance = 1e-1)
})

# EFSA_j7_larva ----------------------------------------------------
context("Test-case: EFSA_j7_hb_larva")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(2.566487, 0.9074518, NA, NA, NA), 
sd = c(1.386, 1.153, NA, NA, NA), 
min = c(NA, NA, 1.5, NA, NA), 
max = c(NA, NA, 2, NA, NA), 
bg = c(NA, NA, NA, 59.4, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 2.2, tolerance = 1e-1)
})

# EFSA_j7_bb_adult_acute ----------------------------------------------------
context("Test-case: EFSA_j7_bb_adult_acute")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(2.566487, 0.9074518, NA, NA, NA), 
sd = c(1.386, 1.153, NA, NA, NA), 
min = c(NA, NA, 26.6, 111, NA), 
max = c(NA, NA, 30.3, 149, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 6.5, tolerance = 1e-1)
})

# EFSA_j7_bb_adult_chronic ----------------------------------------------------
context("Test-case: EFSA_j7_bb_adult_chronic")


est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(2.566487, 0.9074518, NA, NA, NA), 
sd = c(1.386, 1.153, NA, NA, NA), 
min = c(NA, NA, 26.6, 73, NA), 
max = c(NA, NA, 30.3, 149, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 5.9, tolerance = 1e-1)
})


# EFSA_j7_bb_larvae ----------------------------------------------------
context("Test-case: EFSA_j7_bb_larvae")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(2.566487, 0.9074518, NA, NA, NA), 
sd = c(1.386, 1.153, NA, NA, NA), 
min = c(NA, NA, 10.3, NA, NA), 
max = c(NA, NA, 39.5, NA, NA), 
bg = c(NA, NA, NA, 23.8, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 2.6, tolerance = 1e-1)
})

# EFSA_j7_sb_adult ----------------------------------------------------
context("Test-case: EFSA_j7_sb_adult")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(2.566487, 0.9074518, NA, NA, NA), 
sd = c(1.386, 1.153, NA, NA, NA), 
min = c(NA, NA, NA, 18, NA), 
max = c(NA, NA, NA, 77, NA), 
bg = c(NA, NA, 10.2, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 2.3, tolerance = 1e-1)
})



# EFSA_j7_sb_larvae ----------------------------------------------------
context("Test-case: EFSA_j7_sb_larva")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(2.566487, 0.9074518, NA, NA, NA), 
sd = c(1.386, 1.153, NA, NA, NA), 
min = c(NA, NA, NA, NA, NA), 
max = c(NA, NA, NA, NA, NA), 
bg = c(NA, NA, 387, 54, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 30.8, tolerance = 1e-1)
})


# EFSA_j8_hb_forager_acute ----------------------------------------------------
context("Test-case: EFSA_j8_forager_acute")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 80, NA), 
max = c(NA, NA, NA, 128, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.35, tolerance = 1e-1)
})

# EFSA_j8_hb_forager_chronic----------------------------------------------------
context("Test-case: EFSA_j8_forager_chronic")
est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, 0, NA, NA), 
sd = c(0, 0, 0, NA, NA), 
min = c(NA, NA, NA, 32, NA), 
max = c(NA, NA, NA, 128, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.27, tolerance = 1e-2)
})

# EFSA_j8_nurse ----------------------------------------------------
context("Test-case: EFSA_j8_hb_nurse")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, NA, NA, NA), 
sd = c(0, 0, NA, NA, NA), 
min = c(NA, NA, 6.5, 34, NA), 
max = c(NA, NA, 12, 50, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.15, tolerance = 1e-2)
})

# EFSA_j8_larva ----------------------------------------------------
context("Test-case: EFSA_j8_hb_larva")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, NA, NA, NA), 
sd = c(0, 0, NA, NA, NA), 
min = c(NA, NA, 1.5, NA, NA), 
max = c(NA, NA, 2, NA, NA), 
bg = c(NA, NA, NA, 59.4, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.2, tolerance = 1e-2)
})

# EFSA_j8_bb_adult_acute ----------------------------------------------------
context("Test-case: EFSA_j8_bb_adult_acute")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, NA, NA, NA), 
sd = c(0, 0, NA, NA, NA), 
min = c(NA, NA, 26.6, 111, NA), 
max = c(NA, NA, 30.3, 149, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.46, tolerance = 1e-2)
})

# EFSA_j8_bb_adult_chronic ----------------------------------------------------
context("Test-case: EFSA_j8_bb_adult_chronic")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, NA, NA, NA), 
sd = c(0, 0, NA, NA, NA), 
min = c(NA, NA, 26.6, 73, NA), 
max = c(NA, NA, 30.3, 149, NA), 
bg = c(NA, NA, NA, NA, 0.3)), 
row.names = c(NA, 5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.40, tolerance = 1e-2)
})


# EFSA_j8_bb_larvae ----------------------------------------------------
context("Test-case: EFSA_j8_bb_larvae")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, NA, NA, NA), 
sd = c(0, 0, NA, NA, NA), 
min = c(NA, NA, 10.3, NA, NA), 
max = c(NA, NA, 39.5, NA, NA), 
bg = c(NA, NA, NA, 23.8, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.11, tolerance = 1e-2)
})

# EFSA_j8_sb_adult ----------------------------------------------------
context("Test-case: EFSA_j8_sb_adult")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, NA, NA, NA), 
sd = c(0, 0, NA, NA, NA), 
min = c(NA, NA, NA, 18, NA), 
max = c(NA, NA, NA, 77, NA), 
bg = c(NA, NA, 10.2, NA, 0.3)), 
row.names = c(NA, 
5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.17, tolerance = 1e-2)
})



# EFSA_j8_sb_larvae ----------------------------------------------------
context("Test-case: EFSA_j8_sb_larva")

est <- structure(list(param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", 
"CNT_s"), 
mean = c(0, 0, NA, NA, NA), 
sd = c(0, 0, NA, NA, NA), 
min = c(NA, NA, NA, NA, NA), 
max = c(NA, NA, NA, NA, NA), 
bg = c(NA, NA, 387, 54, 0.3)), 
row.names = c(NA, 5L), class = "data.frame")

param <- get_param(raw = NULL,
  est = est)

snotwa <- sim(param)
test_that("NoTWA gives the same SV", {
  expect_equal(mean(snotwa), 0.57, tolerance = 1e-2)
})