#' Convert raw measurements to moments
#'
#' @description a normal distribution is fitted - except for CNT_s,
#'   where a beta distribution is fitted.
#' @param x numeric; raw data
#' @param param character; parameter type
#' @return a data.frame with four columns:
#'   a (=mean), b (=sd), dist (=ditribution name), and source (='raw')
#' @export
#' @examples
#' raw <- c(-0.87, -0.43, -0.26, 2.02, -0.27)
#' raw_to_momnts(raw, "RUD_p")
#' raw2 <- c(0.94, 0.91, 0.79, 0.62, 0.83)
#' raw_to_momnts(raw2, "CNT_s")
raw_to_momnts <- function(x,
    param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s")) {
  param <- match.arg(param)
  # beta distribution
  if (param == "CNT_s") {
    ftd <- suppressWarnings(fitdistrplus::fitdist(x, "beta", method = "mle",
      optim.method = "L-BFGS-B", lower = c(0, 0)))
  } else {
    ftd <- suppressWarnings(fitdistrplus::fitdist(x, "norm"))
  }

  data.frame(a = ftd$estimate[1],
    b = ftd$estimate[2],
    dist = ftd$distname,
    source = "raw",
    stringsAsFactors = FALSE)
}


#' Convert estimates (mean + sd, min+max or best guess) to moments
#' @description The function converts various estimates to moments (
#'  mean & sd, min & max or best guess) in this order.
#'  The "param" (RUD_p, RUD_n, CONS_p, CONS_s, CNT_s) column is used to
#'  determine the distribution.
#'  For CNT_s a different order is used:
#'    (min & max & bg, min & max or best guess)
#' @param x a one row data.frame with columns "param", "mean", "sd", "min",
#'   "max" & "bg"
#' @param param character; parameter type
#' @param alpha numeric; alpha parameter for estimation from range
#' @return a data.frame with four columns:
#'   a (=mean), b (=sd), dist (=ditribution name), and source (='raw')
#' @importFrom rriskDistributions get.beta.par
#' @export
est_to_momnts <- function(x,
    param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"),
    alpha = 0.01) {
  param <- match.arg(param)
  stopifnot(is.data.frame(x))
  stopifnot(nrow(x) == 1)
  stopifnot(check_est(x, param))

  if (!param == "CNT_s") {
    # if moments are given
    if (!is.na(x$mean) & !is.na(x$sd)) {
      return(data.frame(a = x$mean,
        b = x$sd,
        dist = "norm",
        source = "est_mmnts",
        stringsAsFactors = FALSE))
    } else {
      # if range is give
      if (!is.na(x$min) & !is.na(x$max)) {
        a <- (x$min + x$max) / 2
        return(data.frame(a = a,
            b = (a - x$min) / stats::qnorm(1 - alpha),
            dist = "norm",
            source = "est_range",
            stringsAsFactors = FALSE))
      } else {
        # if bg is given
        if (!is.na(x$bg)) {
          return(data.frame(a = x$bg,
            b = 0,
            dist = "norm",
            source = "est_bg",
            stringsAsFactors = FALSE))
        }
      }
    }
  } else {
    # min and max and bg
    if (!is.na(x$min) & !is.na(x$max) & !is.na(x$bg)) {
      betapar <- suppressMessages(
        get.beta.par(p = c(0.01, 0.5, 0.99),
                              q = c(x$min, x$bg, x$max),
                              show.output = FALSE,
                              plot = FALSE))
      return(
        data.frame(a = betapar[1],
          b = betapar[2],
          dist = "beta",
          source = "est_rangebg",
          stringsAsFactors = FALSE))
    } else {
      # min and max
      if (!is.na(x$min) & !is.na(x$max)) {
        betapar <- suppressMessages(
          get.beta.par(p = c(0.01, 0.99),
            q = c(x$min, x$max),
            show.output = FALSE,
            plot = FALSE))
        return(
          data.frame(a = betapar[1],
            b = betapar[2],
            dist = "beta",
            source = "est_range",
            stringsAsFactors = FALSE))
      } else {
        # if bg is given return 0,0
        if (!is.na(x$bg)) {
          return(data.frame(a = x$bg,
                            b = 0,
                            dist = "beta",
                            source = "est_bg",
                            stringsAsFactors = FALSE))
        }
      }
    }
  }
}

#' Check if enough estimates are given and if they are valid
#' @param x a one row data.frame with columns "mean", "sd", "min", "max", "bg"
#' @param param character; parameter type
#' @export
check_est <- function(x,
    param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s")) {

  param <- match.arg(param)
  if (param != "CNT_s") {
    res <- ((!is.na(x$mean) & !is.na(x$sd)) |
      ((!is.na(x$min) & !is.na(x$max)) & x$min < x$max) |
      !is.na(x$bg))
    if (param %in% c("CONS_p", "CONS_s")) {
      res <- (res |
        ((!is.na(x$mean) & !is.na(x$sd)) & (x$mean > 0 & x$sd > 0)) |
        ((!is.na(x$min) & !is.na(x$max)) & x$min < x$max &
          x$min > 0 & x$max > 0))
    }
  } else {
    res <- (((!is.na(x$min) & !is.na(x$max) & !is.na(x$bg)) &
      (x$min < x$max) &
      x$bg >= 0 & x$bg <= 1 & x$min >= 0 & x$min <= 1 & x$max >= 0 &
      x$max <= 1) |
      ((!is.na(x$min) & !is.na(x$max)) & (x$min < x$max) & x$min >= 0 &
        x$min <= 1 & x$max >= 0 & x$max <= 1) |
      (!is.na(x$bg) & x$bg >= 0 & x$bg <= 1))
  }
  return(res)
}
 
#' Get and combine parameters from data.frames of raw data or estimates.
#' @import data.table
#' @param raw data.frame with columns (RUD_p, RUD_n, CONS_p, CONS_s, CNT_s).
#' @param est data.frame with columns (mean, sd, min, max, bg) and
#'   columns (RUD_p, RUD_n, CONS_p, CONS_s, CNT_s)
#' @return a data.frame with four columns:
#'   a (=mean), b (=sd), dist (=distribution name),
#'   and source (='raw', or 'estimate')
#'   and five rows (RUD_p, RUD_n, CONS_p, CONS_s, CNT_s)
#' @note data for RUD must be on log-scale
#' @export
#' @examples
#' raw <- structure(list(RUD_p = c(-0.87, -0.43, -0.26, 2.02, -0.27),
#'      RUD_n = c(1, 2, 3, 4, 5),
#'      CONS_p = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
#'      CONS_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
#'      CNT_s = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
#'    .Names = c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s"),
#'    row.names = c(NA, 5L),
#'    class = "data.frame")
#' est <- structure(list(
#'      param = c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s"),
#'      mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
#'      sd = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
#'      min = c(NA, NA, 1, NA, 0.1),
#'      max = c(NA, NA, 3, NA, 0.5),
#'      bg = c(NA, NA, NA, 5, 0.4)),
#'  .Names = c("param", "mean", "sd", "min", "max", "bg"),
#'  row.names = c(NA, -5L),
#'  class = "data.frame")
#'  get_param(raw, est)
get_param <- function(raw, est) {
  if (!is.null(raw)) {
    # parameters from raw data
    setDT(raw)
    # rm all NA cols
    raw <- raw[, colSums(is.na(raw)) < nrow(raw), with = FALSE]
    if (nrow(raw) == 0) {
      res_raw <- NULL
    } else {
      # set all to double [avoid warning coercion to double in melt]
      raw <- raw[, lapply(.SD, as.numeric)]
      raw <- melt(raw, variable.name = "param", measure.vars = names(raw))
      # keep only those  params with enough data
      raw <- raw[!is.na(value), ]
      raw <- raw[, if (.N > 2) .SD, by = param]
      if (nrow(raw) == 0) {
        res_raw <- NULL
      } else {
        res_raw <- raw[, raw_to_momnts(value), by = param]
      }
    }
  } else {
    res_raw <- NULL
  }

  setDT(est)
  # keep only those with enough data
  est <- est[est[, check_est(.SD, .BY$param), by = param, ]$V1, ]
  if (nrow(est) > 0) {
    res_est <- est[, est_to_momnts(.SD, .BY$param), by = param]
    # raw measurements have higher prio than est
    res_est <- res_est[!est$param %in% raw$param, ]
  } else {
    res_est <- NULL
  }

  param <- rbindlist(list(res_raw, res_est))
  return(param)
}
