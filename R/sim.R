#' Compute 90th percentile of shval
#' @param param data.frame; a data.frame holding th estimates,
#'   as returned by [get_param()]
#' @param nsim integer; number of simulation to perform
#' @param iter integer; number of iterations
#' @param q numeric; with quantile should be returned?
#' @param twa logical; should a time-weighted-average (twa) be returned?
#' @param dt50_p numeric; needed for twa
#' @param dt50_n numeric; needed for twa
#' @param t numeric; needed for twa
#' @export
#' @return a numeric vector
sim <- function(param, nsim = 1000, iter = nsim, q = 0.9, twa = FALSE,
                dt50_p = 2, dt50_n = 2, t = 10) {

  set.seed(123456)
  cons_p_sim <- apply(matrix(stats::rnorm(nsim * nsim,
                     mean = param$a[param$param == "CONS_p"],
                     sd = param$b[param$param == "CONS_p"]),
               nrow = nsim, ncol = nsim, byrow = TRUE), 2, stats::median)
  set.seed(654321)
  cons_s_sim <- apply(
    matrix(
      stats::rnorm(nsim * nsim,
        mean = param$a[param$param == "CONS_s"],
        sd = param$b[param$param == "CONS_s"]),
      nrow = nsim, ncol = nsim, byrow = TRUE), 2, stats::median)

  sv <- numeric(iter)
  for (j in 1:iter) {
    set.seed(123456 + j)
    rud_p_sim <- stats::rnorm(nsim, param$a[param$param == "RUD_p"],
      param$b[param$param == "RUD_p"])
    set.seed(654321 + j)
    rud_n_sim <- stats::rnorm(nsim, param$a[param$param == "RUD_n"],
      param$b[param$param == "RUD_n"])
    set.seed(13579 + j)
    # use best guess
    if (param$source[param$param == "CNT_s"] == "est_bg") {
      cnt_s_sim <- rep(param$a[param$param == "CNT_s"], nsim)
    } else {
      cnt_s_sim <- stats::rbeta(nsim, param$a[param$param == "CNT_s"],
        param$b[param$param == "CNT_s"])
    }

    if (twa) {
      kp <- log(2) / dt50_p
      kn <- log(2) / dt50_n
      tlag <- t
      svs <- (((exp(rud_p_sim) * cons_p_sim / (kp * tlag)) *
        (1 - exp(-kp * tlag))) +
        ((exp(rud_n_sim) * (cons_s_sim / cnt_s_sim) / (kn * tlag)) *
        (1 - exp(-kn * tlag)))) / 1000
    } else {
      svs <- ((exp(rud_p_sim) * cons_p_sim) +
        (exp(rud_n_sim) * (cons_s_sim / cnt_s_sim))) / 1000
    }
    sv[j] <- stats::quantile(svs, probs = 0.9)
  }
  sv
}
