#' helper function to parse original input files
#' @param file character; filepath
#' @export
#' @examples
#' parse_raw(system.file("extdata/original.csv", package = "shinyshval"))
parse_raw <- function(file) {
  df <- utils::read.table(file, header = TRUE, sep = ",")
  raw <- df[, 1:5]
  names(raw) <- c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s")

  est <- df[1:4, 6:11]
  est <- data.frame(t(est[, -1]))
  est$param <- c("RUD_p", "RUD_n", "CONS_p", "CONS_s",  "CNT_s")
  rownames(est) <- NULL
  names(est)[1:4] <- c("min", "max", "mean", "sd")
  est$bg <- ifelse(!is.na(est$mean) & is.na(est$sd), est$mean, NA)
  est$mean <- ifelse(!is.na(est$mean) & is.na(est$sd), NA, est$mean)
  est <- est[, c("param", "mean", "sd", "min", "max", "bg")]

  set <- df[1, 12:14]
  out <- list(raw = raw,
              est = est,
              set = set)
  return(out)
}
