
#' \code{shinyshval} package
#'
#' @docType package
#' @description Calculation of shval as shiny application
#' @name shinyshval
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")
    utils::globalVariables(c("value"))

#' Default values for shinyshval
#'
#' A dataset containing default values for consumptions & contents.
#'
#' @format A data frame with 24 rows and 10 variables:
#' \describe{
#'   \item{species}{species}
#'   \item{stadium}{stadium}
#'   \item{category}{category}
#'   \item{type}{type}
#'   \item{application}{application}
#'   \item{param}{param}
#'   \item{mean}{mean}
#'   \item{sd}{sd}
#'   \item{min}{min}
#'   \item{max}{max}
#'   \item{bg}{bg}
#' }
#' @examples
#' \dontrun{
#' # save default from csv to /data
#' defaults <- read.table(
#'   system.file("extdata/default_estimates.csv", package = "shinyshval"),
#'   header = TRUE, sep = ';')
#' defaults$min <- as.numeric(defaults$min)
#' defaults$max <- as.numeric(defaults$max)
#' # usethis::use_data(defaults, overwrite = TRUE)
#' }
"defaults"
