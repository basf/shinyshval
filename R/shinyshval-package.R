
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
#' Values where extracted from tables J4-J8 of the guideline, as good as possible (see note).
#'
#' @note RUD values are given as mean +- sd on raw scale. However, for shval these numbers on
#'   log-scale are needed. For the upwards scenarious we used the mean and sd from the original data provided, 
#'   which fits to the guideline.
#'   For the sidewards scenarious the sd was not possible to exract and the sd from the raw-scale was used!
#'   This gives a slighly higher sd.
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
