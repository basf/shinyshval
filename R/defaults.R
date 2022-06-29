#' Get default estimates for given parameters
#' 
#' Defaults where extracted from tables J4-J8 of the guideline, as good as possible (see note).
#'
#' @note RUD values are given as mean +- sd on raw scale. However, for shval these numbers on
#'   log-scale are needed. For the upwards scenarious we used the mean and sd from the original data provided, 
#'   which fits to the guideline.
#'   For the sidewards scenarious the sd was not possible to exract and the sd from the raw-scale was used!
#'   This gives a slighly higher sd.
#' 
#' @export
#' @param species character; species
#' @param stadium character; stadium
#' @param category character; category
#' @param type character; type
#' @param application character; application
#' @examples
#' default_estimates(species = "Honey Bee",
#'   stadium = "adult",
#'   category = "forager")
default_estimates <- function(species = c("Honey Bee", "Bumble Bee",
    "Solitary Bee"),
  stadium = c("adult", "larvae"),
  category = c("none", "forager", "nurse"),
  type = c("none", "chronic", "acute"),
  application = c("downwards", "up/sidewards")) {

  species <- match.arg(species)
  stadium <- match.arg(stadium)
  category <- match.arg(category)
  type  <- match.arg(type)
  application  <- match.arg(application)

  # capture eventual wrong inputs (from defaults of inputs)
  if (!(species == "Honey Bee" & stadium == "adult")) {
    category <- "none"
  }

  if (!(species %in% c("Honey Bee", "Bumble Bee") & stadium == "adult") |
      (species %in% c("Honey Bee") & category != "forager")) {
    type <- "none"
  }

  defaults <- shinyshval::defaults
  defaults[defaults$species %in% species & defaults$stadium %in% stadium &
    defaults$category %in% category & defaults$type %in% type &
    defaults$application %in% application,
    c("param", "mean", "sd", "min", "max", "bg")]
}
