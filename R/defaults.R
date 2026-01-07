#' Get default estimates for given parameters.
#'
#' Defaults where extracted from tables J2 (sd) and J4-J8 of the guideline.
#'
#' @export
#' @param species character; species
#' @param stadium character; stadium
#' @param category character; category
#' @param type character; type
#' @param application character; application
#' @examples
#' default_estimates('Honey Bee', 'adult', 'forager')
default_estimates <- function(
  species = c("Honey Bee", "Bumble Bee", "Solitary Bee"),
  stadium = c("adult", "larvae"),
  category = c("none", "forager", "nurse"),
  type = c("none", "chronic", "acute"),
  application = c("downwards", "up/sidewards")
) {
  species <- match.arg(species)
  stadium <- match.arg(stadium)
  category <- match.arg(category)
  type <- match.arg(type)
  application <- match.arg(application)

  # capture eventual wrong inputs (from defaults of inputs)
  if (!(species == "Honey Bee" & stadium == "adult")) {
    category <- "none"
  }

  if (
    !(species %in% c("Honey Bee", "Bumble Bee") & stadium == "adult") |
      (species %in% c("Honey Bee") & category != "forager")
  ) {
    type <- "none"
  }

  defaults <- shinyshval::defaults
  defaults[
    defaults$species %in%
      species &
      defaults$stadium %in% stadium &
      defaults$category %in% category &
      defaults$type %in% type &
      defaults$application %in% application,
    c("param", "mean", "sd", "min", "max", "bg")
  ]
}
