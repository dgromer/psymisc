#' Simulated height anxiety experiment.
#'
#' Simulated data from a height anxiety experiment in virtual reality. Three
#' groups of five participants each were immersed into a virtual height
#' environment (using CAVE (computer automatic virtual environment), HMD
#' (head-mounted display) and monitor) with three levels (5 m, 20 m, 50 m) and
#' rated their subjective anxiety level (0-100).
#'
#' @format A data frame with 45 rows and 4 variables:
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{group}{Between subject factor (CAVE vs. HMD vs. monitor)}
#'   \item{level}{Within subjects factor (5 m, 20 m, 50 m)}
#'   \item{anxiety}{Subjective anxiety rating}
#' }
"height"
