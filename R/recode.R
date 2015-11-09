#' Recode variables
#'
#' @param x numeric or character vector with values to recode.
#' @param ... recode pairs in the format \code{"from" = "to"}, e.g.
#'   \code{"female" = "f"}.
#' @param default numeric or character. All entries in \code{x} not covered by
#'   \code{...} get recoded to \code{default}.
#' @param coerce a function that is applied to \code{x} after recoding, e.g.
#'   \code{as.numeric}.
#' @details
#' The left hand side of the recode pairs in \code{...} can be one of:
#' \describe{
#'   \item{Single value}{e.g. \code{"CTX" = "Context"}}
#'   \item{Range of values}{e.g. \code{"1:10" = 5}}
#'   \item{Vector of values}{e.g. \code{"c('A', 'B')" = "AB"}}
#' }
#' @examples
#' recode(c("female", "male", "male", "female"), "female" = "f", "male" = "m")
#'
#' recode(1:10, "1:5" = "a", "6:10" = "b")
#' recode(1:20, "1:10" = 5, "11:20" = 15)
#'
#' recode(sample(letters[1:6]), "c('a', 'b', 'c')" = "abc",
#'        "c('d', 'e', 'f')" = "def")
#' @export
recode <- function(x, ..., default = NULL, coerce = NULL)
{
  r <- list(...)

  from <- names(r)
  to <- unlist(unname(r))

  # Vector indicating which element in x has already been recoded so a single
  # element is not changed twice
  recoded <- logical(length(x))

  for (i in seq_along(from))
  {
    # Check if 'from' is a vector
    if (grepl("^c\\(.+\\)$", from[i]) || grepl("^[0-9]+\\:[0-9]+$", from[i]))
    {
      matches <- x %in% eval(parse(text = from[i]))
    }
    else
    {
      matches <- x %in% from[i]
    }

    # Recode matches
    x[matches & !recoded] <- to[i]

    # Add recoded elements to recoded
    recoded <- recoded | matches
  }

  if (!is.null(default))
  {
    # Set all elements that didn't get recoded so far to 'default'
    x[!recoded] <- default
  }

  if (!is.null(coerce))
  {
    x <- do.call(coerce, list(x))
  }

  x
}
