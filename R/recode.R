#' Recode variables
#'
#' @importFrom stringr str_trim
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
#'   \item{Single value}{e.g. \code{"CTX" = "Context"}, \code{"3" = 5}}
#'   \item{Range of values}{e.g. \code{"1:10" = 5}, \code{"6:10" = "above
#'   five"}}
#'   \item{Vector of values}{e.g. \code{"c('A', 'B')" = "AB"} (note the use of
#'   single quotes inside the double quotes)}
#'   \item{Logical operation}{e.g. \code{"< 20" = "small"}, \code{"<= 10 | > 90"
#'   = "outlier"}, \code{"> 40 & < 60" = "normal"}}
#' }
#' @examples
#' recode(c("female", "male", "male", "female"), "female" = "f", "male" = "m")
#'
#' recode(1:10, "1:5" = "a", "6:10" = "b")
#' recode(1:20, "1:10" = 5, "11:20" = 15)
#'
#' recode(1:20, "< 3" = "small", ">= 3 & <= 8" = "medium", "> 8" = "large")
#'
#' recode(sample(letters[1:6]), "c('a', 'b', 'c')" = "abc",
#'        "c('d', 'e', 'f')" = "def")
#' @export
recode <- function(x, ..., default = NULL, coerce = NULL)
{
  # Put all recode pairs into a (named) list
  r <- list(...)

  # Separate old and new values
  from <- names(r)
  to <- unlist(unname(r))

  # Make a copy of 'x' that will be filled with new values so we don't run into
  # the problem of a numeric vector being changed to a character vector while
  # recoding and thus logical operations not working anymore
  xout <- x

  # Vector indicating which element in 'x' has already been recoded so a single
  # element is not changed twice
  recoded <- logical(length(x))

  for (i in seq_along(from))
  {
    # Find elements to be recoded
    # 'from' is a vector of values (e.g. c("a", "b") or 1:5)
    if (grepl("^c\\(.+\\)$", from[i]) || grepl("^[0-9]+\\:[0-9]+$", from[i]))
    {
      matches <- x %in% eval(parse(text = from[i]))
    }
    # 'from' is a logical operation (e.g. > 20 or > 5 & < 20)
    else if (grepl("^[<>]=?", from[i]))
    {
      # Add 'x' to the logical operation
      s <- paste("x", str_trim(from[i]))

      # Add 'x' a second time after "and" or "or" operator if present
      if (grepl("[&\\|]", s)) s <- gsub("^(.*[&\\|])(.*)$", "\\1x\\2", s)

      matches <- eval(parse(text = s))
    }
    # 'from' is a single value
    else
    {
      matches <- x %in% from[i]
    }

    # Recode matches
    xout[matches & !recoded] <- to[i]

    # Add recoded elements to 'recoded'
    recoded <- recoded | matches
  }

  if (!is.null(default))
  {
    # Set all elements that didn't get recoded so far to 'default'
    xout[!recoded] <- default
  }

  if (!is.null(coerce))
  {
    xout <- do.call(coerce, list(xout))
  }

  xout
}
