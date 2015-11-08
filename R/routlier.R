#' Remove or flag outliers in a data frame
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @param .data a data frame
#' @param ... names of the columns to check for outliers
#' @param action character string indicating how to deal with outliers, can be
#'   one of \code{"na"} (outliers are turned into \code{NA}; default),
#'   \code{"remove"} (rows containing outliers are removed from the data frame)
#'   or \code{"flag"} (a new logical variable "outlier" is added to the data
#'   frame indicating which rows contain outliers).
#' @param z numeric vector of length two indicating the lower and upper
#'   z-score limits for outlier detection. If only one side should be scanned
#'   use e.g. \code{c(NA, 3)}.
#' @param absolute numeric vector of length two indicating the absolute lower
#'   and upper limits for outlier detection. If only one side should be scanned
#'   use e.g. \code{c(10, NA)}.
#' @param info logical indicating whether to print the number of outliers
#'   detected to the console.
#'
#' @export
routlier <- function(.data, ..., action = c("na", "flag", "remove"),
                     z = c(-3, 3), absolute = NULL, info = FALSE)
{
  action <- match.arg(action)

  x <- select(.data, ...)

  if (is.null(absolute))
  {
    # Apply outlier detection to z-scores
    outliers <-
      as.list(x) %>% # As list because, `scale` converts data frame to matrix
      map(scale) %>% # Apply z-transformation to all columns
      map(~ .x[, 1]) %>% # Retrieve transformed columns as vectors
      map(detect_outliers, z)
  }
  else
  {
    outliers <- map(x, detect_outliers, absolute)
  }

  if (info)
  {
    message(paste(sum(sapply(x, outliers)), "outlier(s) detected."))
  }

  if (action == "na")
  {
    for (s in names(outliers))
    {
      .data[outliers[[s]], s] <- NA
    }
  }
  else if (action == "flag")
  {
    .data$outlier <- Reduce(`|`, outliers)
  }
  else if (action == "remove")
  {
    .data <- .data[!Reduce(`|`, outliers), , drop = FALSE]
  }

  .data
}

detect_outliers <- function(x, borders)
{
  if (!is.na(borders[1]))
  {
    lower <- x < borders[1]
  }
  else
  {
    lower <- rep(FALSE, length(x))
  }

  if (!is.na(borders[2]))
  {
    upper <- x > borders[2]
  }
  else
  {
    upper <- rep(FALSE, length(x))
  }

  lower | upper
}
