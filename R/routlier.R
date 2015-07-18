#' Remove or flag outliers in a data frame
#' 
#' @importFrom dplyr select
#' @param .data a data frame
#' @param ... names of the columns to check for outliers
#' @param action character string indicating how to deal with outliers, can be
#'   one of \code{"na"} (outliers are turned into \code{NA}; default),
#'   \code{"remove"} (rows containing outliers are removed from the data frame)
#'   or \code{"flag"} (a new logical variable "outlier" is added to the data
#'   frame indicating which rows contain outliers).
#' @param z numeric vector of length two indicating the lower and upper
#'   z-score limits for outlier detection. If only one side should be scanned
#'   use e.g. \code{c(NULL, 3)}.
#' @param absolute numeric vector of length two indicating the absolute lower
#'   and upper limits for outlier detection. If only one side should be scanned
#'   use e.g. \code{c(10, NULL)}.
#' @param info
#' 
#' @export
routlier <- function(.data, ..., action = "na", z = c(-3, 3), absolute = NULL,
                     info = FALSE)
{
  x <- select(.data, ...)
  
  if (is.null(absolute))
  {
    outliers <- lapply(scale(x), detect_outliers, z)
  }
  else
  {
    outliers <- lapply(x, detect_outliers, absolute)
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
  else if (action == "remove" || action == "rm")
  {
    .data <- .data[!Reduce(`|`, outliers), , drop = FALSE]
  }
  
  .data
}

detect_outliers <- function(x, borders)
{
  if (!is.null(borders[1]))
  {
    lower <- sapply(x, function(.) . < borders[1])
  }
  else
  {
    lower <- rep(FALSE, length(x))
  }
  
  if (!is.null(borders[2]))
  {
    upper <- sapply(x, function(.) . > borders[2])
  }
  else
  {
    upper <- rep(FALSE, length(x))
  }
  
  lower | upper
}
