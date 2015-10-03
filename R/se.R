#' Standard Error of the Mean
#' 
#' This function computes the standard error of the mean of the values in
#' \code{x} using the formula \code{se = sd(x) / sqrt(length(x))}. If
#' \code{na.rm} is \code{TRUE} then missing values are removed before
#' computation proceeds.
#' 
#' @param x a numeric vector or an \R object which is coercible to one by
#'   \code{as.vector(x, "numeric")}
#' @param na.rm logical. Should missing values be removed?
#' @export
se <- function(x, na.rm = FALSE)
{
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else length(na.omit(x)))
}
