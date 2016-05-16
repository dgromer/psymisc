#' Margin of Error
#'
#' @importFrom stats qt
#' @param x a numeric vector or an \R object which is coercible to one by
#'   \code{as.vector(x, "numeric")}
#' @param quantile numeric, quantile of the t-distribution. Default is
#'   \code{.975} for half the width of a 95\% confidence interval.
#' @param na.rm logical. Should missing values be removed?
#' @export
moe <- function(x, quantile = .975, na.rm = FALSE)
{
  # Calculate degrees of freedom as n - 1
  df <- if(!na.rm) length(x) - 1 else length(na.omit(x)) - 1

  se(x, na.rm) * qt(quantile, df)
}

#' Confidence Interval
#'
#' @param x a numeric vector or an \R object which is coercible to one by
#'   \code{as.vector(x, "numeric")}
#' @param quantile numeric, quantile of the t-distribution. Default is
#'   \code{.975} for a 95\% confidence interval.
#' @param na.rm logical. Should missing values be removed?
#' @export
ci <- function(x, quantile = .975, na.rm = FALSE)
{
  m <- mean(x, na.rm = na.rm)
  moe <- moe(x, quantile, na.rm)

  list(lower = m - moe, upper = m + moe)
}
