#' Cohen's d
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y an optional (non-empty) numeric vector of data values.
#' @param paired a logical indicating whether you want Cohen's d for a paired
#'   sample or two samples.
#' @param na.rm logical. Should missing values be removed?
#' @param data a data frame containing either the variables in the formula
#'   \code{formula} or the variables specified by \code{dv} and \code{iv}.
#' @param dv character indicating the name of the column in \code{data} for the
#'   dependent variable
#' @param iv character indicating the name of the column in \code{data} for the
#'   independent variable
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is a
#' numeric variable giving the data values and \code{rhs} a factor with two
#' levels giving the corresponding groups.
#' @param ttest an object of class \code{htest} (a call to \code{t_test}).
#' @export
cohens_d <- function(x, ...) UseMethod("cohens_d")

#' @rdname cohens_d
#' @export
cohens_d.default <- function(x, y = NULL, paired = FALSE, na.rm = FALSE)
{
  if (!paired)
  {
    m1 <- mean(x, na.rm = na.rm)
    m2 <- mean(y, na.rm = na.rm)
    
    var1 <- var(x, na.rm = na.rm)
    var2 <- var(y, na.rm = na.rm)
    
    n1 <- ifelse(na.rm, length(na.omit(x)), length(x))
    n2 <- ifelse(na.rm, length(na.omit(y)), length(y))
    
    (m1 - m2) / sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / ((n1 + n2) - 2))
  }
  else
  {
    if (is.null(y))
    {
      y <- 0
    }
    
    mean(x - y) / sd(x - y)
  }
}

#' @rdname cohens_d
#' @export
cohens_d.data.frame <- function(data, dv, iv, na.rm = FALSE)
{
  sp <- split(data[[dv]], data[[iv]])
  
  cohens_d(sp[[1]], sp[[2]], na.rm = na.rm)
}

#' @rdname cohens_d
#' @export
cohens_d.formula <- function(formula, data, na.rm = FALSE)
{
  mf <- model.frame(formula, data)
  .data <- setNames(split(mf[[1]], mf[[2]]), c("x", "y"))
  
  do.call("cohens_d", .data)
}

#' @rdname cohens_d
#' @export
cohens_d.htest <- function(ttest)
{
  if (!grepl("t-test", ttest$method) && is.null(ttest[["data"]]))
  {
    stop("'ttest' must be a call to `t_test`")
  }
  
  if (grepl("Paired", ttest$method))
  {
    cohens_d(ttest$data$x, ttest$data$y, paired = TRUE)
  }
  else if (grepl("One Sample", ttest$method))
  {
    cohens_d(ttest$data$x)
  }
  else
  {
    cohens_d(ttest$data$x, ttest$data$y)
  }
}
