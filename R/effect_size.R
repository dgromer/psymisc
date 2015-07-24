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
cohens_d.default <- function(x, y = NULL, paired = FALSE, corr = "none",
                             na.rm = FALSE)
{
  if (!paired && !is.null(y))
  {
    m1 <- mean(x, na.rm = na.rm)
    m2 <- mean(y, na.rm = na.rm)
    
    var1 <- var(x, na.rm = na.rm)
    var2 <- var(y, na.rm = na.rm)
    
    n1 <- ifelse(na.rm, length(na.omit(x)), length(x))
    n2 <- ifelse(na.rm, length(na.omit(y)), length(y))
    
    d <- (m1 - m2) / sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / ((n1 + n2) - 2))
    
    if (corr %in% c("hedges_g", "g"))
    {
      d <- d * (1 - 3 / (4 * (n1 + n2) - 9))
    }
    else if (corr %in% c("glass_delta", "delta"))
    {
      d <- (m1 - m2) / sqrt(var2)
    }
  }
  else
  {
    if (is.null(y))
    {
      y <- 0
    }
    
    d <- mean(x - y) / sd(x - y)
  }
  
  d
}

#' @rdname cohens_d
#' @export
cohens_d.data.frame <- function(data, dv, iv, paired = FALSE, corr = "none",
                                na.rm = FALSE)
{
  sp <- split(data[[dv]], data[[iv]])
  
  cohens_d(sp[[1]], sp[[2]], paired, corr, na.rm)
}

#' @rdname cohens_d
#' @export
cohens_d.formula <- function(formula, data, paired = FALSE, corr = "none",
                             na.rm = FALSE)
{
  mf <- model.frame(formula, data)
  .data <- setNames(split(mf[[1]], mf[[2]]), c("x", "y"))
  
  do.call("cohens_d", c(.data, paired = paired, corr = corr, na.rm = na.rm))
}

#' @rdname cohens_d
#' @export
cohens_d.htest <- function(ttest, corr = "none")
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
    cohens_d(ttest$data$x, ttest$data$y, corr = corr)
  }
}

#' Partial Eta Squared
#' 
#' @param x a call to \code{aov} or \code{ez::ezANOVA}
#' @param effect character string indicating the effect of interest
#' @export
petasq <- function(x, effect)
{
  # aov
  if (inherits(x, "aov"))
  {
    petasq_aov(x, effect)
  }
  # aovlist
  if (inherits(x, "aovlist"))
  {
    petasq_aovlist(x, effect)
  }
  # ez::ezANOVA
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    petasq_ezanova(x, effect)
  }
  # afex
  else if (is.list(x) && names(x)[1:3] == c("Anova", "lm", "data"))
  {
    petasq_afex(x, effect)
  }
}

petasq_aov <- function(x, effect)
{
  x <- anova(x)
  
  if (!effect %in% row.names(x))
  {
    stop("Specified effect not found")
  }
  
  x[effect, "Sum Sq"] / (x[effect, "Sum Sq"] + x["Residuals", "Sum Sq"])
}

#' @importFrom purrr flatten
#' @importFrom stringr str_trim
petasq_aovlist <- function(x, effect)
{
  if (!effect %in% attr(x$`(Intercept)`$terms, "term.labels"))
  {
    stop("Specified effect not found")
  }
  
  # summary.aovlist is a list of lists containing data frames
  x <- flatten(summary(x))
  
  # Look through data frames for specified effect
  for (i in seq_along(x))
  {
    df <- x[[i]]
    
    row <- which(str_trim(row.names(df)) == effect)
    
    if (length(row) > 0)
    {
      petasq <-
        df[row, "Sum Sq"] / (df[row, "Sum Sq"] + df["Residuals", "Sum Sq"])
    }
  }
  
  petasq
}

petasq_ezanova <- function(x, effect)
{
  anova <- x$ANOVA
  
  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }
  
  if (!effect %in% anova$Effect)
  {
    stop("Specified effect not found")
  }
  else
  {
    row <- which(anova$Effect == effect)
  }
  
  anova[row, "SSn"] / (anova[row, "SSn"] + anova[row, "SSd"])
}

petasq_afex <- function(x, effect)
{
  anova <- x$Anova
  
  
}

getasq <- function(x, effect)
{
  # aov
  if (inherits(x, "aov"))
  {
    getasq_anova(anova(x), effect)
  }
  # anova
  else if (inherits(x, "anova"))
  {
    getasq_anova(x, effect)
  }
  # ez::ezANOVA
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    getasq_ezanova(x, effect)
  }
  # afex
  else if (is.list(x) && names(x)[1:3] == c("Anova", "lm", "data"))
  {
    getasq_afex(x, effect)
  }
}

getasq_ezanova <- function(x, effect)
{
  anova <- x$ANOVA
  
  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }
  
  if (!effect %in% anova$Effect)
  {
    stop("Specified effect not found")
  }
  
  anova[which(anova$Effect == effect), "ges"]
}