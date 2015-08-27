#' Cohen's d
#' 
#' Calculate Cohen's d from raw data or a call to \code{t_test}/\code{t.test}.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y an optional (non-empty) numeric vector of data values.
#' @param paired a logical indicating whether Cohen's d should be calculated for
#'   a paired sample or two independent samples \emph{(default)}. Ignored when
#'   calculating Cohen's for one sample.
#' @param corr character specifying the correction applied to calculation of the
#'   effect size: \code{"none"} \emph{(default)} returns Cohen's d,
#'   \code{"hedges_g"} applies Hedges correction and \code{"glass_delta"}
#'   calculates Glass' \eqn{\Delta} (uses the standard deviation of the second
#'   group).
#' @param na.rm logical. Should missing values be removed?
#' @param data a data frame containing either the variables in the formula
#'   \code{formula} or the variables specified by \code{dv} and \code{iv}.
#' @param dv character indicating the name of the column in \code{data} for the
#'   dependent variable
#' @param iv character indicating the name of the column in \code{data} for the
#'   independent variable
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is a
#' numeric variable giving the data values (dependent variable) and \code{rhs} a
#' factor with two levels giving the corresponding groups (independent
#' variable).
#' @param ttest an object of class \code{htest} (a call to either \code{t_test}
#'   (preferred) or \code{t.test}).
#' @references
#'   Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#'   cumulative science: a practical primer for t-tests and ANOVAs.
#'   \emph{Frontiers in Psychology}, 4, 863. doi:10.3389/fpsyg.2013.00863
#' @examples
#' cohens_d(c(10, 15, 11, 14, 17), c(22, 18, 23, 25, 20))
#' 
#' # Methods when working with data frames
#' results <- data.frame(group = rep(letters[1:2], each = 5),
#'                       score = c(10, 15, 11, 14, 17, 22, 18, 23, 25, 20))
#' 
#' cohens_d(results, score, group)
#' # or
#' cohens_d(results, "score", "group")
#' # formula interface
#' cohens_d(score ~ group, results)
#' 
#' # Or pass a call to t_test or t.test
#' cohens_d(t_test(score ~ group, results))
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
    
    sd1 <- sd(x, na.rm)
    sd2 <- sd(y, na.rm)
    
    n1 <- ifelse(na.rm, length(na.omit(x)), length(x))
    n2 <- ifelse(na.rm, length(na.omit(y)), length(y))
    
    d <- cohens_d_(m1, m2, sd1, sd2, n1, n2, corr = corr)
  }
  else
  {
    if (is.null(y))
    {
      y <- 0
    }
    else
    {
      if (length(x) != length(y)) stop("'x' and 'y' must have the same length")
    }
    
    d <- mean(x - y, na.rm = na.rm) / sd(x - y, na.rm)
  }
  
  d
}

#' @rdname cohens_d
#' @export
cohens_d.data.frame <- function(data, dv, iv, paired = FALSE, corr = "none",
                                na.rm = FALSE)
{
  # Convert iv and dv to character if they are a name
  if (!is.character(substitute(iv))) iv <- as.character(substitute(iv))
  if (!is.character(substitute(dv))) dv <- as.character(substitute(dv))
  
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
  if (!grepl("t-test", ttest$method))
  {
    stop('ttest must be a call to either `t_test` or `t.test`')
  }
  
  if (!is.null(ttest[["data"]]))
  {
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
  else
  {
    if (grepl("Paired", ttest$method))
    {
      cohens_d_(t = unname(ttest$statistic), n = unname(ttest$parameter + 2),
                paired = TRUE)
    }
    else if (grepl("One Sample", ttest$method))
    {
      cohens_d_(t = unname(ttest$statistic), n = unname(ttest$parameter + 1),
                paired = TRUE)
    }
    else if (grepl("Welch", ttest$method))
    {
      stop(paste(
        "A Welch test from a call to `t.test` is not supported.",
        "Use either `t_test` or set argument 'var.equal' in `t.test` to TRUE"))
    }
    else
    {
      if (corr == "glass_delta")
      {
        stop(paste(
          "Glass Delta is not supported when passing a test from `t.test`.",
          "Use `t_test` instead."))
      }
      
      cohens_d_(t = unname(ttest$statistic), n = unname(ttest$parameter + 2),
                corr = corr)
    }
  }
}

#' Cohen's d
#' 
#' Calculate Cohens'd from different statistics (see Details).
#' 
#' @param m1 numeric, mean of the first group
#' @param m2 numeric, mean of the second group
#' @param sd1 numeric, standard deviation of the first group
#' @param sd2 numeric, standard deviation of the second group
#' @param n1 numeric, size of the first group
#' @param n2 numeric, size of the second group
#' @param t numeric, t-test statistic
#' @param n numeric, total sample size
#' @param paired logical indicating whether to calculate Cohen's for independent
#'   samples or one sample (\code{FALSE}, \emph{default}) or for dependent
#'   samples (\code{TRUE}).
#' @param corr character specifying the correction applied to calculation of the
#'   effect size: \code{"none"} \emph{(default)} returns Cohen's d,
#'   \code{"hedges_g"} applies Hedges correction and \code{"glass_delta"}
#'   calculates Glass' \eqn{\Delta} (uses the standard deviation of the second
#'   group).
#' @details
#'   The following combinations of statistics are possible:
#'   \itemize{
#'     \item \code{m1}, \code{m2}, \code{sd1}, \code{sd2}, \code{n1} and
#'     \code{n2}
#'     \item \code{t}, \code{n1} and \code{n2}
#'     \item \code{t} and \code{n}
#'   }
#' @references
#'   Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#'   cumulative science: a practical primer for t-tests and ANOVAs.
#'   \emph{Frontiers in Psychology}, 4, 863. doi:10.3389/fpsyg.2013.00863
#' @export
cohens_d_ <- function(m1 = NULL, m2 = NULL, sd1 = NULL, sd2 = NULL, n1 = NULL,
                      n2 = NULL, t = NULL, n = NULL, paired = FALSE,
                      corr = "none")
{
  if (!any(sapply(list(m1, m2, sd1, sd2, n1, n2), is.null)) &&
      corr != "glass_delta")
  {
    d <- (m1 - m2) /
      sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / ((n1 + n2) - 2))
    
    if (corr == "hedges_g")
    {
      j <- function(a) gamma(a / 2) / (sqrt(gamma(a / 2)) * gamma((a - 1) / 2))
      
      d <- d * j(n1 + n2 - 2)
    }
  }
  else if (corr == "glass_delta")
  {
    if (!any(sapply(list(m1, m2, sd2), is.null)))
    {
      d <- (m1 - m2) / sd2
    }
    else
    {
      stop("Arguments 'm1', 'm2' and 'sd2' are required for Glass Delta")
    }
  }
  else if (!any(sapply(list(n1, n2, t), is.null)))
  {
    d <- t * sqrt(1 / n1 + 1 / n2)
  }
  else if (!any(sapply(list(t, n), is.null)) && !paired)
  {
    d <- 2 * t * sqrt(n)
  }
  else if (!any(sapply(list(t, n), is.null)) && paired)
  {
    d <- t * sqrt(n)
  }
  
  d
}

#' Partial Eta Squared
#' 
#' @param x a call to \code{aov}, \code{ez::ezANOVA} or
#'   \code{afex::aov_ez}/\code{afex::aov_car}/\code{afex::aov_4}
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
  # afex
  else if (inherits(x, "afex_aov"))
  {
    petasq_afex(x, effect)
  }
  # ez::ezANOVA
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    petasq_ezanova(x, effect)
  }
}

petasq_aov <- function(x, effect)
{
  x <- anova(x)
  
  if (!effect %in% row.names(x))
  {
    stop("Specified effect not found")
  }
  
  petasq_(x[effect, "Sum Sq"], x["Residuals", "Sum Sq"])
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
      petasq <- petasq_(df[row, "Sum Sq"], df["Residuals", "Sum Sq"])
    }
  }
  
  petasq
}

petasq_afex <- function(x, effect)
{
  anova <- anova(x, es = "pes", intercept = TRUE)
  
  if (!effect %in% row.names(anova))
  {
    stop("Specified effect not found")
  }
  
  anova[effect, "pes"]
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
  
  petasq_(anova[row, "SSn"], anova[row, "SSd"])
}

#' Partial Eta Squared
#' 
#' Calculate the partial eta squared effect size from sum of
#' squares.
#' \deqn{\eta_p^2 = \frac{SS_effect}{SS_effect + SS_error}}{partial eta squared
#' = SS_effect / (SS_effect + SS_error)}
#' 
#' @param ss_effect numeric, sum of squares of the effect
#' @param ss_error numeric, sum of squares of the corresponding error
#' @export
petasq_ <- function(ss_effect, ss_error)
{
  ss_effect / (ss_effect + ss_error)
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
  else if (inherits(x, "afex_aov"))
  {
    getasq_afex(x, effect)
  }
  # ez::ezANOVA
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    getasq_ezanova(x, effect)
  }
}

getasq_afex <- function(x, effect)
{
  # afex drops the 'observed' argument when calling `anova` on the afex_aov
  # object, so we need to get the getasq values from $anova_table. The only
  # thing we can't retrieve is the getasq for the intercept ...
  if (effect == "(Intercept)")
  {
    return(NA)
  }
  
  anova <- x$anova_table
  
  if (!"ges" %in% names(anova))
  {
    stop("Argument 'es' needs to be set to \"ges\" in call to ´aov_*`")
  }
  
  if (!effect %in% row.names(anova))
  {
    stop("Specified effect not found")
  }
  
  anova[effect, "ges"]
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