#' fplot
#'
#' Convenient plotting of means and errors bars or boxplots of factorial
#' data.
#'
#' @importFrom dplyr left_join rename_
#' @importFrom magrittr %<>% %>%
#' @importFrom tidyr complete_
#' @param .data a data frame containing the variables in the formula
#'   \code{formula}.
#' @param formula a formula in the form \code{lhs ~ rhs} where \code{lhs} is a
#'   numeric variable (the dependent variable) and \code{rhs} one or more
#'   factors with two or more levels giving the corresponding groups.
#' @param geom character string indicating the ggplot2 geom used. One of
#'   \code{"bar"} (default), \code{"line"} and \code{"boxplot"}.
#' @param error character string indicating the function used for calculating
#'   the error bars. One of \code{"se"} (default) \code{"sd"} or \code{"moe"}.
#'   Ignored if \code{geom} is \code{"boxplot"}.
#' @param ... Further arguments passed to methods. E.g. \code{quantile} if
#'   \code{error} is \code{"moe"} to indicate the quantile for the confidence
#'   interval.
#' @examples
#' fplot(height, stai_trait ~ group + sex)
#'
#' # Standard deviation instead of standard error
#' fplot(height, stai_trait ~ group + sex, error = "sd")
#'
#' # 95% confidence intervals (moe = margin of error)
#' fplot(height, stai_trait ~ group + sex, error = "moe")
#'
#' # Line graph
#' fplot(height, stai_trait ~ group + sex, geom = "line")
#'
#' # Boxplot
#' fplot(height, stai_trait ~ group + sex, geom = "boxplot")
#'
#' @export
fplot <- function(.data, formula, geom = c("bar", "line", "boxplot"),
                  error = c("se", "sd", "moe"), ...)
{
  geom <- match.arg(geom)
  error <- match.arg(error)

  # Get dependent variable
  dv <- all.vars(formula)[1]

  # Get independent variables
  vars <- all.vars(formula)[-1]

  if (geom == "boxplot")
  {
    return(fplot_boxplot(.data, dv, vars))
  }

  descr <-
    # Calculate means and error bars
    ds(.data, formula, c("mean", error), na.rm = TRUE, ...) %>%
    # Rename column indicating error to "error", since it can be either "se",
    # "sd" or "moe".
    rename_(.dots = setNames(error, "error")) %>%
    # Keep empty groups in descr
    complete_(vars, fill = list())

  # Ensure that independent variables are factors
  descr[1:length(vars)] %<>% lapply(as.factor)

  do.call(paste0("fplot_", geom), list(x = descr, dv = dv, vars = vars))
}

#' @import ggplot2
fplot_bar <- function(x, dv, vars)
{
  if (length(vars) == 1)
  {
    ggplot(x, aes_string(x = vars, y = "mean")) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2) +
      theme_classic() +
      labs(y = dv)
  }
  else if (length(vars) == 2)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", fill = vars[2])) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2,
                    position = position_dodge(.9)) +
      theme_classic() +
      labs(y = dv)

  }
  else if (length(vars) == 3)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", fill = vars[2])) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2,
                    position = position_dodge(.9)) +
      facet_grid(as.formula(paste(". ~", vars[3]))) +
      theme_bw() +
      labs(y = dv)
  }
  else if (length(vars) == 4)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", fill = vars[2])) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2,
                    position = position_dodge(.9)) +
      facet_grid(as.formula(paste(vars[4], "~", vars[3]))) +
      theme_bw() +
      labs(y = dv)
  }
  else
  {
    stop("More than four factors are not supported")
  }
}

#' @import ggplot2
fplot_line <- function(x, dv, vars)
{
  if (length(vars) == 1)
  {
    ggplot(x, aes_string(x = vars, y = "mean")) +
      geom_point(na.rm = TRUE) +
      geom_line(aes(group = 1), na.rm = TRUE) +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2,
                    na.rm = TRUE) +
      theme_classic() +
      labs(y = dv)
  }
  else if (length(vars) == 2)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", group = vars[2],
                         color = vars[2])) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2,
                    na.rm = TRUE) +
      theme_classic() +
      labs(y = dv)

  }
  else if (length(vars) == 3)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", group = vars[2],
                         color = vars[2])) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2,
                    na.rm = TRUE) +
      facet_grid(as.formula(paste(". ~", vars[3]))) +
      theme_bw() +
      labs(y = dv)
  }
  else if (length(vars) == 4)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", group = vars[2],
                         color = vars[2])) +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) +
      geom_errorbar(aes(ymin = mean - error, ymax = mean + error), width = .2,
                    na.rm = TRUE) +
      facet_grid(as.formula(paste(vars[4], "~", vars[3]))) +
      theme_bw() +
      labs(y = dv)
  }
  else
  {
    stop("More than four factors are not supported")
  }
}

#' @import ggplot2
fplot_boxplot <- function(x, dv, vars)
{
  # Convert vars to factors
  x[vars] <- lapply(x[vars], as.factor)

  if (length(vars) == 1)
  {
    ggplot(x, aes_string(x = vars, y = dv)) +
      geom_boxplot() +
      theme_classic() +
      labs(y = dv)
  }
  else if (length(vars) == 2)
  {
    ggplot(x, aes_string(x = vars[1], y = dv, fill = vars[2])) +
      geom_boxplot() +
      theme_classic() +
      labs(y = dv)

  }
  else if (length(vars) == 3)
  {
    ggplot(x, aes_string(x = vars[1], y = dv, fill = vars[2])) +
      geom_boxplot() +
      facet_grid(as.formula(paste(". ~", vars[3]))) +
      theme_bw() +
      labs(y = dv)
  }
  else if (length(vars) == 4)
  {
    ggplot(x, aes_string(x = vars[1], y = dv, fill = vars[2])) +
      geom_boxplot() +
      facet_grid(as.formula(paste(vars[4], "~", vars[3]))) +
      theme_bw() +
      labs(y = dv)
  }
  else
  {
    stop("More than four factors are not supported")
  }
}
