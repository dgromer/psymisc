#' fplot
#'
#' Convenient plotting of means and standard errors or boxplots of factorial
#' data.
#'
#' @importFrom dplyr left_join
#' @importFrom magrittr %<>%
#' @importFrom tidyr complete_
#' @param .data a data frame containing the variables in the formula
#'   \code{formula}.
#' @param formula a formula in the form \code{lhs ~ rhs} where \code{lhs} is
#'   the a numeric variable (dependent variable) and \code{rhs} one or more
#'   factors with two or more levels giving the corresponding groups.
#' @param geom character indicating the ggplot2 geom used. One of \code{"bar"}
#'   (default), \code{"line"} and \code{"boxplot"}.
#'
#' @export
fplot <- function(.data, formula, geom = c("bar", "line", "boxplot"))
{
  geom <- match.arg(geom)

  # Get dependent variable
  dv <- all.vars(formula)[1]

  # Get independent variables
  vars <- all.vars(formula)[-1]

  if (geom == "boxplot")
  {
    return(fplot_boxplot(.data, dv, vars))
  }

  # Calculate means and standard errors
  descr <- ds(.data, formula, na.rm = TRUE)

  # Keep empty groups in descr
  descr <- complete_(descr, vars, fill = list())

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
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2) +
      theme_classic() +
      labs(y = dv)
  }
  else if (length(vars) == 2)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", fill = vars[2])) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2,
                    position = position_dodge(.9)) +
      theme_classic() +
      labs(y = dv)

  }
  else if (length(vars) == 3)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", fill = vars[2])) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2,
                    position = position_dodge(.9)) +
      facet_grid(as.formula(paste(". ~", vars[3]))) +
      theme_bw() +
      labs(y = dv)
  }
  else if (length(vars) == 4)
  {
    ggplot(x, aes_string(x = vars[1], y = "mean", fill = vars[2])) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2,
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
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2,
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
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2,
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
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2,
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
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2,
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
