#' fplot
#'
#' Convenient plotting of means and error bars or boxplots of factorial
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
#' fplot(hquest, trait_anx ~ group + gender)
#'
#' # Standard deviation instead of standard error
#' fplot(hquest, trait_anx ~ group + gender, error = "sd")
#'
#' # 95% confidence intervals (moe = margin of error)
#' fplot(hquest, trait_anx ~ group + gender, error = "moe")
#'
#' # Line graph
#' fplot(hquest, trait_anx ~ group + gender, geom = "line")
#'
#' # Boxplot
#' fplot(hquest, trait_anx ~ group + gender, geom = "boxplot")
#'
#' @export
fplot <- function(.data, formula, geom = c("bar", "line", "boxplot"),
                  error = c("se", "sd", "moe"), ...)
{
  geom <- match.arg(geom)
  error <- match.arg(error)

  # Extract dependent variable from formula
  dv <- all.vars(formula)[1]

  # Extract independent variables from formula
  vars <- all.vars(formula)[-1]

  # Call `fplot_boxplot` if boxplot was requested since calcuation of the
  # statistics is handled by ggplot2
  if (geom == "boxplot")
  {
    return(fplot_boxplot(.data, dv, vars))
  }

  # Calculate the descriptive statistics to be displayed in the plot
  descr <-
    # Calculate means and errors
    ds(.data, formula, c("mean", error), na.rm = TRUE, ...) %>%
    # Rename column indicating error to "error", since it can be either "se",
    # "sd" or "moe".
    rename_(.dots = setNames(error, "error")) %>%
    # Keep empty groups in descr
    complete_(vars)

  # Ensure that independent variables are factors for propper plotting in
  # ggplot2
  descr[1:length(vars)] %<>% lapply(as.factor)

  # Call either `fplot_bar` or `fplot_line`
  do.call(paste0("fplot_", geom), list(x = descr, dv = dv, vars = vars))
}

#' @importFrom ggplot2 aes aes_string facet_grid geom_bar geom_errorbar ggplot
#'   labs position_dodge theme_bw theme_classic
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

#' @importFrom ggplot2 aes aes_string facet_grid geom_errorbar geom_line
#'   geom_point ggplot labs theme_bw theme_classic
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

#' @importFrom ggplot2 aes_string facet_grid geom_boxplot ggplot labs theme_bw
#'   theme_classic
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
