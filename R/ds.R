#' Descriptive statistics
#' 
#' A wrapper for dplyr's \code{group_by} and \code{summarise} for descriptive
#' statistics.
#' 
#' @import dplyr
#' @param .data a data frame containing the variables in the formula
#'   \code{formula}.
#' @param formula a formula in the form \code{lhs ~ rhs} where \code{lhs} is a
#'   numeric variable giving the data values and \code{rhs} one or more factors
#'   with two or more levels giving the corresponding groups or \code{.} if the
#'   parameters should be calculated over all cases.
#' @param funs character vector with function names indicating the parameters to
#'   calculate (default: \code{c("mean", "se")}).
#' @param names character vector with variable names for output data frame,
#'   defaults to \code{funs}.
#' @param ... further arguments passed to \code{funs}, e.g. \code{na.rm}
#' @examples 
#' ds(sleep, extra ~ group)
#' ds(sleep, extra ~ group, funs = c("median", "var", "n"))
#' @seealso \link{aggregate}
#' @export
ds <- function(.data, formula, funs = c("mean", "se"), names = funs, ...)
{
  vars <- all.vars(formula)
  
  args <- dots_to_character(...)
  
  if (vars[2] != ".")
  {
    .data <- group_by_(.data, .dots = vars[2:length(vars)])
  }
  
  dots <- paste0(funs, "(", vars[1], args, ")")
  dots[grep("^n(.*)", dots)] <- "n()"
  dots <- sapply(paste("~", dots), as.formula)
  
  summarise_(.data, .dots = setNames(dots, names))
}

dots_to_character <- function(...)
{
  args <- list(...)
  args <- paste(names(args), args, sep = " = ")
  args <- paste(args, collapse = ", ")
  
  if (args != "")
  {
    paste(",", args)
  }
  else
  {
    ""
  }
}
