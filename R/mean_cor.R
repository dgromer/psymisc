#' Mean correlation using Fisher-Z-transformation
#' 
#' @param ... correlation coefficients to average (either as single numerics,
#'   a vector or a list)
#' @examples
#' mean_cor(.45, .33, .36)
#' 
#' @export
mean_cor <- function(...)
{
  dots <- list(...)
  
  tanh(mean(atanh(unlist(dots))))
}
