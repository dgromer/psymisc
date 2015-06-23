#' Recode variables
#'
#' @param x numeric or character vector with values to recode.
#' @param ... recode pairs, e.g. \code{"female" = "f"}.
#' @param default numeric or character. All entries in \code{x} not covered by
#'   \code{...} get recoded to \code{default}.
#' @param coerce a function that is applied to \code{x} after recoding, e.g.
#'   \code{as.numeric}.
#' @examples
#' recode(c("female", "male", "male", "female"), "female" = "f", "male" = "m")
#' 
#' recode(1:10, "1:5" = "a", "6:10" = "b")
#' 
#' recode(sample(letters[1:6]), "c('a', 'b', 'c')" = "abc",
#'        "c('d', 'e', 'f')" = "def")
#' @export
recode <- function(x, ..., default = NULL, coerce = NULL)
{
  r <- list(...)
  
  from <- names(r)
  to <- unlist(unname(r))
  
  for (i in seq_along(from))
  {
    # Check if "from" is a vector
    if (grepl("^c\\(.+\\)$", from[i]) || grepl("^[0-9]+\\:[0-9]+$", from[i]))
    {
      x[x %in% eval(parse(text = from[i]))] <- to[i]
    }
    else
    {
      x[x %in% from[i]] <- to[i]
    }
  }
  
  if (!is.null(default))
  {
    # Get indices of values that didn't get recoded so far
    def <- !Reduce(`|`, lapply(r, `==`, x))
    
    x[def] <- default
  }
  
  if (!is.null(coerce))
  {
    x <- do.call(coerce, list(x))
  }
  
  x
}
