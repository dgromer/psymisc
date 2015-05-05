#' Student's t-Test
#' 
#' A wrapper for \code{t.test} that also returns sample sizes.
#' 
#' @inheritParams stats::t.test
#' @seealso \link{t.test}
#' 
#' @export
t_test <- function(x, ...) UseMethod("t_test")

#' @rdname t_test
#' @export
t_test.default <- function(x, y = NULL,
                           alternative = c("two.sided", "less", "greater"),
                           mu = 0, paired = FALSE, var.equal = FALSE,
                           conf.level = 0.95, ...)
{
  t <- t.test(x = x, y = y, alternative = alternative, mu = mu, paired = paired,
              var.equal = var.equal, conf.level = conf.level, ...)
  
  t$n <- c(length(x), ifelse(!is.null(y), length(y), integer(0)))
  
  t
}

#' @rdname t_test
#' @export
t_test.formula <- function(formula, data, subset, na.action, ...)
{ 
  t <- t.test(formula = formula, data = data, ...)
  
  t$n <- as.numeric(table(model.frame(formula, data)[2]))
  
  t
}