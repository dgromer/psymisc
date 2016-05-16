#' Display significance in ggplot2 plots
#'
#' A convenience function for displaying significance in ggplot2 plots
#'
#' @importFrom ggplot2 annotate
#' @importFrom magrittr %<>%
#' @param x_lo numeric, x position of left line ending
#' @param x_hi numeric, x position of right line ending
#' @param y_lo_left numeric, corresponding lower y position for \code{x_lo}
#' @param y_lo_right numeric, corresponding lower y position for \code{x_hi}
#' @param y_hi numeric, corresponding upper y-position for \code{x_lo} and
#'   \code{x_hi}
#' @param label character, text label to display, default is \code{"*"}
#' @param label_margin numeric, margin between \code{y_hi} and the text label,
#'   default is \code{.5}
#' @param text_size numeric, text size for label, default is \code{8}
#' @param line_size numeric, size of line elements, default is \code{.3}
#' @param x_lo_lo numeric, if the left line ending (\code{x_lo}) should span
#'   over multiple values, then x_lo_lo defines the leftmost position.
#' @param x_lo_hi numeric, corresponding rightmost value for \code{x_lo_lo}
#' @param x_hi_lo numeric, if the right line ending (\code{x_hi}) should span
#'   over multiple values, then x_hi_lo defines the leftmost position.
#' @param x_hi_hi numeric, corresponding rightmost value for \code{x_hi_lo}
#' @param span_y numeric, y length of the bracket for
#'   \code{x_lo_lo}/\code{x_lo_hi} and/or \code{x_hi_lo}/\code{x_hi_hi}
#' @param color character, color of label and line elements.
#' @param family character, font family
#' @examples
#' library(ggplot2)
#'
#' data <- data.frame(group = c("a", "b"), mean = c(20, 30), se = c(2.5, 3.0))
#'
#' ggplot(data, aes(x = group, y = mean)) +
#'   geom_bar(stat = "identity") +
#'   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .5) +
#'   plotsig(1, 2, 25, 35, 40)
#'
#' data <- data.frame(factor1 = rep(letters[1:2], each = 2),
#'                    factor2 = rep(letters[3:4], times = 2),
#'                    mean = c(10, 12, 20, 35),
#'                    se = c(2.4, 3, 2.9, 3.1))
#'
#' ggplot(data, aes(x = factor1, y = mean, fill = factor2)) +
#'   geom_bar(stat = "identity", position = "dodge") +
#'   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .5,
#'                 position = position_dodge(.9)) +
#'   plotsig(1.775, 2.225, 25, 40, 45) +
#'   plotsig(1, 2, 20, 50, 55, x_lo_lo = .55, x_lo_hi = 1.45, x_hi_lo = 1.55,
#'          x_hi_hi = 2.45)
#'
#' @export
plotsig <- function(x_lo, x_hi, y_lo_left, y_lo_right, y_hi, label = "*",
                    label_margin = .5, text_size = 8, line_size = .3,
                    x_lo_lo = NULL, x_lo_hi = NULL, x_hi_lo = NULL,
                    x_hi_hi = NULL, span_y = 1, color = "black", family = "")
{
  geoms <- list(
    annotate("segment", x = x_lo, xend = x_lo, y = y_lo_left, yend = y_hi,
             size = line_size, color = color),
    annotate("segment", x = x_lo, xend = x_hi, y = y_hi, yend = y_hi,
             size = line_size, color = color),
    annotate("segment", x = x_hi, xend = x_hi, y = y_hi, yend = y_lo_right,
             size = line_size, color = color),
    annotate("text", x = (x_lo + x_hi) / 2, y = y_hi + label_margin,
             label = label, size = text_size, color = color, family = family)
  )

  # Span across multiple value (left side)
  if (!is.null(x_lo_lo) && !is.null(x_lo_hi))
  {
    geoms %<>% c(
      annotate("segment", x = x_lo_lo, xend = x_lo_lo, y = y_lo_left - span_y,
               yend = y_lo_left, size = line_size, color = color),
      annotate("segment", x = x_lo_lo, xend = x_lo_hi, y = y_lo_left,
               yend = y_lo_left, size = line_size, color = color),
      annotate("segment", x = x_lo_hi, xend = x_lo_hi, y = y_lo_left,
               yend = y_lo_left - span_y, size = line_size, color = color)
    )
  }

  # Span across multiple values (right side)
  if (!is.null(x_hi_lo) && !is.null(x_hi_hi))
  {
    geoms %<>% c(
      annotate("segment", x = x_hi_lo, xend = x_hi_lo, y = y_lo_right - span_y,
               yend = y_lo_right, size = line_size, color = color),
      annotate("segment", x = x_hi_lo, xend = x_hi_hi, y = y_lo_right,
               yend = y_lo_right, size = line_size, color = color),
      annotate("segment", x = x_hi_hi, xend = x_hi_hi, y = y_lo_right,
               yend = y_lo_right - span_y, size = line_size, color = color)
    )
  }

  geoms
}
