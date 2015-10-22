#' Correlation table
#'
#' @importFrom knitr kable
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr at_depth
#' @importFrom stringr str_trim
#' @param data a data frame
#' @param adjust character string indicating the method used for adjusting
#'   p-values for muliple comparisons. See \link{p.adjust.methods} for available
#'   methods. If is \code{NULL}, no adjustment is applied (default).
#' @param labels character vector indicating the labels used for header row and
#'   first column
#' @param format character string indicating the output format, one of
#'   \code{"text"}, \code{"html"} or \code{"latex"}. For HTML, the table is
#'   displayed as a htmlwidget. For LaTeX, the \code{tabularx} environment is
#'   used, which requires the tabularx package.
#' @param part character string indicating which part of the correlation matrix
#'   should be printed, can be either \code{"lower"} or \code{"upper"}.
#' @examples
#' cor_table(height[4:9])
#' @export
cor_table <- function(data, adjust = NULL, labels = names(data),
                      format = c("text", "html", "latex", "markdown",
                                 "rmarkdown"),
                      part = c("lower", "upper"))
{
  format <- match.arg(format)
  part <- match.arg(part)

  # Calculate p-values
  p_values <-
    # Apply cor.test to every column in 'data' with every other column as ys
    lapply(data, function(.x) lapply(data, function(.y) cor.test(.x, .y))) %>%
    # Extract p-values from cor.test outputs
    at_depth(2, function(.x) .x$p.value) %>%
    unlist() %>%
    # Convert to matrix
    matrix(nrow = length(data))

  # Remove everything except the lower or upper triangular part using either the
  # lower.tri or upper.tri function
  p_values[!do.call(paste0(part, ".tri"), list(x = p_values))] <- NA

  # Apply adjustation for multiple comparisons if requested
  if (!is.null(adjust))
  {
    p_values[] <- p.adjust(p_values, method = adjust)
  }

  # Format p-values as asterisks and format strings with same width
  p_values %<>%
    apply(c(1, 2), p_to_symbol) %>%
    format(width = 3, justify = "right")

  # Calculate correlations, convert to matrix and format
  tbl <-
    cor(data, use = "pairwise") %>%
    apply(c(1, 2), fmt_stat, leading_zero = FALSE, equal_sign = FALSE) %>%
    format(width = 4, justify = "right")

  # Remove everything except the lower or upper triangular part using either the
  # lower.tri or upper.tri function. Use whitespaces to align text output
  tbl[!do.call(paste0(part, ".tri"), list(x = tbl))] <- "    "
  # Set the diagonale to dash, use whitespaces to align text output
  diag(tbl) <- "  - "

  # Concat correlation and significance asterisks
  tbl[] <- paste(tbl, p_values)

  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  rownames(tbl) <- labels
  colnames(tbl) <- labels

  if (format == "html")
  {
    htmlTable(tbl, header = labels, rnames = labels)
  }
  else if (format == "latex")
  {
    # Trim whitespaces to get more elegant LaTeX code
    tbl[] <- lapply(tbl, str_trim)

    # Build LaTeX table and cat
    cat("\\begin{tabularx}{\\textwidth}{", "X", rep("c", length(tbl)),
        "}\n\\hline\n",
        tbl_to_latex(tbl, rownames = TRUE, colnames = TRUE), "\n",
        "\\hline\n\\end{tabularx}", sep = "")
  }
  else if (format == "markdown" || format == "rmarkdown")
  {
    tbl[] <- lapply(tbl, str_trim)

    # Escape asterisks
    tbl[] <- lapply(tbl, function(.x) gsub("\\*", "\\\\*", .x))

    # Build markdown table and cat
    kable(tbl, format = "markdown", row.names = TRUE,
          col.names = colnames(tbl), rep("c", length(tbl)))
  }
  else
  {
    tbl
  }
}