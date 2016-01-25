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
#' @seealso \link{cor}
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

  # Calculate correlation matrix and format (no leading zero and two decimal
  # points)
  tbl <-
    cor(data, use = "pairwise") %>%
    apply(c(1, 2), fmt_stat, leading_zero = FALSE, equal_sign = FALSE) %>%
    format(width = 4, justify = "right")

  # Remove everything except the lower or upper triangular part of the matrix
  # using either the `lower.tri` or `upper.tri` function. Use whitespaces to
  # align text output
  tbl[!do.call(paste0(part, ".tri"), list(x = tbl))] <- "    "
  # Set the diagonale to dash, use whitespaces to align text output
  diag(tbl) <- "  - "

  # Calculate the corresponding p-values for the correlations
  p_values <-
    # Apply cor.test to every column in 'data' with every other column as ys
    lapply(data, function(.x) lapply(data, function(.y) cor.test(.x, .y))) %>%
    # Extract p-values from cor.test outputs
    at_depth(2, function(.x) .x$p.value) %>%
    unlist() %>%
    # Convert to matrix
    matrix(nrow = length(data))

  # Remove everything except the lower or upper triangular part of the p-value
  # matrix using either the `lower.tri` or `upper.tri` function
  p_values[!do.call(paste0(part, ".tri"), list(x = p_values))] <- NA

  # Apply adjustation for multiple comparisons to p-values if requested
  if (!is.null(adjust))
  {
    p_values[] <- p.adjust(p_values, method = adjust)
  }

  # Format p-values as asterisks and format all strings to the same width
  p_values %<>%
    apply(c(1, 2), p_to_symbol) %>%
    format(width = 3, justify = "left")

  # Concatenate correlations and significance asterisks
  tbl[] <- paste(tbl, p_values)

  # Convert to data.frame and set row and column names
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
    tbl[] <- lapply(tbl, trim_cor_table_cells)

    # Drop last column, because it takes unnecessary space in the console
    tbl[, -length(tbl)]
  }
}

# Subsequently delete whitespaces from the end of each element of a vector of
# strings, while ensuring that all strings stay the same length.
# E.g. c(" .39 ** ", "-.07    ") is transformed to c(" .39 **", "-.07   ")
trim_cor_table_cells <- function(x)
{
  # Check if all strings in 'x' end with a whitespace
  while(identical(grep(" $", x), seq_along(x)))
  {
    x <- substr(x, 1, nchar(x) - 1)
  }

  x
}
