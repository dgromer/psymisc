#' Statistical table
#' 
#' Compiles a table with descriptive and inferential statistics to compare group
#' characteristics, e.g. control variables of a treatment group and a control
#' group.
#' 
#' @importFrom dplyr select
#' @importFrom purrr splice
#' @importFrom htmlTable htmlTable
#' @importFrom rmarkdown pandoc_convert
#' @param .data a data frame
#' @param iv name of the independent variable
#' @param ... names of the dependent variables
#' @param sig logical indicating whether to show significance with symbols in
#'   the last column
#' @param args a list with further arguments passed to functions. If the
#'   independent variable has two levels, then \code{args} can contain arguments
#'   passed to \link{t_test}, e.g. \code{var.equal = TRUE}.
#' @examples
#' stats_table(height, iv = group, age:aq_avoi)
#' stats_table(height, iv = group, age:aq_avoi, sig = TRUE, format = "html",
#'             args = list(var.equal = TRUE))
#' @export
stats_table <- function(.data, iv, ..., sig = FALSE,
                        format = c("default", "html"), args = list())
{
  format <- match.arg(format)
  
  # Convert iv to character if it is a name
  if (!is.character(substitute(iv)))
  {
    iv <- as.character(substitute(iv))
  }
  
  # Get a vector of the levels of the iv
  group_names <- as.character(unique(.data[[iv]]))

  # Get the column names of the dvs
  dvs <- names(select(.data, ...))
  
  header <- stats_table_header(group_names)
  
  # Calculate statistics for each dependent variable
  if (length(group_names) == 2)
  {
    rows <- lapply(dvs, function(dv)
      do.call("stats_table_row_ttest", splice(.data = .data, dv = dv, iv = iv,
                                              args)))
  }
  else
  {
    rows <- lapply(dvs, function(dv) stats_table_row_aov(.data, dv, iv))
  }
  
  rows <- lapply(rows, matrix, nrow = 1)
  tbl <- as.data.frame(Reduce(rbind, rows), stringsAsFactors = FALSE)
  names(tbl) <- header
  row.names(tbl) <- NULL
  
  # Show significance symbols in last column if sig is TRUE
  if (sig)
  {
    tbl$sig <- sapply(as.numeric(gsub(">|<", "", tbl$p)), p_to_symbol)
  }
  
  if (format == "html")
  {
    header <- gsub("_.*$", "", names(tbl))[-1]
    
    htmlTable(
      tbl[, -1],
      header = header,
      rnames = tbl[[1]],
      align = paste0("l", paste(rep("r", length(tbl) - 1), collapse = "")),
      cgroup = c(group_names, ""),
      n.cgroup = c(rep(2, length(group_names)), ifelse(sig, 4, 3))
    )
  }
  else
  {
    tbl
  }
}

stats_table_header <- function(iv)
{
  c("Variable", paste(c("M", "SD"), rep(iv, each = 2), sep = "_"),
    ifelse(length(iv) > 2, "F", "t"), "p",
    ifelse(length(iv) > 2, "petasq", "d"))
}


stats_table_row_aov <- function(.data, dv, iv, es = "petasq")
{
  stats <- stats_table_descriptives(.data, dv, iv, na.rm)
  
  test <- anova(aov(as.formula(paste(dv, "~", iv)), .data))

  f <- fmt_stat(test[["F value"]][1])
  p <- fmt_pval(test[["Pr(>F)"]][1], equal_sign = FALSE)
  petasq <- fmt_es(test[["Sum Sq"]][1] / sum(test[["Sum Sq"]]),
                   equal_sign = FALSE)
  
  c(dv, stats, f, p, petasq)
}

stats_table_row_ttest <- function(.data, dv, iv, ...)
{
  stats <- stats_table_descriptives(.data, dv, iv)
  
  test <- t_test(as.formula(paste(dv, iv, sep = " ~ ")), .data, ...)
  
  t <- fmt_stat(test$statistic)
  p <- fmt_pval(test$p.value, equal_sign = FALSE)
  d <- fmt_es(cohens_d(test), equal_sign = FALSE)
  
  c(dv, stats, t, p, d)
}

stats_table_descriptives <- function(.data, dv, iv)
{  
  m <- sapply(split(.data[[dv]], .data[[iv]]), mean, na.rm = TRUE)
  sd <- sapply(split(.data[[dv]], .data[[iv]]), sd, na.rm = TRUE)
  
  fmt_stat(c(rbind(m, sd)))
}

p_to_symbol <- function(p)
{
  if (p >= .1)
  {
    ""
  }
  else if (p < .1 && p >= .05)
  {
    "."
  }
  else if (p < .05 && p >= .01)
  {
    "*"
  }
  else if (p < .01 && p >= .001)
  {
    "**"
  }
  else if (p < .001)
  {
    "***"
  }
}
