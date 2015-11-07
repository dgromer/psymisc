#' Statistical table
#'
#' Compiles a table with descriptive and inferential statistics to compare group
#' characteristics, e.g. control variables of a treatment group and a control
#' group.
#'
#' @importFrom dplyr select
#' @importFrom purrr splice
#' @importFrom htmlTable htmlTable
#' @importFrom magrittr %<>%
#' @importFrom rmarkdown pandoc_convert
#' @param .data a data frame
#' @param iv name of the independent variable
#' @param ... names of the dependent variables
#' @param funs character vector with function names indicating the parameters to
#'   calculate (default: \code{c("mean", "sd")}).
#' @param sig logical indicating whether to show significance with symbols in
#'   the last column
#' @param format character string indicating the output format, one of
#'   \code{"text"}, \code{"html"} or \code{"latex"}. For HTML, the table is
#'   displayed as a htmlwidget. For LaTeX, the \code{tabularx} environment is
#'   used, which requires the tabularx package. The utilized \code{\\cmidrule}
#'   command requires either the booktabs or ctable package.
#' @param args a list with further arguments passed to functions. If the
#'   independent variable has two levels, then \code{args} can contain arguments
#'   passed to \link{t_test}, e.g. \code{var.equal = TRUE}.
#' @examples
#' stats_table(height, iv = group, age:aq_avoi)
#' stats_table(height, iv = group, age:aq_avoi, sig = TRUE, format = "html",
#'             args = list(var.equal = TRUE))
#' @export
stats_table <- function(.data, iv, ..., funs = c("mean", "sd"), sig = FALSE,
                        format = c("text", "html", "latex"), args = list())
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

  # TODO: replace funs names with M, SD, etc.
  header <- stats_table_header(group_names, funs, sig)

  # Calculate statistics for each dependent variable
  if (length(group_names) == 2)
  {
    rows <- lapply(dvs, function(dv)
      do.call("stats_table_row_ttest", splice(.data = .data, dv = dv, iv = iv,
                                              funs = funs, sig = sig, args)))
  }
  else
  {
    rows <- lapply(dvs, function(dv) stats_table_row_aov(.data, dv, iv, funs,
                                                         sig))
  }

  rows <- lapply(rows, matrix, nrow = 1)
  tbl <- as.data.frame(Reduce(rbind, rows), stringsAsFactors = FALSE)
  names(tbl) <- header
  row.names(tbl) <- NULL

  # All statistics get formatted with two decimal places. If n was requested,
  # reformat this with no decimal places
  if ("n" %in% funs)
  {
    tbl[, grep("^n_", names(tbl))] %<>% lapply(function(.) round(as.numeric(.)))
  }

  if (format == "html")
  {
    # Remove group names from statistics, e.g. "mean_group" to "mean", since
    # group names go to their own lines
    header <- gsub("_.*$", "", names(tbl))[-1]

    # Use htmlTable::htmlTable
    htmlTable(
      tbl[, -1],
      header = header,
      rnames = tbl[[1]],
      align = paste0("l", paste(rep("r", length(tbl) - 1), collapse = "")),
      cgroup = c(group_names, ""),
      n.cgroup = c(rep(2, length(group_names)), ifelse(sig, 4, 3))
    )
  }
  else if (format == "latex")
  {
    header <- gsub("_.*$", "", names(tbl))[-1]

    cat("\\begin{tabularx}{\\textwidth}{Xrrrrrrr}\n\\hline\n",
        paste0(" & \\multicolumn{2}{c}{", group_names, "}"), " & & & \\\\\n",
        "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\n",
        " & ", paste(paste0("\\textit{", header, "}"), collapse = " & "),
        " \\\\\n",
        tbl_to_latex(tbl), " \\\\\n",
        "\\hline\n\\end{tabularx}", sep = "")
  }
  else
  {
    tbl
  }
}

# Convert a data.frame or matrix to LaTeX rows and columns
#' @importFrom magrittr %>%
#' @importFrom purrr map2 map_if
tbl_to_latex <- function(x, colnames = FALSE, rownames = FALSE)
{
  # Separate columns with &
  tbl <- apply(x, 1, paste, collapse = " & ")

  if (rownames)
  {
    # Append row name to the front of each row
    tbl <- map2(rownames(x), tbl, function(.x, .y) paste(.x, .y, sep = " & "))
  }

  if (colnames)
  {
    # Add a row with column names
    tbl <- c(paste(colnames(x), collapse = " & "), "\\hline", tbl)

    # If row names were requested, add empty cell in front of column names row
    if (rownames) tbl[[1]] <- paste("&", tbl[[1]])
  }

  # Separate rows with \\
  tbl <- map_if(tbl, !grepl("\\hline", tbl), paste, "\\\\")

  paste(tbl, collapse = " \n")
}

#' @importFrom magrittr %<>%
stats_table_header <- function(iv, funs, sig)
{
  sig <- if (sig) "sig" else character(0)

  header <- c("Variable", paste(funs, rep(iv, each = length(funs)), sep = "_"),
              if(length(iv) > 2) "F" else "t", "p",
              if(length(iv) > 2) "petasq" else "d", sig)

  # Replace "mean" with "M", "sd" with "SD" and "se" with "SE"
  header %<>% gsub("^mean_", "M_", .)
  header %<>% gsub("^sd_", "SD_", .)
  header %<>% gsub("^se_", "SE_", .)

  header
}

stats_table_row_aov <- function(.data, dv, iv, es = "petasq", funs, sig)
{
  stats <- stats_table_descriptives(.data, dv, iv, funs)

  test <- anova(aov(as.formula(paste(dv, "~", iv)), .data))

  f <- test[["F value"]][1] %>% fmt_stat(equal_sign = FALSE)
  p <- test[["Pr(>F)"]][1] %>% fmt_pval(equal_sign = FALSE)
  es <- petasq(test, iv) %>% fmt_es(equal_sign = FALSE)
  sig <- if (sig) test[["Pr(>F)"]][1] %>% p_to_symbol() else character(0)

  c(dv, stats, f, p, es, sig)
}

#' @importFrom magrittr %>%
stats_table_row_ttest <- function(.data, dv, iv, funs, sig, ...)
{
  stats <- stats_table_descriptives(.data, dv, iv, funs)

  test <- t_test(as.formula(paste(dv, iv, sep = " ~ ")), .data, ...)

  t <- test$statistic %>% fmt_stat(equal_sign = FALSE)
  p <- test$p.value %>% fmt_pval(equal_sign = FALSE)
  d <- cohens_d(test) %>% fmt_es(equal_sign = FALSE)
  sig <- if (sig) test$p.value %>% p_to_symbol() else character(0)

  c(dv, stats, t, p, d, sig)
}

#' @importFrom magrittr %>%
stats_table_descriptives <- function(.data, dv, iv, funs)
{
  # Calculate descriptive statistics
  ds(.data, as.formula(paste(dv, iv, sep = " ~ ")), funs, na.rm = TRUE) %>%
    # Extract the statistics from the data frame
    (function(.x) .x[, funs]) %>%
    # Bring them into the correct order
    (function(.x) c(t(as.matrix(.x)))) %>%
    # Format values
    sapply(fmt_stat, equal_sign = FALSE)
}
