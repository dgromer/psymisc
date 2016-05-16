#' Statistical table
#'
#' Compiles a table with descriptive and inferential statistics to compare group
#' characteristics, e.g. control variables of a treatment group and a control
#' group.
#'
#' @importFrom dplyr as_data_frame select_ bind_rows
#' @importFrom purrr map
#' @importFrom htmlTable htmlTable
#' @importFrom magrittr %<>%
#' @importFrom rmarkdown pandoc_convert
#' @param .data a data frame
#' @param iv name of the independent variable
#' @param dvs names of the dependent variables
#' @param funs character vector with function names indicating the parameters to
#'   calculate (default: \code{c("mean", "sd")}).
#' @param paired logical indicating whether you have paired data (\code{iv} is a
#'   within factor) or non-paired data (\code{iv} is a between factor).
#' @param id name or character string indicating the subject identifier column
#'   in \code{.data}. Only required if the independent variable has more than
#'   two levels.
#' @param sig logical indicating whether to show significance with symbols in
#'   the last column
#' @param format character string indicating the output format, one of
#'   \code{"text"}, \code{"html"} or \code{"latex"}. For HTML, the table is
#'   displayed as a htmlwidget. For LaTeX, the \code{tabularx} environment is
#'   used, which requires the tabularx package. The utilized \code{\\cmidrule}
#'   command requires either the booktabs or ctable package.
#' @param ... Further arguments passed to functions. If the independent variable
#'   has two levels, then \code{...} can contain arguments passed to
#'   \link{t_test}, e.g. \code{var.equal = TRUE}.
#' @examples
#' stats_table(hquest, iv = group, dvs = age:sens_seek)
#' stats_table(hquest, iv = group, dvs = age:sens_seek, sig = TRUE,
#'             format = "html", var.equal = TRUE)
#' @export
stats_table <- function(.data, iv, dvs, funs = c("mean", "sd"), paired = FALSE,
                        id, sig = FALSE,
                        format = c("text", "html", "latex"), ...)
{
  # TODO: parameter for overwriting variable names for printing
  # TODO: parameter for overwriting group names for printing

  format <- match.arg(format)

  # Convert iv to character if it is a name
  if (!is.character(substitute(iv))) iv <- as.character(substitute(iv))

  # Get a vector with the levels of the iv
  group_names <- levels(factor(.data[[iv]]))

  # Get column names of the dvs
  dvs <- names(select_(.data, .dots = substitute(dvs)))

  # Build header (column names of the output table)
  header <- stats_table_header(group_names, funs, sig)

  # Calculate statistics for each dependent variable
  rows <- map(dvs, ~ stats_table_row(header, group_names, .data, .x, iv, funs,
                                     paired, id, sig, ...))

  tbl <- bind_rows(rows)

  # All statistics get formatted with two decimal places. If n was requested,
  # reformat this with no decimal places
  if ("n" %in% funs)
  {
    tbl[, grep("^n_", names(tbl))] %<>% map(~ round(as.numeric(.x)))
  }

  if (format == "html")
  {
    # Remove group names from statistics, e.g. "mean_group" to "mean", since
    # group names go to their own lines
    header <- gsub("_.*$", "", names(tbl))[-1]

    tbl[] <- map(tbl, ~ sub("<(.*)", "&lt;\\1", .x))

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

    # TODO: replace _ with \_
    # TODO: make stats_table_latex function? (similar for html)
    # TODO: center parameter row with \cmidrule{1}{c}{text} for each element
    # TODO: option for fill with whitespaces for better alignment

    cat("\\begin{tabularx}{\\textwidth}{Xrrrrrrr}\n\\hline\n",
        paste0(" & \\multicolumn{2}{c}{", group_names, "}"), " & & & \\\\\n",
        "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\n",
        " & ", paste(paste0("\\textit{", header, "}"), collapse = " & "),
        " \\\\\n",
        tbl_to_latex(tbl), " \\\n",
        "\\hline\n\\end{tabularx}", sep = "")
  }
  else
  {
    # Return as data.frame instead of tbl_df for cleaner printing
    as.data.frame(tbl)
  }
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

#' @importFrom afex aov_ez
#' @importFrom dplyr last
#' @importFrom magrittr %>%
#' @importFrom stats as.formula setNames
#' @importFrom stringr str_extract str_extract_all
stats_table_row <- function(header, group_names, .data, dv, iv, funs, paired,
                            id, sig, ...)
{
  # Compute descriptive statistics
  descriptives <- stats_table_descriptives(.data, dv, iv, funs)

  # Run t-test if independent variable has two levels
  if (length(group_names) == 2)
  {
    test <-
      t_test(as.formula(paste(dv, iv, sep = " ~ ")), .data, paired = paired,
             ...) %>%
      t_apa(print = FALSE)
  }
  # Run ANOVA otherwise
  else
  {
    if (!missing(id))
    {
      if (!is.character(substitute(id))) id <- as.character(substitute(id))
    }

    # List with arguments for `aov_ez`, because we call it with `do.call`
    args <- list(id = id, dv = dv, data = .data, ...)

    # Set independent variable as between of within factor
    if (!paired) args$between <- iv else args$within <- iv

    test <-
      # Silence `aov_ez`; it informs about changing contrasts on each call
      suppressMessages(do.call("aov_ez", args)) %>%
      anova_apa(effect = iv, print = FALSE)
  }

  # Extract t- or F-value
  statistic <- str_extract(test, "(?<= )(< )?-?[0-9]+\\.[0-9]{2}")

  # Extract p-value (delete equal sign if present)
  p <- str_extract(test, "([<>] )?\\.[0-9]{3}")

  # Extract value of effect size
  es <- str_extract_all(test, "(< )?-?[0-9]*\\.[0-9]{2}")[[1]] %>% last()

  # Significance asterisks if requested
  if (sig)
  {
    sig <-
      sub("[<=>] ", "", p) %>%
      as.numeric() %>%
      p_to_symbol()
  }
  else
  {
    sig <- character(0)
  }

  # Build data frame
  c(dv, descriptives, statistic, p, es, sig) %>%
    setNames(header) %>%
    as.list() %>%
    as_data_frame()
}

#' @importFrom magrittr %>%
#' @importFrom stats as.formula
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
