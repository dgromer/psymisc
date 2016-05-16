#' Report Chi-squared test in APA style
#'
#' @importFrom rmarkdown render
#' @param x a call to \code{chisq.test}
#' @param print_n logical indicating whether to show sample size in text
#' @param format character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as character string.
#' @examples
#' chisq_apa(chisq.test(hquest$group, hquest$gender))
#'
#' @export
chisq_apa <- function(x, print_n = FALSE, format = c("text", "markdown",
                                                     "rmarkdown", "html",
                                                     "latex", "docx",
                                                     "plotmath"),
                      info = FALSE, print = TRUE)
{
  format <- match.arg(format)

  # Check if 'x' was a call to `chisq.test`
  if (!grepl("Chi-squared test", x$method))
  {
    stop("'x' must be a call to `chisq.test`")
  }

  if (format == "docx")
  {
    return(apa_to_docx("chisq_apa", x))
  }

  # Extract and format test statistics
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  n <- if (print_n) paste(", n =", sum(x$observed)) else ""
  p <- fmt_pval(x$p.value)

  if (info) message(x$method)

  # Put the formatted string together
  text <- paste0(fmt_symb("chisq", format), "(", df, n, ") ", statistic, ", ",
                 fmt_symb("p", format), " ", p)

  if (format == "latex")
  {
    text <- spaces_latex(text)
  }
  else if (format == "plotmath")
  {
    # Convert text to an expression
    text <- apa_to_plotmath(text, "(\\([0-9]+.*\\) [<=] [0-9]\\.[0-9]{2}, )",
                            "( [<=>] \\.[0-9]{3})")

    # Text is an expression, so we can't use `cat` to print it to the console
    print <- FALSE
  }

  if (print) cat(text) else text
}

#' Report Correlation in APA style
#'
#' @importFrom rmarkdown render
#' @param x a call to \code{cor.test}
#' @param format character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as character string.
#' @examples
#' cor_apa(cor.test(~ acrophobia + sens_seek, hquest))
#'
#' @export
cor_apa <- function(x, format = c("text", "markdown", "rmarkdown", "html",
                                  "latex", "docx", "plotmath"),
                    info = FALSE, print = TRUE)
{
  format <- match.arg(format)

  # Check if 'x' was a call to `cor.test`
  if (!grepl("correlation", x$method))
  {
    stop("'x' must be a call to `cor.test`")
  }

  if (format == "docx")
  {
    return(apa_to_docx("cor_apa", x))
  }

  # Extract and format test statistics
  coef <- tolower(strsplit(x$method, " ")[[1]][1])
  estimate <- fmt_stat(x$estimate, leading_zero = FALSE,
                       negative_values = TRUE)
  df <- x$parameter
  p <- fmt_pval(x$p.value)

  if (info) message(x$method)

  # Put the formatted string together
  text <- paste0(fmt_symb(coef, format),
                 if (coef == "pearson's") paste0("(", df, ") ") else " ",
                 estimate, ", ", fmt_symb("p", format), " ", p)

  if (format == "latex")
  {
    text <- spaces_latex(text)
  }
  else if (format == "plotmath")
  {
    # Convert text to an expression
    text <- apa_to_plotmath(text, "(\\([0-9]+\\))", "( [<=] -?\\.[0-9]{2}, )",
                            "( [<=>] \\.[0-9]{3})")

    # Text is an expression, so we can't use `cat` to print it to the console
    print <- FALSE
  }

  if (print) cat(text) else text
}

#' Report t-Test in APA style
#'
#' @importFrom rmarkdown render
#' @param x a call to \code{t_test}
#' @param es character specifying the effect size to report. One of
#'   \code{"cohens_d"} (default), \code{"hedges_g"} or \code{"glass_delta"} if
#'   \code{x} is an independent samples t-test. Ignored if \code{x} is a paired
#'   samples or one sample t-test (cohen's d is reported for these test).
#' @param format character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as character string.
#' @examples
#' t_apa(t_test(sens_seek ~ group, hquest))
#'
#' @export
t_apa <- function(x, es = "cohens_d", format = c("text", "markdown",
                                                 "rmarkdown", "html", "latex",
                                                 "docx", "plotmath"),
                  info = FALSE, print = TRUE)
{
  format <- match.arg(format)

  # Check if 'x' was a call to `t_test` or `t.test`
  if (!grepl("t-test", x$method))
  {
    stop("'x' must be a call to `t_test` or `t.test`")
  }

  if (format == "docx")
  {
    return(apa_to_docx("t_apa", x, es = es))
  }

  # Extract and format test statistics
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  p <- fmt_pval(x$p.value)
  d <- fmt_es(cohens_d(x, corr = if (es == "cohens_d") "none" else es))

  # Format degrees of freedom if Welch correction was applied
  if (grepl("Welch", x$method))
  {
    df <- fmt_stat(df, equal_sign = FALSE)
  }

  if (es != "cohens_d" && (grepl("One Sample|Paired", x$method)))
  {
    warning(paste0("'", es, "' not available for ", x$method, ",",
                   " 'cohens_d' will be reported instead."))
    es <- "cohens_d"
  }

  if (info) message(x$method)

  # Put the formatted string together
  text <- paste0(fmt_symb("t", format), "(", df, ") ", statistic, ", ",
                 fmt_symb("p", format), " ", p, ", ", fmt_symb(es, format), " ",
                 d)

  if (format == "latex")
  {
    text <- spaces_latex(text)
  }
  else if (format == "plotmath")
  {
    # Convert text to an expression
    text <- apa_to_plotmath(
      text, "(\\([0-9]+\\.[0-9]*\\) [<=] -?[0-9]+\\.[0-9]{2}, )",
      "( [<=>] \\.[0-9]{3}, )", "( [<=] -?[0-9]+\\.[0-9]{2}$)"
    )

    # Text is an expression, so we can't use `cat` to print it to the console
    print <- FALSE
  }

  if (print) cat(text) else text
}

#' Report ANOVA in APA style
#'
#' @param x a call to \code{ez::ezANOVA} or \code{afex::afex_ez},
#'   \code{afex::afex_car} or \code{afex::afex_4}
#' @param effect character string indicating the name of the effect to display.
#'   If is \code{NULL}, all effects are reported (default).
#' @param sph_corr character string indicating the method used for correction if
#'   sphericity is violated (only applies to repeated-measures and mixed design
#'   ANOVA). Can be one of \code{"greenhouse-geisser"} (default),
#'   \code{"huynh-feldt"} or \code{"none"} (you may also use the abbreviations
#'   \code{"gg"} or \code{"hf"}).
#' @param es character string indicating the effect size to display in the
#'   output, one of \code{"petasq"} (partial eta squared) or \code{"getasq"}
#'   (generalized eta squared) (you may also use the abbreviations \code{"pes"}
#'   or \code{"ges"}).
#' @param format character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"}, \code{"docx"} or \code{"plotmath"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @param print logical indicating wheter to print the formatted output via
#'   \code{cat} (\code{TRUE}, default) or return as a data frame.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Using the ez package
#' library(ez)
#' ezANOVA(height, dv = anxiety, wid = id, within = level, between = group,
#'         detailed = TRUE) %>%
#'   anova_apa()
#'
#' # Using the afex package
#' library(afex)
#' aov_ez(id = "id", dv = "anxiety", data = height, between = "group",
#'        within = "level") %>%
#'   anova_apa()
#' }
#'
#' @export
anova_apa <- function(x, effect = NULL,
                      sph_corr = c("greenhouse-geisser", "gg", "huynh-feldt",
                                   "hf", "none"),
                      es = c("petasq", "pes", "getasq", "ges"),
                      format = c("text", "markdown", "rmarkdown", "html",
                                 "latex", "docx", "plotmath"),
                      info = FALSE, print = TRUE)
{
  sph_corr <- match.arg(sph_corr)
  es <- match.arg(es)
  format <- match.arg(format)

  # Use a pseudo-S3 method dispatch here, because `ezANOVA` returns a list
  # without a particular class

  if (inherits(x, "afex_aov"))
  {
    anova_apa_afex(x, effect, sph_corr, es, format, info, print)
  }
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    anova_apa_ezanova(x, effect, sph_corr, es, format, info, print)
  }
  else
  {
    stop("'x' must be a call to `ez::ezANOVA` or `afex::aov_*`")
  }
}

#' @importFrom dplyr data_frame
#' @importFrom magrittr %>% %<>%
anova_apa_afex <- function(x, effect, sph_corr, es, format, info, print)
{
  info_msg <- ""

  # Set 'correction' to FALSE because afex does greenhouse-geisser correction on
  # all within-effects by default
  anova <- anova(x, intercept = TRUE, correction = "none")

  # Extract information from object
  tbl <- data_frame(
    effects = row.names(anova),
    statistic = sapply(anova$F, fmt_stat),
    df_n = anova$`num Df`, df_d = anova$`den Df`,
    p = sapply(anova$`Pr(>F)`, fmt_pval),
    symb = sapply(anova$`Pr(>F)`, p_to_symbol),
    es = sapply(effects, function(effect) fmt_es(do.call(es, list(x, effect)),
                                                 leading_zero = FALSE))
  )

  # Check if within-effects are present
  if (length(attr(x, "within")) != 0)
  {
    s <- summary(x)

    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                         `huynh-feldt` =, hf = "HF")

    # Extract Mauchly's test of sphericity
    sph_tests <- s$sphericity.tests

    # Check which effects do not meet the assumption of sphericity
    mauchlys <- dimnames(sph_tests)[[1]][which(sph_tests[, "p-value"] < .05)]

    if (length(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[tbl$effects %in% mauchlys, c("df_n", "df_d")] %<>%
        `*`(s$pval.adjustments[mauchlys, paste(corr_method, "eps")]) %>%
        lapply(fmt_stat, equal_sign = FALSE)

      # Replace p-values in tbl with corrected ones
      tbl[tbl$effects %in% mauchlys, "p"] <-
        s$pval.adjustments[mauchlys, paste0("Pr(>F[", corr_method, "])")] %>%
        sapply(fmt_pval)

      # Add performed corrections to info message
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  The following effects were adjusted using the ",
        if (corr_method == "GG") "Greenhouse-Geisser" else "Huynh-Feldt",
        " correction:\n",
        paste0("  ", mauchlys, " (Mauchly's W ",
               sapply(sph_tests[mauchlys, "Test statistic"], fmt_stat),
               ", p ", sapply(sph_tests[mauchlys, "p-value"], fmt_pval), ")",
               collapse = "\n")
      )
    }
    else
    {
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  No corrections applied, all p-values for Mauchly's test p > .05"
      )
    }
  }

  if (info && info_msg != "") message(info_msg)

  anova_apa_build(tbl, effect, es, format, print)
}

#' @importFrom dplyr data_frame left_join
#' @importFrom magrittr %>% %<>%
anova_apa_ezanova <- function(x, effect, sph_corr, es, format, info, print)
{
  info_msg <- ""

  anova <- x$ANOVA

  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }

  # Extract and format test statistics
  tbl <- data_frame(
    effects = anova$Effect,
    statistic = sapply(anova$F, fmt_stat),
    df_n = anova$DFn, df_d = anova$DFd, p = sapply(anova$p, fmt_pval),
    symb = sapply(anova$p, p_to_symbol),
    es = sapply(effects, function(effect) fmt_es(do.call(es, list(x, effect)),
                                                 leading_zero = FALSE))
  )

  # Apply correction for violation of sphericity if required
  if ("Mauchly's Test for Sphericity" %in% names(x) && sph_corr != "none")
  {
    corr_method <- switch(sph_corr, `greenhouse-geisser` =, gg = "GG",
                          `huynh-feldt` =, hf = "HF")

    # Check which effects do not meet the assumption of sphericity
    mauchlys <- left_join(x$`Mauchly's Test for Sphericity`,
                          x$`Sphericity Corrections`, by = "Effect") %>%
      `[`(.$p < .05, )

    if (nrow(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[tbl$effects %in% mauchlys$Effect, c("df_n", "df_d")] %<>%
        `*`(mauchlys[[paste0(corr_method, "e")]]) %>%
        lapply(fmt_stat, equal_sign = FALSE)

      # Replace p-values in tbl with corrected ones
      tbl[tbl$effects %in% mauchlys$Effect, "p"] <-
        mauchlys[[paste0("p[", corr_method, "]")]] %>%
        sapply(fmt_pval)

      # Add performed corrections to info message
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  The following effects were adjusted using the ",
        if (corr_method == "GG") "Greenhouse-Geisser" else "Huynh-Feldt",
        " correction:\n",
        paste0("  ", mauchlys$Effect, " (Mauchly's W ",
               sapply(mauchlys$W, fmt_stat), ", p ",
               sapply(mauchlys$p, fmt_pval), ")", collapse = "\n")
      )
    }
    else
    {
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  No corrections applied, all p-values for Mauchly's test p > .05"
      )
    }
  }

  if (info && info_msg != "") message(info_msg)

  anova_apa_build(tbl, effect, es, format, print)
}

#' @importFrom dplyr data_frame
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr map_chr
#' @importFrom rmarkdown render
anova_apa_build <- function(tbl, effect, es_name, format, print)
{
  # Output for default parameters
  if (format == "text" && print)
  {
    # Split test statistic and its sign, because the tabular output will be
    # aligned along the test statistic
    sign <- substr(tbl$statistic, 1, 1)
    statistic <- substr(tbl$statistic, 2, nchar(tbl$statistic))

    tbl <- data_frame(
      Effect = tbl$effects,
      ` ` = paste0("F(", tbl$df_n, ", ", tbl$df_d, ") ", sign,
                   format(statistic, width = max(nchar(statistic)),
                          justify = "right"),
                   ", p ", tbl$p, ", ", fmt_symb(es_name, format), " ", tbl$es,
                   " ", format(tbl$symb, width = 3))
    )

    if (is.null(effect))
    {
      print.data.frame(tbl)
    }
    else
    {
      # Extract text for specified effect from tbl.
      `[.data.frame`(tbl, tbl$Effect == effect, " ") %>%
        # Remove alignment whitespaces
        gsub("[[:blank:]]+", " ", .) %>%
        cat()
    }
  }
  else if (format == "docx")
  {
    # Create temporary markdown file
    tmp <- tempfile("anova_apa", fileext = ".md")
    sink(tmp)
    # Put the formatted string together
    out <- paste0(tbl$effects, " *F*(", tbl$df_n, ", ", tbl$df_d, ") ",
                  tbl$statistic, ", *p* ", tbl$p, ", ",
                  fmt_symb(es_name, "rmarkdown"), " ", tbl$es, "\n\n")

    if (is.null(effect))
    {
      # Write output line by line to the markdown file
      for (i in seq_along(out)) cat(out[i])
    }
    else
    {
      # Select only the output string for 'effect'
      out[which(tbl$effects == effect)] %>%
        # Remove the name of the effect from the beginning of the string
        sub("^.*\\s\\*F\\*", "\\*F\\*", .) %>%
        # Write to markdown file
        cat()
    }

    sink()
    # Convert markdown to docx
    outfile <- render(tmp, output_format = "word_document", quiet = TRUE)

    sys_open(outfile)

    return()
  }
  else
  {
    # Put the formatted string together
    text <- paste0(fmt_symb("F", format), "(", tbl$df_n, ", ", tbl$df_d, ") ",
                   tbl$statistic, ", ", fmt_symb("p", format), " ", tbl$p, ", ",
                   fmt_symb(es_name, format), " ", tbl$es)

    if (format == "latex")
    {
      text <- map_chr(text, spaces_latex)
    }

    # Check if 'effect' is specified, because we can't print a data frame with
    # expressions.
    if (format == "plotmath")
    {
      if (is.null(effect))
      {
        stop("Argument 'effect' must be specified if 'format' is \"plotmath\"")
      }

      # Set 'print' to FALSE for plotmath format
      print <- FALSE
    }

    # cat to console
    if (print)
    {
      if (is.null(effect))
      {
        # Align names of effects
        tbl$effects <- format(paste0(tbl$effects, ": "),
                              width = max(sapply(tbl$effects, nchar)))

        # Add line breaks
        text <- paste0(tbl$effects, text, "\n")

        for (i in seq_along(text))
        {
          cat(text[i])
        }
      }
      else
      {
        cat(text[which(tbl$effect == effect)])
      }
    }
    # Return as string(s)
    else
    {
      if (is.null(effect))
      {
        data_frame(effect = tbl$effects, text = text)
      }
      else
      {
        text <- text[which(tbl$effects == effect)]

        if (format == "plotmath")
        {
          text <- apa_to_plotmath(
            text, "(\\([0-9]+\\.?[0-9]*, [0-9]+\\.?[0-9]*\\) [<=] [0-9]+\\.[0-9]{2}, )",
            "( [<=>] \\.[0-9]{3}, )", "( [<=] -?[0-9]*\\.[0-9]{2}$)"
          )
        }

        text
      }
    }
  }
}

# Create a docx file and open it
apa_to_docx <- function(fun, x, ...)
{
  tmp <- tempfile("to_apa", fileext = ".md")
  sink(tmp)
  do.call(fun, list(x, format = "rmarkdown", ...))
  sink()
  outfile <- render(tmp, output_format = "word_document", quiet = TRUE)

  sys_open(outfile)
}

# Open with standard application on different operating systems
sys_open <- function(filename)
{
  sys <- Sys.info()[['sysname']]

  if (sys == "Windows")
  {
    shell(paste0("\"", filename, "\""))
  }
  else if (sys == "Linux")
  {
    system(paste0("xdg-open \"", filename, "\""))
  }
  else if (sys == "Darwin")
  {
    system(paste0("open \"", filename, "\""))
  }
}

# Convert APA text to an expression in R's plotmath syntax
#' @importFrom stringr str_trim
apa_to_plotmath <- function(text, ...)
{
  # Remove significance asterisks if there are any
  text <- str_trim(gsub("\\**", "", text))

  dots <- list(...)

  # Enclose plain text in single quotes and add comma between plotmath syntax
  # and plain text because we are going to put everything in a call to `paste`.
  for (i in seq_along(dots))
  {
    # If it is not the last element to be replaced, add comma before and after
    if (i < length(dots))
    {
      text <- sub(dots[[i]], ", '\\1', ", text)
    }
    else
    {
      text <- sub(dots[[i]], ", '\\1'", text)
    }
  }

  text <- paste0("paste(", text, ")")

  # Create the expression
  parse(text = text)
}

#' APA Formatting for RMarkdown Reports
#'
#' A wrapper around the \code{*_apa} functions, providing a convenient way to
#' use the formatters in inline code in RMarkdown documents.
#'
#' @param x an \R object. Must be a call to one of \code{afex::aov_4},
#'   \code{afex::aov_car}, \code{afex::aov_ez}, \code{chisq.test},
#'   \code{cor.test}, \code{ez::ezANOVA} or \code{t_test}.
#' @param effect (only applicable if \code{x} is an ANOVA) character string
#'   indicating the name of the effect to display. If is \code{NULL}, all
#'   effects are reported (default).
#' @param format character string specifying the output format. One of
#'   \code{"text"}, \code{"markdown"}, \code{"rmarkdown"}, \code{html},
#'   \code{"latex"} or \code{"docx"}.
#' @param print logical indicating whether to return the result as an \R object
#'   (\code{FALSE}) or print using \code{cat} (\code{TRUE}).
#' @param ... further arguments passed to other methods
#' @seealso \link{anova_apa}, \link{chisq_apa},
#'   \link{cor_apa}, \link{t_apa}
#'
#' @export
apa <- function(x, effect = NULL, format = "rmarkdown", print = FALSE, ...)
{
  if (inherits(x, "htest"))
  {
    if (grepl("Chi-squared test", x$method))
    {
      chisq_apa(x, format = format, print = print, ...)
    }
    else if (grepl("correlation", x$method))
    {
      cor_apa(x, format = format, print = print, ...)
    }
    else if (grepl("t-test", x$method))
    {
      t_apa(x, format = format, print = print, ...)
    }
    else
    {
      stop("Unkown type passed to 'x'")
    }
  }
  else if (inherits(x, "afex_aov") || (is.list(x) && names(x)[1] == "ANOVA"))
  {
    anova_apa(x, effect, format = format, print = print, ...)
  }
  else
  {
    stop("Unkown type passed to 'x'")
  }
}

