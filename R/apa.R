#' Report Chi-squared test in APA style
#' 
#' @importFrom rmarkdown render
#' @param x a call to \code{chisq.test}
#' @param print_n logical indicating whether to show sample size in text
#' @param format character string specifying the output format. One of 
#'   \code{"default"}, \code{"text"}, \code{"markdown"}, \code{"rmarkdown"},
#'   \code{html}, \code{"latex"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @examples
#' chisq_apa(chisq.test(height$group, height$sex))
#' 
#' @export
chisq_apa <- function(x, print_n = FALSE, format = c("default", "text",
                                                     "markdown", "rmarkdown",
                                                     "html", "latex", "docx"),
                      info = FALSE)
{
  format <- match.arg(format)
  
  if (!grepl("Chi-squared test", x$method))
  {
    stop("'x' must be a call to `chisq.test`")
  }
  
  # Extract and format test statistics
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  n <- ifelse(print_n, paste(", n =", sum(x$observed)), "")
  p <- fmt_pval(x$p.value)
  
  if (info) message(x$method)
  
  if (format == "default")
  {
    paste0("chi^2(", df, n, ") = ", statistic, ", p ", p)
  }
  else if (format == "text")
  {
    cat("chi^2(", df, n, ") = ", statistic, ", p ", p, sep = "")
  }
  else if (format == "latex")
  {
    cat("$\\chi^2$(", df, n, ") = ", statistic, ", \\textit{p} ", p, sep = "")
  }
  else if (format == "markdown")
  {
    cat("*chi^2*(", df, n, ") = ", statistic, ", *p* ", p, sep = "")
  }
  else if (format == "rmarkdown")
  {
    cat("$\\chi^2$(", df, n, ") = ", statistic, ", *p* ", p, sep = "")
  }
  else if (format == "html")
  {
    cat("<i>&chi;</i><sup>2</sup>(", df, n, ") = ", statistic, ", <i>p</i> ",
        p, sep = "")
  }
  else if (format == "docx")
  {
    apa_to_docx("chisq_apa", x)
  }
}

#' Report Correlation in APA style
#' 
#' @importFrom rmarkdown render
#' @param x a call to \code{cor.test}
#' @param format character string specifying the output format. One of 
#'   \code{"default"}, \code{"text"}, \code{"markdown"}, \code{"rmarkdown"},
#'   \code{html}, \code{"latex"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @examples
#' cor_apa(cor.test(~ anx_lvl1 + anx_lvl2, height))
#' 
#' @export
cor_apa <- function(x, format = c("default", "text", "markdown", "rmarkdown",
                                  "html", "latex", "docx"),
                    info = FALSE)
{
  format <- match.arg(format)
  
  if (!grepl("correlation", x$method))
  {
    stop("'x' must be a call to `cor.test`")
  }
  
  # Extract and format test statistics
  coef <- cor_coef(x$method)
  estimate <- fmt_stat(x$estimate)
  df <- x$parameter
  p <- fmt_pval(x$p.value)
  
  if (info) message(x$method)
  
  if (format == "default")
  {
    if (coef == "r")
    {
      paste0("r(", df, ") = ", estimate, ", p ", p)
    }
    else
    {
      paste0(coef, " = ", estimate, ", p ", p)
    }
  }
  else if (format == "text")
  {
    if (coef == "r")
    {
      cat("r(", df, ") = ", estimate, ", p ", p, sep = "")
    }
    else
    {
      cat(coef, " = ", estimate, ", p ", p, sep = "")
    }
  }
  else if (format == "latex")
  {
    if (coef == "r")
    {
      cat("\\textit{r}(", df, ") = ", estimate, ", \\textit{p} ", p, sep = "")
    }
    else if (coef == "tau")
    {
      cat("$r_\\tau$ = ", estimate, ", \\textit{p} ", p, sep = "")
    }
    else
    {
      cat("$r_s$ = ", estimate, ", \\textit{p} ", p, sep = "")
    }
  }
  else if (format == "markdown")
  {
    if (coef == "r")
    {
      cat("*r*(", df, ") = ", estimate, ", *p* ", p, sep = "")
    }
    else
    {
      cat("*r_", coef, " = ", estimate, ", *p* ", p, sep = "")
    }
  }
  else if (format == "rmarkdown")
  {
    if (coef == "r")
    {
      cor_apa(x, format = "markdown")
    }
    else if (coef == "tau")
    {
      cat("$r_\\tau$ = ", estimate, ", *p* ", p, sep = "")
    }
    else
    {
      cat("$r_s$ = ", estimate, ", *p* ", p, sep = "")
    }
  }
  else if (format == "html")
  {
    if (coef == "r")
    {
      cat("<i>r</i>(", df, ") = ", estimate, ", <i>p</i> ", p, sep = "")
    }
    else if (coef == "tau")
    {
      cat("<i>r<sub>&tau;</sub></i> = ", estimate, ", <i>p</i> ", p, sep = "")
    }
    else
    {
      cat("<i>r<sub>s</sub></i> = ", estimate, ", <i>p</i> ", p, sep = "")
    }
  }
  else if (format == "docx")
  {
    apa_to_docx("cor_apa", x)
  }
}

cor_coef <- function(x)
{
  if (grepl("Pearson's", x))
  {
    "r"
  }
  else if (grepl("Kendall's", x))
  {
    "tau"
  }
  else if (grepl("Spearman's", x))
  {
    "s"
  }
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
#'   \code{"default"}, \code{"text"}, \code{"markdown"}, \code{"rmarkdown"},
#'   \code{html}, \code{"latex"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @examples
#' t_apa(t_test(anx_lvl1 ~ group, height))
#' 
#' @export
t_apa <- function(x, es = "cohens_d", format = c("default", "text", "markdown",
                                                 "rmarkdown", "html", "latex",
                                                 "docx"),
                  info = FALSE)
{
  format <- match.arg(format)
  
  # Extract and format test statistics
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  p <- fmt_pval(x$p.value)
  d <- fmt_es(cohens_d(x, corr = es))
  
  es_name <- switch(es, "cohens_d" = "d", "hedges_g" = "g",
                    "glass_delta" = "delta")
  
  # Format degrees of freedom if correction was applied
  if (grepl("Welch", x$method))
  {
    df <- fmt_stat(df)
  }
  
  if (es != "cohens_d" && (grepl("One Sample|Paired", x$method)))
  {
    warning(paste0("'", es, "' not available for ", x$method, ",",
                   " 'cohens_d' will be reported instead."))
    es_name <- "d"
  }
  
  if (info) message(x$method)
  
  if (format == "default")
  {
    paste0("t(", df, ") = ", statistic, ", p ", p, ", ", es_name, " ", d)
  }
  else if (format == "text")
  {
    cat("t(", df, ") = ", statistic, ", p ", p, ", ", es_name, " ", d, sep = "")
  }
  else if (format == "latex")
  {
    cat("\\textit{t}(", df, ") = ", statistic, ", \\textit{p} ", p,
        ", ", latex_es(es), " ", d, sep = "")
  }
  else if (format == "markdown")
  {
    cat("*t*(", df, ") = ", statistic, ", *p* ", p, ", *", es_name, "* ", d,
        sep = "")
  }
  else if (format == "rmarkdown")
  {
    cat("*t*(", df, ") = ", statistic, ", *p* ", p, ", ", rmarkdown_es(es), " ",
        d, sep = "")
  }
  else if (format == "html")
  {
    cat("<i>t</i>(", df, ") = ", statistic, ", <i>p</i>", p, ", ", html_es(es),
        " ", d, sep = "")
  }
  else if (format == "docx")
  {
    apa_to_docx("t_apa", x, es = es)
  }
}

#' Report ANOVA in APA style
#' 
#' @param x a call to \code{ez::ezANOVA}.
#' @param sph_corr character string indicating the method used for correction if
#'   sphericity is violated (only applies to repeated-measures and mixed design
#'   ANOVA). Can be one of \code{"greenhouse-geisser"} (default),
#'   \code{"huynh-feldt"} or \code{"none"} (you may also use the abbreviations
#'   \code{"gg"} or \code{"hf"}).
#' @param es character string indicating the effect size to show in the output,
#'   one of \code{"petasq"} or \code{"getasq"}.
#' @param format character string specifying the output format. One of 
#'   \code{"default"}, \code{"text"}, \code{"markdown"}, \code{"rmarkdown"},
#'   \code{html}, \code{"latex"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' 
#' # Convert data from wide format to long format
#' data <- height %>%
#'   select(id, group, anx_lvl1:anx_lvl3) %>%
#'   gather(key = "level", value = "anxiety", anx_lvl1:anx_lvl3)
#' 
#' # Use ez package
#' library(ez)
#' ezANOVA(data, dv = anxiety, wid = id, within = level, between = group,
#'         detailed = TRUE) %>%
#'   anova_apa()
#' 
#' # Use afex package
#' library(afex)
#' aov_ez(id = "id", dv = "anxiety", data = data, between = "group",
#'        within = "level") %>%
#'   anova_apa()
#' }
#' 
#' @export
anova_apa <- function(x, sph_corr = "greenhouse-geisser", es = "petasq",
                      format = c("default", "text", "markdown", "rmarkdown",
                                 "html", "latex", "docx"),
                      info = FALSE)
{
  format <- match.arg(format)
  
  if (inherits(x, "afex_aov"))
  {
    anova_apa_afex(x, sph_corr, es, format, info)
  }
  else if (is.list(x) && names(x)[1] == "ANOVA")
  {
    anova_apa_ezanova(x, sph_corr, es, format, info)
  }
  else
  {
    stop("'x' must be a call to `ez::ezANOVA` or `afex::aov_*`")
  }
}

#' @importFrom dplyr data_frame
#' @importFrom magrittr %>% %<>%
anova_apa_afex <- function(x, sph_corr, es, format, info)
{
  info_msg <- ""
  
  # Set 'correction' to FALSE because afex does greenhouse-geisser correction on
  # all within-effects by default
  anova <- anova(x, intercept = TRUE, correction = "none")
  
  # Extract information from object
  tbl <- data_frame(
    effects = row.names(anova), statistic = sapply(anova$F, fmt_stat),
    df_n = anova$`num Df`, df_d = anova$`den Df`,
    p = sapply(anova$`Pr(>F)`, fmt_pval),
    symb = sapply(anova$`Pr(>F)`, p_to_symbol),
    es = sapply(effects, function(effect) fmt_es(do.call(es, list(x, effect)),
                                                 leading_zero = FALSE))
  )
  
  if (length(attr(x, "within")) != 0)
  {
    s <- summary(x)
    
    if (sph_corr == "greenhouse-geisser" || sph_corr == "gg")
    {
      corr_method <- "GG"
    }
    else if (sph_corr == "huynh-feldt" || sph_corr == "hf")
    {
      corr_method <- "HF"
    }
    else
    {
      stop(paste0("Unknown correction method '", sph_corr, "'"))
    }
    
    sph_tests <- s$sphericity.tests
    
    # Check which effects do not meet the assumption of sphericity
    mauchlys <- sph_tests[, "p-value"] %>%
      `[`(. < .05) %>%
      names()
    
    if (length(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[tbl$effects == mauchlys, c("df_n", "df_d")] %<>%
        `*`(s$pval.adjustments[mauchlys, paste(corr_method, "eps")]) %>%
        lapply(fmt_stat)
      
      # Replace p-values in tbl with corrected ones
      tbl[tbl$effects == mauchlys, "p"] <-
        s$pval.adjustments[mauchlys, paste0("Pr(>F[", corr_method, "])")] %>%
        sapply(fmt_pval)
      
      # Add performed corrections to info message
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  The following effects were adjusted using the ",
        ifelse(corr_method == "GG", "Greenhouse-Geisser", "Huynh-Feldt"),
        " correction:\n",
        paste0("  ", mauchlys, " (Mauchly's W = ",
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
  
  anova_apa_build(tbl, es, format)
}

#' @importFrom dplyr data_frame left_join
#' @importFrom magrittr %>% %<>%
anova_apa_ezanova <- function(x, sph_corr, es, format, info)
{
  info_msg <- ""
  
  anova <- x$ANOVA
  
  if (!all(c("SSn", "SSd") %in% names(anova)))
  {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }
  
  # Extract and format test statistics
  tbl <- data_frame(
    effects = anova$Effect, statistic = sapply(anova$F, fmt_stat),
    df_n = anova$DFn, df_d = anova$DFd, p = sapply(anova$p, fmt_pval),
    symb = sapply(anova$p, p_to_symbol),
    es = sapply(effects, function(effect) fmt_es(do.call(es, list(x, effect)),
                                                 leading_zero = FALSE))
  )
  
  # Apply correction for violation of sphericity if required
  if ("Mauchly's Test for Sphericity" %in% names(x) && sph_corr != "none")
  {
    if (sph_corr == "greenhouse-geisser" || sph_corr == "gg")
    {
      corr_method <- "GG"
    }
    else if (sph_corr == "huynh-feldt" || sph_corr == "hf")
    {
      corr_method <- "HF"
    }
    else
    {
      stop(paste0("Unknown correction method '", sph_corr, "'"))
    }
    
    # Check which effects do not meet the assumption of sphericity
    mauchlys <- left_join(x$`Mauchly's Test for Sphericity`,
                          x$`Sphericity Corrections`, by = "Effect") %>%
      `[`(.$p < .05, )
    
    
    if (nrow(mauchlys) > 0)
    {
      # Apply correction to degrees of freedom
      tbl[tbl$effects == mauchlys$Effect, c("df_n", "df_d")] %<>%
        `*`(mauchlys[[paste0(corr_method, "e")]]) %>%
        lapply(fmt_stat)
      
      # Replace p-values in tbl with corrected ones
      tbl[tbl$effects == mauchlys$Effect, "p"] <-
        mauchlys[[paste0("p[", corr_method, "]")]] %>%
        sapply(fmt_pval)
      
      # Add performed corrections to info message
      info_msg %<>% paste0(
        "Sphericity corrections:\n",
        "  The following effects were adjusted using the ",
        ifelse(corr_method == "GG", "Greenhouse-Geisser", "Huynh-Feldt"),
        " correction:\n",
        paste0("  ", mauchlys$Effect, " (Mauchly's W = ",
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
  
  anova_apa_build(tbl, es, format)
}

#' @importFrom dplyr data_frame
#' @importFrom rmarkdown render
anova_apa_build <- function(tbl, es_name, format)
{
  if (format == "default")
  {
    # TODO: align df also?
    out <- data_frame(
      Effect = tbl$effects,
      ` ` = paste0("F(", tbl$df_n, ",", tbl$df_d, ") = ",
                   format(tbl$statistic, width = max(nchar(tbl$statistic)),
                          justify = "right"),
                   ", p ", tbl$p, ", ", es_name, " ", tbl$es, " ",
                   format(tbl$symb, width = 3))
    )
  }
  
  tbl$effects <- format(paste0(tbl$effects, ":"),
                        width = max(sapply(tbl$effects, nchar)))
  
  if (format == "text")
  {
    out <- paste0(tbl$effects, " F(", tbl$df_n, ",", tbl$df_d, ") = ",
                  tbl$statistic, ", p ", tbl$p, ", ", es_name, " ", tbl$es, 
                  "\n")
  }
  else if (format == "markdown")
  {
    out <- paste0(tbl$effects, " *F*(", tbl$df_n, ",", tbl$df_d, ") = ",
                  tbl$statistic, ", *p* ", tbl$p, ", *", es_name, "* ",
                  tbl$es, "\n")
  }
  else if (format == "rmarkdown")
  {
    out <- paste0(tbl$effects, " *F*(", tbl$df_n, ",", tbl$df_d, ") = ",
                  tbl$statistic, ", *p* ", tbl$p, ", ", latex_es(es_name),
                  " ", tbl$es, "\n")
  }
  else if (format == "html")
  {
    out <- paste0(tbl$effects, " <i>F</i>(", tbl$df_n, ",", tbl$df_d, ") = ",
                  tbl$statistics, ", <i>p</i> ", tbl$p, ", ", html_es(es_name),
                  " ", tbl$es, "\n")
  }
  else if (format == "latex")
  {
    out <- paste0(tbl$effects, " \\textit{F}(", tbl$df_n, ",", tbl$df_d, ") = ",
                  tbl$statistic, ", \\textit{p} ", tbl$p, ", ",
                  latex_es(es_name), " ", tbl$es, "\n")
  }
  else if (format == "docx")
  {
    tmp <- tempfile("anova_apa", fileext = ".md")
    sink(tmp)
    out <- paste0(tbl$effects, " *F*(", tbl$df_n, ",", tbl$df_d, ") = ",
                  tbl$statistic, ", *p* ", tbl$p, ", ", latex_es(es_name), " ",
                  tbl$es, "\n\n")
    for (i in seq_along(out)) cat(out[i])
    sink()
    outfile <- render(tmp, output_format = "word_document", quiet = TRUE)
    
    sys_open(outfile)
    
    return()
  }

  if (format == "default")
  {
    print.data.frame(out)
  }
  else
  {
    for (i in seq_along(out))
    {
      cat(out[i])
    }
  }
}

# Format a test statistic
fmt_stat <- function(statistic)
{
  sprintf("%.2f", statistic)
}

# Format a p-value
fmt_pval <- function(p, equal_sign = TRUE)
{
  if (p < .001)
  {
    "< .001"
  }
  else if (p == 1)
  {
    "> .999"
  }
  else if (equal_sign)
  {
    paste("=", substr(sprintf("%.3f", p), 2, 5))
  }
  else
  {
    substr(sprintf("%.3f", p), 2, 5)
  }
}

# Format an effect size
fmt_es <- function(es, leading_zero = TRUE, equal_sign = TRUE)
{
  if (is.na(es))
  {
    return(ifelse(leading_zero, "=   NA", "=  NA"))
  }
  
  if (abs(es) < .01)
  {
    es <- "< 0.01"
  }
  else if (equal_sign)
  {
    es <- paste("=", sprintf("%.2f", es))
  }
  else
  {
    es <- sprintf("%.2f", es)
  }
  
  if (!leading_zero)
  {
    es <- sub("0.", ".", es)
  }
  
  es
}

# Format different effect sizes in RMarkdown
rmarkdown_es <- function(es)
{
  switch(es, 
         "cohens_d"    = "*d*",
         "hedges_g"    = "*g*",
         "glass_delta" = "$\\Delta$",
         "petasq"      = "$\\eta^2_p$",
         "getasq"      = "$\\eta^2_g",
         "omegasq"     = "\\omega^2"
  )
}

# Format different effect sizes in LaTeX
latex_es <- function(es)
{
  switch(es, 
        "cohens_d"    = "\\textit{d}",
        "hedges_g"    = "\\textit{g}",
        "glass_delta" = "$\\Delta$",
        "petasq"      = "$\\eta^2_p$",
        "getasq"      = "$\\eta^2_g",
        "omegasq"     = "\\omega^2"
  )
}

# Format different effect sizes in HTML
html_es <- function(es)
{
  switch(es,
         "cohens_d"    = "<i>d</i>",
         "hedges_g"    = "<i>g</i>",
         "glass_delta" = "<i>&Delta;</i>",
         "petasq"      = "<i>&eta;<sup>2</sup><sub>p</sub></i>",
         "getasq"      = "<i>&eta;<sup>2</sup><sub>g</sub></i>",
         "omegasq"     = "<i>&omega;<sup>2</sup></i>"
  )
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
