#' Report Chi-squared test in APA style
#' 
#' @importFrom rmarkdown render
#' @param x a call to \code{chisq.test}
#' @param print_n logical indicating whether to show sample size in text
#' @param format character specifying the output format, one of
#'   \code{"default"}, \code{"text"}, \code{"latex"}, \code{"markdown"},
#'   \code{"rmarkdown"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' 
#' @export
chisq_apa <- function(x, print_n = FALSE, format = "default", info = FALSE)
{
  check_format(format)
  
  if (!grepl("Chi-squared test", x$method))
  {
    stop("`x` must be a call to `chisq.test`")
  }
  
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
  else if (format == "docx")
  {
    tmp <- tempfile("chi_apa", fileext = ".md")
    sink(tmp)
    chisq_apa(x, format = "rmarkdown")
    sink()
    outfile <- render(tmp, output_format = "word_document", quiet = TRUE)
    
    sys <- Sys.info()[['sysname']]
    
    if (sys == "Windows")
    {
      shell(paste0("\"", outfile, "\""))
    }
    else if (sys == "Linux")
    {
      system(paste0("xdg-open \"", outfile, "\""))
    }
    else if (sys == "Darwin")
    {
      system(paste0("open \"", outfile, "\""))
    }
  }
}

#' Report Correlation in APA style
#' 
#' @importFrom rmarkdown render
#' @param x a call to \code{cor.test}
#' @param format character specifying the output format, one of
#'   \code{"default"}, \code{"text"}, \code{"latex"}, \code{"markdown"},
#'   \code{"rmarkdown"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @examples
#' ct <- cor.test(runif(20), runif(20))
#' cor_apa(ct)
#' cor_apa(ct, format = "latex")
#' 
#' @export
cor_apa <- function(x, format = "default", info = FALSE)
{
  check_format(format)
  
  if (!grepl("correlation", x$method))
  {
    stop("`x` must be a call to `cor.test`")
  }
  
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
  else if (format == "docx")
  {
    tmp <- tempfile("cor_apa", fileext = ".md")
    sink(tmp)
    cor_apa(x, format = "rmarkdown")
    sink()
    outfile <- render(tmp, output_format = "word_document", quiet = TRUE)
    
    sys <- Sys.info()[['sysname']]
    
    if (sys == "Windows")
    {
      shell(paste0("\"", outfile, "\""))
    }
    else if (sys == "Linux")
    {
      system(paste0("xdg-open \"", outfile, "\""))
    }
    else if (sys == "Darwin")
    {
      system(paste0("open \"", outfile, "\""))
    }
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
#' @param format character specifying the output format, one of \code{"text"},
#'   \code{"latex"}, \code{"markdown"}, \code{"rmarkdown"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' 
#' @export
t_apa <- function(x, format = "default", info = FALSE)
{
  check_format(format)
  
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  p <- fmt_pval(x$p.value)
  d <- fmt_es(cohens_d(x))
  
  if (grepl("Welch", x$method))
  {
    df <- fmt_stat(df)
  }
  
  if (info) message(x$method)
  
  if (format == "default")
  {
    paste0("t(", df, ") = ", statistic, ", p ", p, ", d ", d)
  }
  else if (format == "text")
  {
    cat("t(", df, ") = ", statistic, ", p ", p, ", d ", d, sep = "")
  }
  else if (format == "latex")
  {
    cat("\\textit{t}(", df, ") = ", statistic, ", \\textit{p} ", p,
        ", \\textit{d} ", d, sep = "")
  }
  else if (format == "markdown" || format == "rmarkdown")
  {
    cat("*t*(", df, ") = ", statistic, ", *p* ", p, ", *d* ", d, sep = "")
  }
  else if (format == "docx")
  {
    tmp <- tempfile("t_apa", fileext = ".md")
    sink(tmp)
    t_apa(x, format = "rmarkdown")
    sink()
    outfile <- render(tmp, output_format = "word_document", quiet = TRUE)
    
    sys <- Sys.info()[['sysname']]
    
    if (sys == "Windows")
    {
      shell(paste0("\"", outfile, "\""))
    }
    else if (sys == "Linux")
    {
      system(paste0("xdg-open \"", outfile, "\""))
    }
    else if (sys == "Darwin")
    {
      system(paste0("open \"", outfile, "\""))
    }
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
#'   one of \code{"petasq"}, \code{"getasq"}.
#' @param format character string specifying the output format, one of
#'   \code{"default"}, \code{"text"}, \code{"latex"}, \code{"markdown"},
#'   \code{"rmarkdown"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @export
anova_apa <- function(x, sph_corr = "greenhouse-geisser", es = "petasq",
                      format = "default", info = FALSE)
{
  check_format(format)
  
  if (is.list(x) && names(x)[1] == "ANOVA")
  {
    anova_apa_ezanova(x, sph_corr, es, format, info)
  }
  else
  {
    stop("'x' must be a call to `ez::ezANOVA`")
  }
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
  
  tbl <- data_frame(
    effects = anova$Effect, statistic = sapply(anova$F, fmt_stat),
    df_n = anova$DFn, df_d = anova$DFd, p = sapply(anova$p, fmt_pval),
    es = sapply(effects, function(.) fmt_es(do.call(es, list(x, .))))
  )
  
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

#' @importFrom rmarkdown render
anova_apa_build <- function(tbl, es_name, format)
{
  if (format == "default")
  {
    out <- data.frame(
      Effect = tbl$effects,
      Text = paste0("F(", tbl$df_n, ",", tbl$df_d, ") = ",
                    format(tbl$statistic, width = max(nchar(tbl$statistic)),
                           justify = "right"),
                    ", p ", tbl$p, ", ", es_name, " ", tbl$es),
      stringsAsFactors = FALSE
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
    
    sys <- Sys.info()[['sysname']]
    
    if (sys == "Windows")
    {
      shell(paste0("\"", outfile, "\""))
    }
    else if (sys == "Linux")
    {
      system(paste0("xdg-open \"", outfile, "\""))
    }
    else if (sys == "Darwin")
    {
      system(paste0("open \"", outfile, "\""))
    }
    
    return()
  }

  if (format == "default")
  {
    out
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
  format(round(statistic, 2), nsmall = 2)
}

# Format a p-value
fmt_pval <- function(p)
{
  if (p < .001)
  {
    "< .001"
  }
  else
  {
    paste("=", substr(format(round(p, 3), nsmall = 3), 2, 5))
  }
}

# Format an effect size
fmt_es <- function(es)
{
  if (abs(es) < .01)
  {
    "< 0.01"
  }
  else
  {
    paste("=", format(round(es, 2), nsmall = 2))
  }
}

check_format <- function(x)
{
  if (!x %in% c("default", "text", "markdown", "rmarkdown", "latex", "docx"))
  {
    stop("Unknown format")
  }
}

latex_es <- function(es)
{
  if (es == "petasq")
  {
    "$\\eta^2_p$"
  }
  else if (es == "getasq")
  {
    "$\\eta^2_g"
  }
  else if (es == "omegasq")
  {
    "\\omega^2"
  }
}
