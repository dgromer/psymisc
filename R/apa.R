#' Report Chi-squared test in APA style
#' 
#' @importFrom rmarkdown render
#' @param x a call to \code{chisq.test}
#' @param print_n logical indicating whether to show sample size in text
#' @param format character specifying the output format, one of \code{"text"},
#'   \code{"latex"}, \code{"markdown"}, \code{"rmarkdown"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' 
#' @export
chisq_apa <- function(x, print_n = FALSE, format = "text", info = FALSE)
{
  if (!grepl("Chi-squared test", x$method))
  {
    stop("`x` must be a call to `chisq.test`")
  }
  
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  n <- ifelse(print_n, paste(", n =", sum(x$observed)), "")
  p <- fmt_pval(x$p.value)
  
  if (info) message(x$method)
  
  if (format == "text")
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
#' @param format character specifying the output format, one of \code{"text"},
#'   \code{"latex"}, \code{"markdown"}, \code{"rmarkdown"} or \code{"docx"}.
#' @param info logical indicating whether to print a message on the used test
#'   (default is \code{FALSE})
#' @examples
#' ct <- cor.test(runif(20), runif(20))
#' cor_apa(ct)
#' cor_apa(ct, format = "latex")
#' 
#' @export
cor_apa <- function(x, format = "text", info = FALSE)
{
  if (!grepl("correlation", x$method))
  {
    stop("`x` must be a call to `cor.test`")
  }
  
  coef <- cor_coef(x$method)
  estimate <- fmt_stat(x$estimate)
  df <- x$parameter
  p <- fmt_pval(x$p.value)
  
  if (info) message(x$method)
  
  if (format == "text")
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
#' @export
t_apa <- function(x, format = "text", info = FALSE)
{
  statistic <- fmt_stat(x$statistic)
  df <- x$parameter
  p <- fmt_pval(x$p.value)
  d <- fmt_es(cohens_d(x))
  
  if (info) message(x$method)
  
  if (format == "text")
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

fmt_stat <- function(statistic)
{
  format(round(statistic, 2), nsmall = 2)
}

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

fmt_es <- function(es)
{
  if (abs(es) < .01)
  {
    "< .01"
  }
  else
  {
    paste("=", format(round(es, 2), nsmall = 2))
  }
}