# Format a statistic, e.g. t, F, mean, standard deviation
fmt_stat <- function(statistic, leading_zero = TRUE, equal_sign = TRUE,
                     negative_values = TRUE)
{
  if (!negative_values && statistic < .01)
  {
    statistic <- "< 0.01"
  }
  else
  {
    statistic <- sprintf("%.2f", statistic)

    if (equal_sign)
    {
      statistic <- paste("=", statistic)
    }
  }

  if (!leading_zero)
  {
    statistic <- sub("0\\.", "\\.", statistic)
  }

  statistic
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
    if (es == "= 1.00")
    {
      es <- "> .99"
    }
    else
    {
      es <- sub("0.", ".", es)
    }
  }

  es
}

fmt_symb <- function(x, format)
{
  if (format == "text")
  {
    switch(x,
           "chisq"       = "chi^2",
           "cohens_d"    = "d",
           "F"           = "F",
           "getasq"      = "getasq",
           "glass_delta" = "Delta",
           "hedges_g"    = "g",
           "kendall's"   = "r_tau",
           "p"           = "p",
           "pearson's"   = "r",
           "petasq"      = "petasq",
           "r"           = "r",
           "spearman's"  = "r_s",
           "t"           = "t")
  }
  else if (format == "latex")
  {
    switch(x,
           "chisq"       = "\\textit{chisq}",
           "cohens_d"    = "\\textit{d}",
           "F"           = "\\textit{F}",
           "getasq"      = "$\\eta^2_g$",
           "glass_delta" = "$\\Delta$",
           "hedges_g"    = "\\textit{g}",
           "kendall's"   = "$r_\\tau$",
           "p"           = "\\textit{p}",
           "pearson's"   = "\\textit{r}",
           "petasq"      = "$\\eta^2_p$",
           "r"           = "\\textit{r}",
           "spearman's"  = "$r_s$",
           "t"           = "\\textit{t}")
  }
  else if (format == "markdown")
  {
    switch(x,
           "chisq"       = "*chi^2*",
           "cohens_d"    = "*d*",
           "F"           = "*F*",
           "getasq"      = "*getasq*",
           "glass_delta" = "*Delta*",
           "hedges_g"    = "*g*",
           "kendall's"   = "*r_tau*",
           "p"           = "*p*",
           "pearson's"   = "*r*",
           "petasq"      = "*petasq*",
           "r"           = "*r*",
           "spearman's"  = "*r_s*",
           "t"           = "*t*")
  }
  else if (format == "rmarkdown")
  {
    switch(x,
           "chisq"       = "$\\chi^2$",
           "cohens_d"    = "*d*",
           "F"           = "*F*",
           "getasq"      = "$\\eta^2_g$",
           "glass_delta" = "$\\Delta$",
           "hedges_g"    = "*g*",
           "kendall's"   = "$r_\\tau$",
           "p"           = "*p*",
           "pearson's"   = "*r*",
           "petasq"      = "$\\eta^2_p$",
           "r"           = "*r*",
           "spearman's"  = "$r_s$",
           "t"           = "*t*")
  }
  else if (format == "html")
  {
    switch(x,
           "chisq"       = "<i>&chi;</i><sup>2</sup>",
           "cohens_d"    = "<i>d</i>",
           "F"           = "<i>F</i>",
           "getasq"      = "<i>&eta;<sup>2</sup><sub>g</sub></i>",
           "glass_delta" = "<i>&Delta;</i>",
           "hedges_g"    = "<i>g</i>",
           "kendall's"   = "<i>r<sub>&tau;</sub></i>",
           "p"           = "<i>p</i>",
           "pearson's"   = "<i>r</i>",
           "petasq"      = "<i>&eta;<sup>2</sup><sub>p</sub></i>",
           "r"           = "<i>r</i>",
           "spearman's"  = "<i>r<sub>s</sub></i>",
           "t"           = "<i>t</i>")
  }
  else if (format == "plotmath")
  {
    switch(x,
           "chisq"       = "chi^2",
           "cohens_d"    = "italic('d')",
           "F"           = "italic('F')",
           "getasq"      = "eta[g]^2",
           "glass_delta" = "Delta",
           "hedges_g"    = "italic('g')",
           "kendall's"   = "italic(r)[tau]",
           "p"           = "italic('p')",
           "pearson's"   = "italic('r')",
           "petasq"      = "eta[p]^2",
           "r"           = "italic('r')",
           "spearman's"  = "italic(r)[s]",
           "t"           = "italic('t')")
  }
}

p_to_symbol <- function(p)
{
  if (is.na(p))
  {
    ""
  }
  else if (p >= .1)
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

# Convert a data.frame or matrix to a string with LaTeX rows and columns
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
