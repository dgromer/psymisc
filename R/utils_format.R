# Format a test statistic
fmt_stat <- function(statistic, leading_zero = TRUE, equal_sign = TRUE)
{
  if (statistic < .01)
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
    statistic <- sub(" 0\\.", " \\.", statistic)
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
}
