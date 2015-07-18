# psymisc: Miscellaneous functions for psychologists

psymisc is an R package that provides some useful high-level and helper functions for psychologists that make everyday data analysis a little bit easier.

## Functions

-   `cohens_d()`: Calculate Cohen's d effect size.
-   `ds()`: An alternative to `aggregate()` for descriptive statistics. It wraps `dplyr::group_by()` and `dplyr::sumarise()` into a convenient formula interface.
-   `fplot()`: Convenient plotting of means and standard errors or boxplots of factorial data.
-	`mean_cor()`: Calculate the mean of several correlations using Fisher-Z-transformation.
-   `plotsig()`: A convenience function for displaying significance in ggplot2 plots.
-   `recode()`: Recode variables based on multiple rules.
-   `routlier()`: Remove or flag outliers.
-   `se()`: Standard error, SE = SD(x) / sqrt(n)
-   `stats_table()`: Compare group characteristics (means and standard deviations plus significance tests).
-   `t_test()`: A wrapper for `t.test()` that includes the original data in its return list.
-   `*_apa()`: A set of functions for formatting statistical output according to APA guidelines, ready to copy-and-paste into manuscripts (supports Text, Markdown, RMarkdown, HTML, LaTeX and docx<sup>1</sup>). Currently available methods are `anova_apa()`<sup>2</sup>, `chisq_apa()`, `cor_apa()` and `t_apa()`.

<sup>1</sup> [pandoc](http://pandoc.org/) is required for docx output and needs to be installed manually when not using RStudio (which ships pandoc).

<sup>2</sup> Supports input from `ezANOVA()` from the [ez package](http://cran.r-project.org/package=ez).

## Installation

The development version can be installed using:

```r
install.packages("devtools")
devtools::install_github("dgromer/psymisc")
```

## Related approaches

-   [schoRsch](http://cran.r-project.org/web/packages/schoRsch/)
-   [papaja](https://github.com/crsh/papaja)
