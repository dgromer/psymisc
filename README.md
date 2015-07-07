# psymisc: Miscellaneous functions for psychologists

psymisc is an R package that provides some useful functions for psychologists that make everyday data analysis a little bit easier.

## Functions

-   `cohens_d()`: Calculate Cohen's d effect size.
-   `ds()`: An alternative to `aggregate()` for descriptive statistics. It wraps `dplyr::group_by()` and `dplyr::sumarise()` into a convenient formula interface.
- `mean_cor()`: Calculate the mean of several correlations using Fisher-Z-transformation.
-   `plotsig()`: A convenience function for displaying significance in ggplot2 plots.
-   `recode()`: Recode variables based on multiple rules.
-   `se()`: Standard error, SE = SD(x) / sqrt(n)
-   `t_test()`: A wrapper for `t.test()` that includes the original data in its return list.
-   `*_apa()`: A set of functions for formatting statistical output according to APA guidelines, ready to copy-and-paste into manuscripts (supports Text, Markdown, RMarkdown, LaTeX and docx<sup>1</sup>). Currently available methods are `anova_apa()`, `chisq_apa()`, `cor_apa()` and `t_apa()`.

<sup>1</sup> [pandoc](http://pandoc.org/) is required for docx output and needs to be installed manually when not using RStudio (which ships pandoc).

## Installation

The development version can be installed using:

```r
install.packages("devtools")
devtools::install_github("dgromer/psymisc")
```

## Related approaches

-   [schoRsch](http://cran.r-project.org/web/packages/schoRsch/)
-   [papaja](https://github.com/crsh/papaja)
