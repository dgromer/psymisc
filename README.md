# psymisc: Miscellaneous functions for psychologists

**NOTE:** This package is in development. Function design, names and arguments may change.

psymisc is an R package that provides some useful high-level and helper functions for psychologists that make everyday data analysis a little bit easier.

## Functions

-   `cohens_d()`/`cohens_d_()`: Calculate Cohen's d effect size (from raw data, t-test or statistical parameters). Also supports Hedge's g* and Glass's &Delta;.
-   `ds()`: An alternative to `aggregate()` for summary statistics. It wraps `dplyr::group_by()` and `dplyr::summarise()` into a convenient formula interface.
-   `fplot()`: Convenient plotting of means and standard errors or boxplots of factorial data.
-	`mean_cor()`: Calculate the mean of several correlations using Fisher-Z-transformation.
-   `plotsig()`: A convenience function for displaying significance in ggplot2 plots.
-   `recode()`: Recode variables based on multiple rules.
-   `routlier()`: Remove or flag outliers.
-   `se()`: Standard error, SE = SD(x) / sqrt(n)
-   `stats_table()`: Compare group characteristics (means and standard deviations plus significance tests). Outputs to either the console or HTML (which can then be copy-and-pasted directly into Word).
-   `t_test()`: A wrapper for `t.test()` that includes the original data in its return list (in order to calculate the effect size in `cohens_d()` and `t_apa()` directly from the data).
-   `*_apa()`: A set of functions for formatting statistical output according to APA guidelines, ready to copy-and-paste into manuscripts (supports Text, Markdown, RMarkdown, HTML, LaTeX and docx<sup>1</sup>). Currently available methods are `anova_apa()`<sup>2</sup>, `chisq_apa()`, `cor_apa()` and `t_apa()`. These functions were heavily influenced by the `*_out()` functions in the [schoRsch package](http://cran.r-project.org/web/packages/schoRsch/).

<sup>1</sup> [pandoc](http://pandoc.org/) is required for docx output and needs to be installed manually when not using RStudio (which ships pandoc).

<sup>2</sup> Supports input from `ezANOVA()` from the [ez package](http://cran.r-project.org/package=ez) and `aov_ez()` / `aov_car()` / `aov_4()` from the [afex package](http://cran.r-project.org/package=afex).

## Installation

The development version can be installed using:

```r
install.packages("devtools")
devtools::install_github("dgromer/psymisc")
```

If not already installed, you also need to get the development version of [purrr](https://github.com/hadley/purrr) from GitHub:

```r
devtools::install_github("hadley/purrr")
```

## Related approaches

-   [schoRsch](http://cran.r-project.org/web/packages/schoRsch/)
-   [papaja](https://github.com/crsh/papaja)
