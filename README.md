# psymisc: Miscellaneous functions for psychologists

psymisc is an R package that provides some useful functions for psychologists that make everyday data analysis a little bit easier.

## Functions

-   `cohens_d()`: Calculate Cohen's d effect size.
-   `ds()`: An alternative to `aggregate()` for descriptive statistics. It wraps `dplyr::group_by` and `dplyr::sumarise` into a convenient formula interface.
-   `recode()`: Recode variables based on multiple rules.
-   `se()`: Standard error, SE = SD(x) / sqrt(n)
-   `t_test()`: A wrapper for `t.test()` that includes the original data in its return list.
-   `*_apa()`: A set of functions for formatting statistical output ready to copy-and-paste into manuscripts (supports Text, Markdown, RMarkdown, LaTeX and Word). Currently available methods are `chisq_apa()`, `cor_apa()` and `t_apa()`.


## Installation

The development version can be installed using:

```r
install.packages("devtools")
devtools::install_github("dgromer/psymisc")
```
