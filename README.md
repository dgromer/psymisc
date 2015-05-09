# psymisc: Miscellaneous functions for psychologists

psymisc is an R package that provides some useful functions for psychologists that make everyday data analysis a little bit easier.

## Functions

-   `ds()`: An alternative to `aggregate` for descriptive statistics
-   `recode()`: Recode variables
-   `se()`: Standard error
-   `t_test()`: A wrapper for `t.test()` that includes sample sizes in its return value
-   `*_apa()`: A set of functions for formatting statistical output ready to copy-and-paste into manuscripts (supports Text, Markdown, RMarkdown, LaTeX and Word). Currently available methods are `chisq_apa` and `cor_apa`.


## Installation

The development version can be installed using:

```r
install.packages("devtools")
devtools::install_github("dgromer/psymisc")
```
