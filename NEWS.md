# flowchart 0.1.0

* Initial CRAN submission.

# flowchart 0.2.0

* Updated description with minimum R version (>= 4.1.0)

* Older minimum version for dplyr

* Added `round_digits=` argument to `fc_filter()` and `fc_split()` functions, which allows to change the number of decimals to round percentages.

* New `fc_export()` function to export a flowchart in the desired format.

* Added new `N=` argument in functions `as_fc()`, `fc_filter()` and `fc_split()` to enter the number of rows manually in case that a dataframe is not available. 
