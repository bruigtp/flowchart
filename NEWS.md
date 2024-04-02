# flowchart 0.1.0

* Initial CRAN submission.

# flowchart 0.2.0

## Major changes

* Added new `N=` argument in functions `as_fc()`, `fc_filter()` and `fc_split()` to enter the number of rows manually in case that a dataframe is not available. 

* New `fc_export()` function to export a flowchart in the desired format.

* Solved bug when performing a split. Now the x-coordinates of the resulting boxes are placed symmetrically around the parent box.

## Minor changes

* Added `round_digits=` argument to `fc_filter()` and `fc_split()` functions, which allows to change the number of decimals to round percentages.

* Added `show_zero=` argument to `fc_split()`, which allows to control whether the groups with zero events should be shown in a box or not.

* Updated description with minimum R version (>= 4.1.0).

* Older minimum version for dplyr.
