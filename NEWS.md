# flowchart 0.1.0

* Initial CRAN submission.

# flowchart 0.2.0

## Major changes

* Added new `N=` argument in functions `as_fc()`, `fc_filter()` and `fc_split()` to enter the number of rows manually in case that a dataframe is not available. 

* New `fc_export()` function to export a flowchart in the desired format.

* Solved bug when performing a split. Now the x-coordinates of the resulting boxes are placed symmetrically around the parent box, not homogeneously distributed across all the space.

## Minor changes

* Added `round_digits=` argument to `fc_filter()` and `fc_split()` functions, which allows to change the number of decimals to round percentages.

* Added `show_zero=` argument to `fc_split()`, which allows to control whether the groups with zero events should be shown in a box or not.

* Updated description with minimum R version (>= 4.1.0).

# flowchart 0.3.0

## Major changes

* Bug in the `fc_split()` function when splitting by a factor with levels that are not arranged in alphabetical order.

* Bug in the `fc_split()` function when performing multiple splits and showing percentages. These percentages were calculated based on the total number of rows, not the total number of rows in each of the groups defined by the previous splits. 

# flowchart 0.4.0

## Major changes

* Removed `clinic_patient` and `clinic_visit` built-in datasets

## Minor changes

* Changed `safo` built-in dataset

* Now `unite=FALSE` is the default in the `fc_stack()` function

* Added `bmp` format to the `fc_export()` function
