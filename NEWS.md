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

# flowchart 0.5.0

## Major changes

* New arguments `text_fface`, `text_ffamily` and `text_padding` to change the font face, font family and padding of the text inside the box

* New argument `perc_total` to calculate percentages with respect to the total number of rows

* New argument `offset` to add space to the distance between boxes in a split, and `offset_exc` to add space to the distance from the exclude box in a filter

* New function `fc_view()` to view the `$fc` element or the `$data` element, associated to a flowchart

* New argument `title` in the `fc_draw()` function to add a title to a flowchart, along with some additional arguments to customize it

* New argument `title` in the `fc_split()` function to add a title box in a split, together with some additional arguments to customize it

* Bug in the exclusion box out of margins

## Minor changes

* Added minimum version for dplyr (>= 1.1.0)

* Bug in the `fc_export()` function when using the `format` argument

* Replaced evaluations to tidy evaluations using `rlang::eval_tidy()`

# flowchart 0.5.1

* Changed license to GPL (>= 3) license

# flowchart 0.6.0

* Added `box_corners` argument to `fc_draw()` to allow drawing boxes with or without round corners; default set to `"round"` to avoid breaking changes (#2; @kenkomodo)

* Updated `fc_export()` to include vector formats (svg, pdf) and to use `ragg` in place of `grDevices` for relevant bitmap formats (png, jpeg, tiff) for improved performance and image quality when `ragg` is installed (#16; @kenkomodo)

* Methods for S3 class `fc` correctly defined

* Bug in the `hide = TRUE` option in `as_fc()`

* Bug when specifying `sel_group` and `N` at the same time in `fc_split()`

* Bug when specifying `title` in `fc_split()` with a number of splits different than two

# flowchart 0.6.0.9000 (development)

* Solved `bug` when `sel_group` is used repeatedly in the same flowchart

* Changed y-coordinate distribution of boxes when using `fc_stack()` with `unite = TRUE`

* When performing multiple splits in a flowchart, the group label is stored in `$fc` concatenating the values of the different groups separated by '//'.

* Allow expressions in the label argument to produce bold or italics text, or even formulas. 
