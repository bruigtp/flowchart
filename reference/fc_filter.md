# fc_filter

This function allows to filter the flowchart in function of a expression
that returns a logic value that are defined in terms of the variables in
the database. It will generate one box per group showing the number of
rows of the group that matches the condition, and will retain only those
rows in the data base.

## Usage

``` r
fc_filter(
  object,
  filter = NULL,
  N = NULL,
  label = NULL,
  text_pattern = "{label}\n {n} ({perc}%)",
  perc_total = FALSE,
  show_exc = FALSE,
  direction_exc = "right",
  label_exc = "Excluded",
  text_pattern_exc = "{label}\n {n} ({perc}%)",
  sel_group = NULL,
  round_digits = 2,
  trim_trailing_zeros = FALSE,
  just = "center",
  text_color = "black",
  text_fs = 8,
  text_fface = 1,
  text_ffamily = NA,
  text_padding = 1,
  bg_fill = "white",
  border_color = "black",
  width = NA,
  height = NA,
  just_exc = "center",
  text_color_exc = "black",
  text_fs_exc = 6,
  text_fface_exc = 1,
  text_ffamily_exc = NA,
  text_padding_exc = 1,
  bg_fill_exc = "white",
  border_color_exc = "black",
  offset_exc = NULL,
  width_exc = NA,
  height_exc = NA,
  title = NULL,
  x_title = 0.1,
  text_color_title = "black",
  text_fs_title = 10,
  text_fface_title = 1,
  text_ffamily_title = NA,
  text_padding_title = 0.6,
  bg_fill_title = "#B3D1FF",
  border_color_title = "black",
  width_title = NA,
  height_title = NA
)
```

## Arguments

- object:

  fc object that we want to filter.

- filter:

  Expression that returns a logical value and are defined in terms of
  the variables in the data frame. The data base will be filtered by
  this expression, and it will create a box showing the number of rows
  satisfying this condition.

- N:

  Number of rows after the filter in case `filter` is `NULL`.

- label:

  Character or expression that will be the title of the box. By default
  it will be the evaluated condition.

- text_pattern:

  Character or expression defining the structure that will have the text
  in each of the boxes. It recognizes `label`, `n`, `N` and `perc`
  within brackets. For default it is `"{label}\n {n} ({perc}%)"`. If
  `text_pattern` or `label` is an expression, the `label` is always
  placed at the beginning of the pattern, followed by a line break where
  the structure specified by `text_pattern` is placed.

- perc_total:

  logical. Should percentages be calculated using the total number of
  rows at the beginning of the flowchart? Default is `FALSE`, meaning
  that they will be calculated using the number at the parent leaf.

- show_exc:

  Logical value. If `TRUE` a box showing the number of excluded rows
  will be added to the flow chart.

- direction_exc:

  One of `"left"` or `"right"` indicating if the exclusion box goes into
  the left direction or in the right direction. By default is `"right"`.

- label_exc:

  Character or expression that will be the title of the added box
  showing the excluded patients. By default it will show "Excluded".

- text_pattern_exc:

  Character or expression defining the structure that will have the text
  in the exclude box. It recognizes `label`, `n`, `N` and `perc` within
  brackets. For default it is `"{label}\n {n} ({perc}%)"`. If
  text_pattern or label is an expression, the label is always placed at
  the beginning of the pattern, followed by a line break where the
  structure specified by `text_pattern_exc` is placed.

- sel_group:

  Select the group in which to perform the filter. The default is
  `NULL`. Can only be used if the flowchart has previously been split.
  If the flowchart has more than one group, it can either be given the
  full name as it is stored in the `$fc` component (separated by '\\),
  or it can be given as a vector with the names of each group to be
  selected.

- round_digits:

  Number of digits to round percentages. It is 2 by default.

- trim_trailing_zeros:

  Logical value. If `TRUE`, allows trailing zeros after the decimal to
  be trimmed (default is `FALSE`).

- just:

  Justification for the text: `"left"`, `"center"` or `"right"`. Default
  is `"center"`.

- text_color:

  Color of the text. It is `"black"` by default. See the `col` parameter
  for gpar.

- text_fs:

  Font size of the text. It is 8 by default. See the `fontsize`
  parameter for gpar.

- text_fface:

  Font face of the text. It is 1 by default. See the `fontface`
  parameter for gpar.

- text_ffamily:

  Changes the font family of the text. Default is `NA`. See the
  `fontfamily` parameter for gpar.

- text_padding:

  Changes the text padding inside the box. Default is 1. This number has
  to be greater than 0.

- bg_fill:

  Box background color. It is `"white"` by default. See the `fill`
  parameter for gpar.

- border_color:

  Box border color. It is `"black"` by default. See the `col` parameter
  for gpar.

- width:

  Width of the box. If `NA`, it automatically adjusts to the content
  (default). Must be an object of class unit or a number between 0 and
  1.

- height:

  Height of the box. If `NA`, it automatically adjusts to the content
  (default). Must be an object of class unit or a number between 0 and
  1.

- just_exc:

  Justification for the text of the exclude box: `"left"`, `"center"` or
  `"right"`. Default is `"center"`.

- text_color_exc:

  Color of the text of the exclude box. It is `"black"` by default. See
  `text_color`.

- text_fs_exc:

  Font size of the text of the exclude box. It is 6 by default. See
  `text_fs`.

- text_fface_exc:

  Font face of the text of the exclude box. It is 1 by default. See the
  `fontface` parameter for gpar. See `text_fface`.

- text_ffamily_exc:

  Changes the font family of the text of the exclude box. Default is
  `NA`. See the `fontfamily` parameter for gpar. See `text_ffamily`.

- text_padding_exc:

  Changes the text padding inside the exclude box. Default is 1. This
  number has to be greater than 0.

- bg_fill_exc:

  Exclude box background color. It is `"white"` by default. See
  `bg_fill`.

- border_color_exc:

  Box background color of the exclude box. It is `"black"` by default.
  See `border_color`.

- offset_exc:

  Amount of space to add to the distance between the box and the
  excluded box (in the x coordinate). If positive, this distance will be
  larger. If negative, it will be smaller. This number has to be at
  least between 0 and 1 (plot limits) and the resulting x coordinate
  cannot exceed these plot limits. The default is `NULL` (no offset).

- width_exc:

  Width of the exclude box. If `NA`, it automatically adjusts to the
  content (default). Must be an object of class unit or a number between
  0 and 1.

- height_exc:

  Height of the box. If `NA`, it automatically adjusts to the content
  (default). Must be an object of class unit or a number between 0 and
  1.

- title:

  Add a title box to the filter. Default is `NULL`.

- x_title:

  x-coordinate of the title box. Default is `0.1` (placed in the left).

- text_color_title:

  Color of the title text. It is `"black"` by default.

- text_fs_title:

  Font size of the title text. It is 8 by default.

- text_fface_title:

  Font face of the title text. It is 1 by default. See the `fontface`
  parameter for gpar.

- text_ffamily_title:

  Changes the font family of the title text. Default is `NA`. See the
  `fontfamily` parameter for gpar.

- text_padding_title:

  Changes the title text padding inside the box. Default is 1. This
  number has to be greater than 0.

- bg_fill_title:

  Title box background color. It is `"white"` by default.

- border_color_title:

  Title box border color. It is `"black"` by default.

- width_title:

  Width of the title box. If `NA`, it automatically adjusts to the
  content (default). Must be an object of class unit or a number between
  0 and 1.

- height_title:

  Height of the title box. If `NA`, it automatically adjusts to the
  content (default). Must be an object of class unit or a number between
  0 and 1.

## Value

List with the filtered dataset and the flowchart parameters with the
resulting filtered box.

## Examples

``` r
safo |>
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
  fc_draw()

```
