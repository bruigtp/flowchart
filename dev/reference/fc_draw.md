# fc_draw

This function allows to draw the flowchart from a fc object.

## Usage

``` r
fc_draw(
  object,
  big.mark = "",
  box_corners = "round",
  arrow_angle = 30,
  arrow_length = grid::unit(0.1, "inches"),
  arrow_ends = "last",
  arrow_type = "closed",
  arrow_color = "black",
  arrow_fill = "black",
  arrow_lwd = 1,
  arrow_lineend = "round",
  arrow_linejoin = "round",
  title = NULL,
  title_x = 0.5,
  title_y = 0.9,
  title_color = "black",
  title_fs = 15,
  title_fface = 2,
  title_ffamily = NULL,
  canvas_bg = "white"
)
```

## Arguments

- object:

  fc object that we want to draw.

- big.mark:

  character. Used to specify the thousands separator for patient count
  values. Defaults is no separator (`""`); if not empty used as mark
  between every 3 digits (ex: `big.mark = ","` results in `1,000`
  instead of `1000`).

- box_corners:

  Indicator of whether to draw boxes with round (`"round"`) vs non-round
  (`"sharp"`) corners. Default is `"round"`.

- arrow_angle:

  The angle of the arrow head in degrees, as in `arrow`.

- arrow_length:

  A unit specifying the length of the arrow head (from tip to base), as
  in `arrow`.

- arrow_ends:

  One of `"last"`, `"first"`, or `"both"`, indicating which ends of the
  line to draw arrow heads, as in arrow.

- arrow_type:

  One of `"open"` or `"closed"` indicating whether the arrow head should
  be a closed triangle, as in arrow.

- arrow_color:

  Color of the arrows. Default is `"black"`. See the `col` parameter for
  gpar.

- arrow_fill:

  Color for filling the arrow head. Default is `"black"`. See the `fill`
  parameter for gpar.

- arrow_lwd:

  Line width of the arrows. Default is 1. See the `lwd` parameter for
  gpar.

- arrow_lineend:

  Line end style for arrows. One of `"round"`, `"butt"`, or `"square"`.
  Default is `"round"`. See the `lineend` parameter for gpar.

- arrow_linejoin:

  Line join style for arrow heads (i.e., shape of arrow head corners).
  One of `"round"`, `"mitre"`, or `"bevel"`. Default is `"round"`. See
  the `linejoin` parameter for gpar.

- title:

  The title of the flowchart. Default is `NULL` (no title).

- title_x:

  x coordinate for the title. Default is 0.5.

- title_y:

  y coordinate for the title. Default is 0.9.

- title_color:

  Color of the title. It is `"black"` by default. See the `col`
  parameter for gpar.

- title_fs:

  Font size of the title. It is 15 by default. See the `fontsize`
  parameter for gpar.

- title_fface:

  Font face of the title. It is 2 by default. See the `fontface`
  parameter for gpar.

- title_ffamily:

  Changes the font family of the title. Default is `NA`. See the
  `fontfamily` parameter for gpar.

- canvas_bg:

  Background color for the entire canvas (the area behind the flowchart
  boxes). Default is `"white"`. Set to `"transparent"` or `NULL` for a
  transparent background; `"transparent"` background will only be
  noticeable when exporting drawn flowcharts via
  [`fc_export()`](https://bruigtp.github.io/flowchart/dev/reference/fc_export.md)
  and is compatible with all
  [`fc_export()`](https://bruigtp.github.io/flowchart/dev/reference/fc_export.md)
  formats except `"jpeg"` and `"bmp"`.

## Value

Invisibly returns the same object that has been given to the function,
with the given arguments to draw the flowchart stored in the attributes.

## Examples

``` r
safo |>
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
  fc_split(group) |>
  fc_filter(itt == "Yes", label = "Included in ITT") |>
  fc_filter(pp == "Yes", label = "Included in PP") |>
  fc_draw()

```
