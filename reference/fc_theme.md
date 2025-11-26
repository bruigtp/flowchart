# fc_theme

This function allows you to change the appearance of all boxes of a
flowchart at once.

## Usage

``` r
fc_theme(
  object,
  text_pattern = NULL,
  text_pattern_init = NULL,
  text_pattern_exc = NULL,
  just = NULL,
  text_color = NULL,
  text_fs = NULL,
  text_fface = NULL,
  text_ffamily = NULL,
  text_padding = NULL,
  bg_fill = NULL,
  border_color = NULL,
  width = NULL,
  height = NULL,
  just_exc = NULL,
  text_color_exc = NULL,
  text_fs_exc = NULL,
  text_fface_exc = NULL,
  text_ffamily_exc = NULL,
  text_padding_exc = NULL,
  bg_fill_exc = NULL,
  border_color_exc = NULL,
  width_exc = NULL,
  height_exc = NULL,
  text_color_title = NULL,
  text_fs_title = NULL,
  text_fface_title = NULL,
  text_ffamily_title = NULL,
  text_padding_title = NULL,
  bg_fill_title = NULL,
  border_color_title = NULL,
  width_title = NULL,
  height_title = NULL
)
```

## Arguments

- object:

  fc object.

- text_pattern:

  Text pattern for all the boxes, except the initial and exclusion ones.

- text_pattern_init:

  Text pattern for the initial box.

- text_pattern_exc:

  Text pattern for the exclusion box.

- just:

  Justification for the text: `"left"`, `"center"` or `"right"`.

- text_color:

  Color of the text. See the `col` parameter for gpar.

- text_fs:

  Font size of the text. See the `fontsize` parameter for gpar.

- text_fface:

  Font face of the text. See the `fontface` parameter for gpar.

- text_ffamily:

  Changes the font family of the text. See the `fontfamily` parameter
  for gpar.

- text_padding:

  Changes the text padding inside the box. This number has to be greater
  than 0.

- bg_fill:

  Box background color. See the `fill` parameter for gpar.

- border_color:

  Box border color. See the `col` parameter for gpar.

- width:

  Width of the box. Must be an object of class unit or a number between
  0 and 1.

- height:

  Height of the box. Must be an object of class unit or a number between
  0 and 1.

- just_exc:

  Justification for the text of the exclude box: `"left"`, `"center"` or
  `"right"`.

- text_color_exc:

  Color of the text of the exclude box. See `text_color`.

- text_fs_exc:

  Font size of the text of the exclude box. See `text_fs`.

- text_fface_exc:

  Font face of the text of the exclude box. See the `fontface` parameter
  for gpar. See `text_fface`.

- text_ffamily_exc:

  Changes the font family of the text of the exclude box. See the
  `fontfamily` parameter for gpar. See `text_ffamily`.

- text_padding_exc:

  Changes the text padding inside the exclude box. This number has to be
  greater than 0.

- bg_fill_exc:

  Exclude box background color. See `bg_fill`.

- border_color_exc:

  Box background color of the exclude box. See `border_color`.

- width_exc:

  Width of the exclude box. Must be an object of class unit or a number
  between 0 and 1.

- height_exc:

  Height of the box. Must be an object of class unit or a number between
  0 and 1.

- text_color_title:

  Color of the title text.

- text_fs_title:

  Font size of the title text.

- text_fface_title:

  Font face of the title text. See the `fontface` parameter for gpar.

- text_ffamily_title:

  Changes the font family of the title text. See the `fontfamily`
  parameter for gpar.

- text_padding_title:

  Changes the title text padding inside the box. This number has to be
  greater than 0.

- bg_fill_title:

  Title box background color.

- border_color_title:

  Title box border color.

- width_title:

  Width of the title box. Must be an object of class unit or a number
  between 0 and 1.

- height_title:

  Height of the title box. Must be an object of class unit or a number
  between 0 and 1.

## Value

List with the dataset and the flowchart parameters with their
modifications.

## Examples

``` r
safo |>
  dplyr::filter(!is.na(group)) |>
  as_fc(label = "Randomized patients") |>
  fc_split(group) |>
  fc_theme(text_fs = 11, text_color = "#324C54", text_fface = 2, bg_fill = "#ADD8E6") |>
  fc_draw()

```
