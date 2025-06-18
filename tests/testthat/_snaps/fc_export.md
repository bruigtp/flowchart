# errors without draw parameters

    Code
      fc_export(fc, "test.png")
    Condition
      Error in `fc_export()`:
      ! Object must be created with `fc_draw()`.

# errors with invalid format

    Code
      fc_export(fc, "test.invalid")
    Condition
      Error in `fc_export()`:
      ! Invalid `format` specified
      i Valid `format` choices are "png", "jpeg", "tiff", "bmp", "svg", and "pdf".

# errors when format doesn't match extension

    Code
      fc_export(fc, "test.png", format = "pdf")
    Condition
      Error in `fc_export()`:
      ! `filename` extension and the specified `format` don't match.

# errors with no extension and no format

    Code
      fc_export(fc, "test")
    Condition
      Error in `fc_export()`:
      ! File `filename` has no extension and format is `NULL`.

# errors with invalid vector format units

    Code
      fc_export(fc, "test.pdf", units = "px")
    Condition
      Error in `fc_export()`:
      ! Invalid units for vector formats. Units must be "in", "cm", or "mm".

# errors with invalid bitmap format units

    Code
      fc_export(fc, "test.png", units = "invalid")
    Condition
      Error in `fc_export()`:
      ! The `units` for bitmap formats must be "in", "cm", "mm", or "px".

# warns about default dimensions for vector formats

    Code
      fc_export(fc, "test.pdf", units = "cm")
    Condition
      Warning:
      If `width` is missing for vector formats ("svg", "pdf"), default `width` is 6 inches.
      Warning:
      If `height` is missing for vector formats ("svg", "pdf"), default `height` is 6 inches.
    Message
      The function `grDevices::cairo_pdf()` is used to export the flowchart.

# warns about default dimensions for bitmap formats

    Code
      fc_export(fc, "test.png", units = "in")
    Condition
      Warning:
      If `width` is missing for bitmap formats, default `width` is 600 pixels.
      Warning:
      If `height` is missing for bitmap formats, default `height` is 600 pixels.
      Error:
      ! One or both dimensions exceed the maximum (50000px).
      - Use `options(ragg.max_dim = ...)` to change the max
        Warning: May cause the R session to crash

