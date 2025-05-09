# errors with neither filter nor N

    Code
      fc_filter(fc)
    Condition
      Error in `fc_filter()`:
      ! Either `filter` or `N` arguments must be specified.

# errors with both filter and N

    Code
      fc_filter(fc, filter = TRUE, N = 5)
    Condition
      Error in `fc_filter()`:
      ! The `filter` and `N` arguments cannot be specified simultaneously.

# errors when text_padding is zero

    Code
      fc_filter(fc, filter = TRUE, text_padding = 0)
    Condition
      Error in `fc_filter()`:
      ! Text padding cannot be equal to zero.

---

    Code
      fc_filter(fc, filter = TRUE, text_padding_exc = 0)
    Condition
      Error in `fc_filter()`:
      ! Text padding cannot be equal to zero.

# errors when N is too large

    Code
      fc_filter(fc, N = 20)
    Condition
      Error in `fc_filter()`:
      ! The number of rows after the filter specified in `N` cannot exceed the original number of rows.

# errors with invalid label type

    Code
      fc_filter(fc, filter = TRUE, label = 1)
    Condition
      Error in `fc_filter()`:
      ! The `label` and `text_pattern` must be either characters or expressions.

# errors with invalid label_exc type

    Code
      fc_filter(fc, filter = TRUE, show_exc = TRUE, label_exc = 1)
    Condition
      Error in `fc_filter()`:
      ! The `label_exc` and `text_pattern_exc` must be either characters or expressions.

# errors when sel_group used without groups

    Code
      fc_filter(fc, filter = TRUE, sel_group = "A")
    Condition
      Error in `fc_filter()`:
      ! Cannot supply `sel_group` because no groups exist in the flowchart yet, as no previous split has been performed.

# errors informatively with nonexistent group

    Code
      fc_filter(fc, filter = TRUE, sel_group = "C")
    Condition
      Error in `fc_filter()`:
      ! Cannot supply `sel_group` because no groups exist in the flowchart yet, as no previous split has been performed.

