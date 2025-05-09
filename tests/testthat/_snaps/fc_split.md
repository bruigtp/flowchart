# errors with neither var nor N

    Code
      fc_split(fc)
    Condition
      Error in `fc_split()`:
      ! A `var` or `N` argument must be specified.

# errors with both var and N

    Code
      fc_split(fc, var = "group", N = 5)
    Condition
      Error in `fc_split()`:
      ! Arguments `var` and `N` cannot be specified simultaneously.

# errors when text_padding is zero

    Code
      fc_split(fc, N = c(5, 5), text_padding = 0)
    Condition
      Error in `fc_split()`:
      ! Text padding cannot be equal to zero.

# errors with invalid label type

    Code
      fc_split(fc, N = c(5, 5), label = 1)
    Condition
      Error in `fc_split()`:
      ! The `label` and `text_pattern` must be either characters or expressions.

# errors when sel_group used without previous split

    Code
      fc_split(fc, N = c(5, 5), sel_group = "A")
    Condition
      Error in `fc_split()`:
      ! The `sel_group` argument can't be used because no groups exist in the flowchart, as no previous split has been performed.

