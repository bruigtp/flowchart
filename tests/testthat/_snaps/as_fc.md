# errors with neither .data nor N

    Code
      as_fc()
    Condition
      Error in `as_fc()`:
      ! Either `.data` or `N` arguments must be specified.

# errors with both .data and N

    Code
      as_fc(mtcars, N = 32)
    Condition
      Error in `as_fc()`:
      ! The `.data` and `N` arguments cannot be specified simultaneously.

# errors when text_padding is zero

    Code
      as_fc(N = 100, text_padding = 0)
    Condition
      Error in `as_fc()`:
      ! Text padding cannot be equal to zero.

# errors on invalid label type

    Code
      as_fc(N = 10, label = 1)
    Condition
      Error in `as_fc()`:
      ! The `label` and `text_pattern` must be either characters or expressions.

# warns and returns NULL fc when hide = TRUE

    Code
      fc <- as_fc(N = 10, hide = TRUE)
    Condition
      Warning:
      `hide = TRUE` can only be combined with `fc_split()`

