# replace_num_in_expr

Helper function for
[`update_numbers()`](https://bruigtp.github.io/flowchart/reference/update_numbers.md).

## Usage

``` r
replace_num_in_expr(expr, row, big.mark)
```

## Arguments

- expr:

  expression in `fc$text`.

- row:

  A row from the `fc` object containing `n`, `N`, and `perc` values.

- big.mark:

  character. Used to specify the thousands separator for patient count
  values.
