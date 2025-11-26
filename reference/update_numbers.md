# update_numbers

Updates values of `n` or `N` referenced in the `text` column when user
specifies `big.mark` argument in `fc_draw`.

## Usage

``` r
update_numbers(object, big.mark = "")
```

## Arguments

- object:

  fc object that we want to draw.

- big.mark:

  character. Used to specify the thousands separator for patient count
  values. Defaults is no separator (`""`); if not empty used as mark
  between every 3 digits (ex: `big.mark = ","` results in `1,000`
  instead of `1000`).
