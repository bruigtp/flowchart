# quiet_prettyNum

Wrapper for [`prettyNum()`](https://rdrr.io/r/base/formatc.html) that
suppresses the original warning message if user `big.mark` equal to the
character defined in the R environment `OutDec` option.

## Usage

``` r
quiet_prettyNum(x, big.mark)
```

## Arguments

- x:

  an atomic numerical or character object, possibly complex, typically a
  vector of real numbers.

- big.mark:

  character. Used to specify the thousands separator for patient count
  values.
