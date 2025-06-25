# warns when big.mark equals OutDec

    Code
      update_numbers(fc, big.mark = ".")
    Condition
      Warning:
      You have set `big.mark` equal to your environment's `OutDec` ('.') - it can be confusing if your flowchart uses the same mark for both.
      i Consider an alternative decimal mark.
      > To change the decimal mark, run: `options(OutDec = "<alternative decimal mark>")`
    Output
      $data
      # A tibble: 1,000 x 1
            id
         <int>
       1     1
       2     2
       3     3
       4     4
       5     5
       6     6
       7     7
       8     8
       9     9
      10    10
      # i 990 more rows
      
      $fc
      # A tibble: 1 x 22
           id     x     y     n     N perc  label text_pattern text  type  group just 
        <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <chr> <chr>        <chr> <chr> <lgl> <chr>
      1     1   0.5   0.5  1000  1000 100   Init~ "{label}\n{~ "Ini~ init  NA    cent~
      # i 10 more variables: text_color <chr>, text_fs <dbl>, text_fface <dbl>,
      #   text_ffamily <lgl>, text_padding <dbl>, bg_fill <chr>, border_color <chr>,
      #   width <lgl>, height <lgl>, end <lgl>
      
      attr(,"class")
      [1] "fc"

