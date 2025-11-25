# fc_view

This function allows you to return either the data stored in `$data` or
the flowchart information stored in `$fc`.

## Usage

``` r
fc_view(object, what)
```

## Arguments

- object:

  fc object that we want to access.

- what:

  Choose `"data"` to return the data associated to the flowchart stored
  in `$data` or `"fc"` to return the flowchart information stored in
  `$fc`.

## Value

Returns a tibble. Either `$data` or `$fc`.

## Examples

``` r
#Return the data associated to the flowchart
safo |>
 as_fc(label = "Patients assessed for eligibility") |>
 fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
 fc_view("data")
#> # A tibble: 215 × 21
#>       id inclusion_crit exclusion_crit chronic_heart_failure expected_death_24h
#>    <int> <fct>          <fct>          <fct>                 <fct>             
#>  1     3 No             No             No                    No                
#>  2     5 No             No             No                    No                
#>  3     9 No             No             No                    No                
#>  4    12 No             No             No                    No                
#>  5    14 No             No             No                    No                
#>  6    27 No             No             No                    No                
#>  7    29 No             No             No                    No                
#>  8    32 No             No             No                    No                
#>  9    50 No             No             No                    No                
#> 10    51 No             No             No                    No                
#> # ℹ 205 more rows
#> # ℹ 16 more variables: polymicrobial_bacteremia <fct>,
#> #   conditions_affect_adhrence <fct>, susp_prosthetic_valve_endocard <fct>,
#> #   severe_liver_cirrhosis <fct>, acute_sars_cov2 <fct>,
#> #   blactam_fosfomycin_hypersens <fct>, other_clinical_trial <fct>,
#> #   pregnancy_or_breastfeeding <fct>, previous_participation <fct>,
#> #   myasthenia_gravis <fct>, decline_part <fct>, group <fct>, itt <fct>, …

#Return the flowchart information
safo |>
 as_fc(label = "Patients assessed for eligibility") |>
 fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
 fc_view("fc")
#> # A tibble: 3 × 22
#>      id     x     y     n     N perc  label text_pattern text  type  group just 
#>   <int> <dbl> <dbl> <int> <int> <chr> <chr> <chr>        <chr> <chr> <lgl> <chr>
#> 1     1  0.5  0.667   925   925 100   Pati… "{label}\n{… "Pat… init  NA    cent…
#> 2     2  0.5  0.333   215   925 23.24 Rand… "{label}\n … "Ran… filt… NA    cent…
#> 3     3  0.65 0.5     710   925 76.76 Excl… "{label}\n … "Exc… excl… NA    cent…
#> # ℹ 10 more variables: text_color <chr>, text_fs <dbl>, text_fface <dbl>,
#> #   text_ffamily <lgl>, text_padding <dbl>, bg_fill <chr>, border_color <chr>,
#> #   width <lgl>, height <lgl>, end <lgl>
```
