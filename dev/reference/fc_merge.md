# fc_merge

This function allows to combine horizontally two different flowcharts.

## Usage

``` r
fc_merge(fcs)
```

## Arguments

- fcs:

  list with all the flowcharts that we want to merge

## Value

List containing a list with the datasets belonging to each flowchart and
another list with each of the flowcharts parameters to merge.

## Examples

``` r
# Create first flowchart for ITT
fc1 <- safo |>
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(itt == "Yes", label = "Intention to treat (ITT)")


# Create second flowchart for PP
fc2 <- safo |>
  as_fc(label = "Patients assessed for eligibility") |>
 fc_filter(pp == "Yes", label = "Per protocol (PP)")

list(fc1, fc2) |>
  fc_merge() |>
  fc_draw()

```
