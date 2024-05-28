#' @title fc_merge
#' @description This function allows to combine horizontally two different flowcharts.
#'
#' @param fcs list with all the flowcharts that we want to merge
#' @return List containing a list with the datasets belonging to each flowchart and another list with each of the flowcharts parameters to merge.
#'
#' @examples
#' # Create first flowchart for ITT
#' fc1 <- safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'   fc_filter(itt == "Yes", label = "Intention to treat (ITT)")
#'
#'
#' # Create second flowchart for PP
#' fc2 <- safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(pp == "Yes", label = "Per protocol (PP)")
#'
#' list(fc1, fc2) |>
#'   fc_merge() |>
#'   fc_draw()
#'
#' @export
#' @importFrom rlang .data

fc_merge <- function(fcs) {

  purrr::map(fcs, ~is_class(.x, "fc"))

  object <- tibble::tibble(
    id = 1:length(fcs),
    data = purrr::map(fcs, ~.x$data),
    fc = purrr::map(fcs, ~.x$fc)
  ) |>
    dplyr::mutate(
      fc = purrr::map(seq_along(.data$fc), function(i) {
        .data$fc[[i]] |>
          dplyr::mutate(
            x = update_x(.data$x, i, length(fcs))
          )


      })
    )

  class(object) <- "fc"

  object

}
