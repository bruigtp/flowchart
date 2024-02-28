#' @title fc_merge
#' @description This function allows to combine horizontally two different flowcharts.
#'
#' @param fcs list with all the flowcharts that we want to merge
#' @return List containing a list with the datasets belonging to each flowchart and another list with each of the flowcharts parameters to merge.
#'
#' @examples
#' #Create first flowchart for patients
#' fc1 <- clinic_patient |>
#'   dplyr::filter(!is.na(group)) |>
#'   as_fc(label = "Patients included") |>
#'   fc_split(group)
#'
#' #Create second flowchart for visits
#' fc2 <- clinic_visit |>
#'   dplyr::filter(!is.na(group)) |>
#'   as_fc(label = "Number of visits") |>
#'   fc_split(group)
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
