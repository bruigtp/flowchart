#' @title fc_modify
#' @description This function allows to modify the `.$fc` tibble included in each fc object that contains all the parameters of the flowchart.
#'
#' @param object flowchart created as a fc object.
#' @param fun A function or formula that will be applied to `.$fc`. If a _function_, it is used as is. If a _formula_, e.g. `fun = ~.x |> mutate(x = x + 0.2)`, it is converted to a function.
#' @param ... Additional arguments passed on to the mapped function.
#' @return List with the dataset and the modified flowchart parameters.
#'
#' @examples
#' #Example: let's modify the excluded box
#' text_exc <- paste0(
#'   sum(safo$inclusion_crit == "Yes"),
#'   " not met the inclusion criteria\n",
#'   sum(safo$exclusion_crit == "Yes"),
#'   " met the exclusion criteria"
#' )
#'
#' safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'   fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'   fc_modify(
#'     ~ . |>
#'       dplyr::mutate(
#'         text = ifelse(id == 3, text_exc, text),
#'         x = ifelse(id == 3, 0.75, x)
#'       )
#'   ) |>
#'   fc_draw()
#'
#' @export

fc_modify <- function(object, fun, ...) {

  is_class(object, "fc")
  UseMethod("fc_modify")

}

#' @export

fc_modify.fc <- function(object, fun, ...) {

  #Execute function on .$fc

  if(tibble::is_tibble(object$fc)) {

    object$fc <- list(object$fc)
    object$fc <- purrr::map_dfr(object$fc, fun, ...)

  } else {

    object$fc <- purrr::map(object$fc, fun, ...)

  }

  object

}
