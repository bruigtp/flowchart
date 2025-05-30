#' @title fc_view
#' @description This function allows you to return either the data stored in `$data` or the flowchart information stored in `$fc`.
#'
#' @param object fc object that we want to access.
#' @param what Choose "data" to return the data associated to the flowchart stored in `$data` or "fc" to return the flowchart information stored in `$fc`.
#' @return Returns a tibble. Either `$data` or `$fc`.
#'
#' @examples
#' #Return the data associated to the flowchart
#' safo |>
#'  as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'  fc_view("data")
#'
#' #Return the flowchart information
#' safo |>
#'  as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'  fc_view("fc")
#'
#' @export

fc_view <- function(object, what) {

  is_class(object, "fc")
  UseMethod("fc_view")

}

#' @export
#' @importFrom rlang .data

fc_view.fc <- function(object, what) {

  if(!what %in% c("data", "fc")) {
    cli::cli_abort("{.arg what} argument must be one of {.val data} or {.val fc}.")
  }

  object[[what]]

}
