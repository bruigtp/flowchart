#' @title fc_modify
#' @description This function allows to modify the `.$fc` tibble included in each fc object that contains all the parameters of the flowchart.
#'
#' @param object flowchart created as a fc object.
#' @param fun A function or formula that will be applied to `.$fc`. If a _function_, it is used as is. If a _formula_, e.g. `fun = ~.x \%>\% mutate(x = x + 0.2)`, it is converted to a function.
#' @param ... Additional arguments passed on to the mapped function.
#' @examples
#' #Example 1 (change text):
#' clinic_patient %>%
#'   as_fc(label = "Patients included") %>%
#'   fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) %>%
#'   fc_modify(~.x %>%
#'               dplyr::mutate(
#'                 text = dplyr::case_when(
#'                   id == 3 ~ stringr::str_glue("Excluded patients:
#'                                      - {sum(clinic_patient$age < 18)} under-age
#'                                      - {sum(clinic_patient$consent == 'No')} no consent
#'                                      "),
#'                   TRUE ~ text
#'                 )
#'               )) %>%
#'   fc_draw()
#'
#' #Example 2 (change coordinates):
#' clinic_patient %>%
#'   as_fc(label = "Patients included") %>%
#'   fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) %>%
#'   fc_modify(~.x %>%
#'              dplyr::mutate(
#'                x = dplyr::case_when(
#'                  id == 3 ~ 0.8,
#'                  TRUE ~ x
#'                ),
#'                y = dplyr::case_when(
#'                  id == 1 ~ 0.85,
#'                  id == 2 ~ 0.15,
#'                  id == 3 ~ 0.5
#'                )
#'              )) %>%
#'    fc_draw()
#' @export

fc_modify <- function(object, fun, ...) {

  is_class(object, "fc")

  #Execute function on .$fc

  if(tibble::is_tibble(object$fc)) {

    object$fc <- list(object$fc)
    object$fc <- purrr::map_dfr(object$fc, fun, ...)

  } else {

    object$fc <- purrr::map(object$fc, fun, ...)

  }

  object

}
