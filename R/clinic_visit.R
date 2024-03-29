#' Simulated clinical trial information by visit
#' Simulated dataset of a clinical trial comparing some biomarker values between two randomized groups (control/treatment). This dataset contains the information by visit. It is equivalent to the dataset clinic_patient, which has the same information by patient.
#'
#' @docType data
#' @usage data(clinic_visit)
#'
#' @format A data frame with 411 rows and 8 columns
#'  \describe{
#'   \item{id:}{Identifier of each patient.}
#'   \item{age:}{Age values.}
#'   \item{consent:}{The patient has signed the informed consent?}
#'   \item{group:}{Randomized group: control/treatment.}
#'   \item{visit:}{Number of the corresponding follow-up visit.}
#'   \item{marker:}{Marker value in the visit.}
#' }
#'
#' @keywords datasets
#' @examples
#' clinic_visit |>
#'   dplyr::filter(!is.na(group)) |>
#'   as_fc(label = "Number of visits") |>
#'   fc_split(group) |>
#'   fc_draw()
#'

"clinic_visit"
