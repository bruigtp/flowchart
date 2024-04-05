#' @title fc_export
#' @description This function allows you to export the drawn flowchart to the most popular image formats (png, jpeg, tiff).
#'
#' @param object fc object that we want to export.
#' @param filename File name to create on disk.
#' @param path Path of the directory to save plot to: path and filename are combined to create the fully qualified file name. Defaults to the working directory.
#' @param format Format to export the image. One of 'png', 'jpeg' or 'tiff'. If NULL (default), the format is guessed based on the filename extension.
#' @param width,height Plot size in units expressed by the `units` argument. Default is 600px.
#' @param units One of the following units in which the width and height arguments are expressed: "in", "cm", "mm" or "px". Default is "px".
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels. Default is 100.
#' @return Invisibly returns the same object that has been given to the function.
#'
#' @examples
#' \dontrun{
#' clinic_patient |>
#'   as_fc(label = "Available patients") |>
#'   fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) |>
#'   fc_split(group) |>
#'   fc_filter(n_visits == 2, label = "Two visits available", show_exc = TRUE) |>
#'   fc_split(marker_alt, label = c("Marker not alterated", "Marker alterated")) |>
#'   fc_draw() |>
#'   fc_export("flowchart.png")
#'
#' }
#' @export
#' @importFrom rlang .data

fc_export <- function(object, filename, path = NULL, format = NULL, width = NA, height = NA, units = "px", res = 100) {

  is_class(object, "fc")

  #Get parameters from the previously drawn object
  params <- attr(object$fc, "draw")
  if(is.null(params)) {
    stop("Expecting object created with fc_draw()")
  }

  #Get format from filename if not specified
  if (is.null(format)) {
    format <- tolower(tools::file_ext(filename))
    if (identical(format, "")) {
      stop("filename has no file extension and format is NULL")
    }
  }

  #If format is not one of 'png', 'jpeg' or 'tiff':
  if(! format %in% c("png", "jpeg", "tiff")) {
    stop("The format has to be one of png, jpeg or tiff")
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  if (is.na(width)) {
    width <- 600
    if(units != "px") {
      warning("If width is missing the default units are taken in pixels.")
      units <- "px"
    }
  }

  if(is.na(height)) {
    height <- 600
    if(units != "px") {
      warning("If height is missing the default units are taken in pixels.")
      units <- "px"
    }
  }

  grDevices::dev.copy(device = get(format), filename = filename, width = width, height = height, units = units, res = res)

  object |>
    fc_draw(arrow_angle = params$arrow_angle, arrow_length = params$arrow_length, arrow_ends = params$arrow_ends, arrow_type = params$arrow_type)

  grDevices::dev.off()

  invisible(object)
}
