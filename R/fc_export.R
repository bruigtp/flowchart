#' @title fc_export
#' @description This function allows you to export the drawn flowchart to the most popular graphic formats, including bitmap formats (png, jpeg, tiff, bmp) and vector formats (svg, pdf). For bitmap formats, it uses the `ragg` package devices when available for higher performance and higher quality output than standard raster devices provide by `grDevices`.
#'
#' @details
#' - **Vector Formats ('svg', 'pdf'):** These formats are ideal for graphics that need to be scaled without loss of quality. The default units for width and height are inches. If user specifies `units` other than inches, the function will convert the dimensions to inches using standard conversion formulas for "mm" and "cm" `units`; "px" conversion to inches is calculated using the specified dimension divided by `res`.
#' - **Bitmap Formats ('png', 'jpeg', 'tiff', 'bmp'):** For these formats, the function uses the `ragg` package devices when available, providing higher performance and higher quality output. The default units for width and height are pixels.
#' - **Suggested Dependencies:** For superior performance and quality bitmap outputs, it is recommended to install the `ragg` package. For exporting to 'pdf' format with enhanced features, the Cairo graphics library will be used if it is available.
#'
#' @param object fc object that we want to export.
#' @param filename File name to create on disk.
#' @param path Path of the directory to save plot to: path and filename are combined to create the fully qualified file name. Defaults to the working directory.
#' @param format Name of the graphic device. One of 'png', 'jpeg', 'tiff', 'bmp', 'svg', or 'pdf'. If `NULL` (default), the format is guessed based on the filename extension.
#' @param width,height Plot size in units expressed by the `units` argument. Default is 600px for bitmap formats and 6 inches for vector formats.
#' @param units One of the following units in which the width and height arguments are expressed: "in", "cm", "mm" or "px". Defaults are "px" for bitmap formats and "in" for vector formats.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels. Default is 100.
#' @return Invisibly returns the same object that has been given to the function.
#'
#' @examples
#' \dontrun{
#' safo |>
#'  as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'  fc_draw() |>
#'  fc_export("flowchart.png")
#'
#' #Specifying size and resolution
#' safo |>
#'  as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'  fc_draw() |>
#'  fc_export("flowchart.png", width = 2500, height = 2000, res = 700)
#'
#' #Exporting to an SVG file
#' safo |>
#'  as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'  fc_draw() |>
#'  fc_export("flowchart.svg")
#'
#' #Exporting to a PDF file
#' safo |>
#'  as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'  fc_draw() |>
#'  fc_export("flowchart.pdf")
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
  } else {
    #Put format to filename if format is specified
    format_file <- tolower(tools::file_ext(filename))
    if(identical(format_file, "")) {
      filename <- paste0(filename, ".", format)
    } else {
      if(!identical(format_file, format)) {
        stop("filename extension and the specified format don't match")
      }
    }
  }

  #If format is not one of 'png', 'jpeg', 'tiff', 'bmp', 'svg', or 'pdf':
  valid_formats <- c("png", "jpeg", "tiff", "bmp", "svg", "pdf")
  if(! format %in% valid_formats) {
    stop(paste("The format has to be one of the following:", paste(valid_formats, collapse = ", ")))
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  #Handle units and default dimensions
  units_conv <- c("in", "cm", "mm", "px")
  if (!(units %in% units_conv)) {
    stop("Invalid units. Units must be one of 'in', 'cm', 'mm', 'px'.")
  }

  #Set default dimensions based on format
  if (format %in% c("svg", "pdf")) {
    #For vector formats, default dimensions in inches
    if (is.na(width)) {
      width <- 6
      if (units != "in") {
        warning("For vector formats (svg, pdf), default width is in inches.")
        units <- "in"
      }
    }
    if (is.na(height)) {
      height <- 6
      if (units != "in") {
        warning("For vector formats (svg, pdf), default height is in inches.")
        units <- "in"
      }
    }
    #Convert units to inches if necessary
    width_in <- switch(units,
                       "in" = width,
                       "cm" = width / 2.54,
                       "mm" = width / 25.4,
                       "px" = width / res)
    height_in <- switch(units,
                        "in" = height,
                        "cm" = height / 2.54,
                        "mm" = height / 25.4,
                        "px" = height / res)
    #Open the appropriate device
    if (format == "svg") {
      grDevices::svg(
        filename = filename,
        width = width_in,
        height = height_in
        )
    } else if (format == "pdf") {
      if (capabilities("cairo")) {
        grDevices::cairo_pdf(
          filename = filename,
          width = width_in,
          height = height_in
        )
      } else {
        warning("Cairo graphics library is not available. Falling back to `grDevices::pdf()`.")
        grDevices::pdf(
          file = filename,
          width = width_in,
          height = height_in
        )
      }
    }
  } else {
    #For bitmap formats
    if (is.na(width)) {
      width <- 600
      if(units != "px") {
        warning("If width is missing, the default units are pixels.")
        units <- "px"
      }
    }
    if(is.na(height)) {
      height <- 600
      if(units != "px") {
        warning("If height is missing, the default units are pixels.")
        units <- "px"
      }
    }
    #Open the bitmap device, using ragg-based devices when available
    #Map formats to device functions explicitly
    if (format %in% c("png", "jpeg", "tiff")) {
      if (rlang::is_installed("ragg")) {
        device_fun <- switch(format,
                             png = ragg::agg_png,
                             jpeg = ragg::agg_jpeg,
                             tiff = ragg::agg_tiff)
      } else {
        warning(" Defaulting to `grDevices` package since `ragg` is not installed.\n   Consider installing the `ragg` package for higher quality png, jpeg, and tiff images.")
        device_fun <- switch(format,
                             png = grDevices::png,
                             jpeg = grDevices::jpeg,
                             tiff = grDevices::tiff)
      }
    } else {
      device_fun <- switch(format, bmp = grDevices::bmp)
    }
    device_fun(filename = filename, width = width, height = height, units = units, res = res)
  }

  #Redraw the plot
  object |>
    fc_draw(arrow_angle = params$arrow_angle, arrow_length = params$arrow_length, arrow_ends = params$arrow_ends, arrow_type = params$arrow_type)

  grDevices::dev.off()

  invisible(object)
}
