#' @title fc_export
#' @description This function allows you to export the drawn flowchart to the most popular graphic formats, including bitmap formats (png, jpeg, tiff, bmp) and vector formats (svg, pdf). For bitmap formats, it uses the `ragg` package devices when available for higher performance and higher quality output than standard raster devices provide by `grDevices`.
#'
#' @details
#' - **Vector Formats ('svg', 'pdf'):** These formats are ideal for graphics that need to be scaled without loss of quality. The default units for width and height are inches. If user specifies `units` other than inches ("mm" or "cm"), the function will convert the dimensions to inches using standard conversion formulas.
#' - **Bitmap Formats ('png', 'jpeg', 'tiff', 'bmp'):** For these formats (with the exception of 'bmp'), the function uses the `ragg` package devices when available, providing higher performance and higher quality output. The default units for width and height are pixels.
#' - **Suggested Dependencies:** For superior performance and quality bitmap outputs, it is recommended to install the `ragg` package. For exporting to 'pdf' format with enhanced features, the Cairo graphics library will be used if it is available.
#'
#' @param object fc object that we want to export.
#' @param filename File name to create on disk.
#' @param path Path of the directory to save plot to: path and filename are combined to create the fully qualified file name. Defaults to the working directory.
#' @param format Name of the graphic device. One of 'png', 'jpeg', 'tiff', 'bmp', 'svg', or 'pdf'. If `NULL` (default), the format is guessed based on the filename extension.
#' @param width,height Plot size in units expressed by the `units` argument. Default is 600px for bitmap formats and 6 inches for vector formats.
#' @param units One of the following units in which the width and height arguments are expressed: "in", "cm", "mm" for vector formats and "in", "cm", "mm" or "px" for bitmap formats. If left `NULL` (default), the function will automatically use "px" for bitmap formats and "in" for vector formats.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels. Default is 100 if exporting in bitmap format. This argument is unused if exporting to a vector format.
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
#'  fc_export("flowchart.png", width = 3000, height = 4000, res = 700)
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

fc_export <- function(object, filename, path = NULL, format = NULL, width = NA, height = NA, units = NULL, res = 100) {

  is_class(object, "fc")
  UseMethod("fc_export")

}

#' @importFrom rlang .data
#' @export

fc_export.fc <- function(object, filename, path = NULL, format = NULL, width = NA, height = NA, units = NULL, res = 100) {

  #Get parameters from the previously drawn object
  params <- attr(object$fc, "draw")
  if(is.null(params)) {
    cli::cli_abort("Object must be created with {.fn fc_draw}.")
  }

  #Get format from filename if not specified
  if (is.null(format)) {
    format <- tolower(tools::file_ext(filename))
    if (identical(format, "")) {
      cli::cli_abort("File {.arg filename} has no extension and format is {.code NULL}.")
    }
  } else {
    #Put format to filename if format is specified
    format_file <- tolower(tools::file_ext(filename))
    if(identical(format_file, "")) {
      filename <- paste0(filename, ".", format)
    } else {
      if(!identical(format_file, format)) {
        cli::cli_abort("{.arg filename} extension and the specified {.arg format} don't match.")
      }
    }
  }

  #If format is not one of 'png', 'jpeg', 'tiff', 'bmp', 'svg', or 'pdf':
  valid_formats <- c("png", "jpeg", "tiff", "bmp", "svg", "pdf")
  if(! format %in% valid_formats) {
    cli::cli_abort(
      c("Invalid {.arg format} specified",
        "i" = "Valid {.arg format} choices are {.val {valid_formats}}.")
        )
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  # Set default units based on format if units is NULL (default)
  if (is.null(units)) {
    if (format %in% c("svg", "pdf")) {
      units <- "in"
    } else {
      units <- "px"
    }
  }

  # Handle units and default dimensions
  if (format %in% c("svg", "pdf")) {
    # For vector formats, units cannot be 'px'
    units_conv <- c("in", "cm", "mm")
    if (!(units %in% units_conv)) {
      cli::cli_abort("Invalid units for vector formats. Units must be {.or {.val in}, {.val cm}, or {.val mm}}.")
    }
    # Set default dimensions if missing width in inches and alert user if they specified different unit type
    if (is.na(width)) {
      width <- 6
      if (units != "in") {
        cli::cli_warn("If {.arg width} is missing for vector formats ({.val svg}, {.val pdf}), default {.arg width} is 6 inches.")
      }
    }
    # Set default dimensions if missing height in inches and alert user if they specified different unit type
    if (is.na(height)) {
      height <- 6
      if (units != "in") {
        cli::cli_warn("If {.arg height} is missing for vector formats ({.val svg}, {.val pdf}), default {.arg height} is 6 inches.")
      }
    }
    # Convert units to inches if necessary
    width_in <- switch(units,
                       "in" = width,
                       "cm" = width / 2.54,
                       "mm" = width / 25.4)
    height_in <- switch(units,
                        "in" = height,
                        "cm" = height / 2.54,
                        "mm" = height / 25.4)
    # Open the appropriate device
    if (format == "svg") {
      grDevices::svg(
        filename = filename,
        width = width_in,
        height = height_in,
        bg = params$canvas_bg  # Original default for grDevices::svg() is "white"
      )
    } else if (format == "pdf") {
      if (capabilities("cairo")) {
        grDevices::cairo_pdf(
          filename = filename,
          width = width_in,
          height = height_in,
          bg = params$canvas_bg  # Original default for grDevices::pdf() is "white"
        )
      } else {
        cli::cli_warn("Cairo graphics library is not available. Falling back to {.fn grDevices::pdf}.")
        grDevices::pdf(
          file = filename,
          width = width_in,
          height = height_in,
          bg = params$canvas_bg    # Original default for grDevices::pdf() is "transparent"
        )
      }
    }
  } else {
    # For bitmap formats, units can be 'in', 'cm', 'mm', or 'px'
    units_conv <- c("in", "cm", "mm", "px")
    if (!(units %in% units_conv)) {
      cli::cli_abort("The {.arg units} for bitmap formats must be {.val in}, {.val cm}, {.val mm}, or {.val px}.")
    }
    # Set default dimensions if missing
    if (is.na(width)) {
      width <- 600
      if (units != "px") {
        cli::cli_warn("If {.arg width} is missing for bitmap formats, default {.arg width} is 600 pixels.")
      }
    }
    if (is.na(height)) {
      height <- 600
      if (units != "px") {
        cli::cli_warn("If {.arg height} is missing for bitmap formats, default {.arg height} is 600 pixels.")
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
        cli::cli_warn(
          c(
            "Defaulting to {.pkg grDevices} package since {.pkg ragg} is not installed.",
            "i" = "Consider installing the {.pkg ragg} package for higher quality {.val png}, {.val jpeg}, and {.val tiff} images."
          )
        )
        device_fun <- switch(format,
                             png = grDevices::png,
                             jpeg = grDevices::jpeg,
                             tiff = grDevices::tiff)
      }
    } else {
      device_fun <- switch(format, bmp = grDevices::bmp)
    }

    # If canvas_bg == "transparent" or NULL and bitmap format supports transparency, set to "transparent"
    if (params$canvas_bg == "transparent" || is.null(params$canvas_bg)) {
      # Add transparency support for PNG and TIFF
      if (format %in% c("png", "tiff")) {
        device_fun(filename = filename, width = width, height = height, units = units, res = res, bg = "transparent")
      } else {
        # JPEG and bmp does not support transparency - warn user and fallback on device default
        device_fun(filename = filename, width = width, height = height, units = units, res = res)
        warning(" \"jpeg\" and \"bmp\" formats do not support transparent `canvas_bg`, falling back to \"white\"")
      }
    } else {
      # Normal case with a background color ("white" or otherwise)
      device_fun(filename = filename, width = width, height = height, units = units, res = res, bg = params$canvas_bg)
    }
  }

  #Redraw the plot
  object |>
    fc_draw(arrow_angle = params$arrow_angle, arrow_length = params$arrow_length, arrow_ends = params$arrow_ends, arrow_type = params$arrow_type, canvas_bg = params$canvas_bg)

  grDevices::dev.off()

  invisible(object)
}
