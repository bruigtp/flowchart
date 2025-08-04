#' @title fc_draw
#' @description This function allows to draw the flowchart from a fc object.
#'
#' @param object fc object that we want to draw.
#' @param big.mark character. Used to specify the thousands separator for patient count values. Defaults is no separator (`""`); if not empty used as mark between every 3 digits (ex: `big.mark = ","` results in `1,000` instead of `1000`).
#' @param box_corners Indicator of whether to draw boxes with round (`"round"`) vs non-round (`"sharp"`) corners. Default is `"round"`.
#' @param arrow_angle The angle of the arrow head in degrees, as in `arrow`.
#' @param arrow_length A unit specifying the length of the arrow head (from tip to base), as in `arrow`.
#' @param arrow_ends One of `"last"`, `"first"`, or `"both"`, indicating which ends of the line to draw arrow heads, as in [arrow].
#' @param arrow_type One of `"open"` or `"closed"` indicating whether the arrow head should be a closed triangle, as in [arrow].
#' @param arrow_color Color of the arrows. Default is `"black"`. See the `col` parameter for [gpar].
#' @param arrow_lwd Line width of the arrows. Default is 1. See the `lwd` parameter for [gpar].
#' @param arrow_lineend Line end style for arrows. One of `"round"`, `"butt"`, or `"square"`. Default is `"round"`. See the `lineend` parameter for [gpar].
#' @param arrow_linejoin Line join style for arrow heads (i.e., shape of arrow head corners). One of `"round"`, `"mitre"`, or `"bevel"`. Default is `"round"`. See the `linejoin` parameter for [gpar].
#' @param title The title of the flowchart. Default is `NULL` (no title).
#' @param title_x x coordinate for the title. Default is 0.5.
#' @param title_y y coordinate for the title. Default is 0.9.
#' @param title_color Color of the title. It is `"black"` by default. See the `col` parameter for [gpar].
#' @param title_fs Font size of the title. It is 15 by default. See the `fontsize` parameter for [gpar].
#' @param title_fface Font face of the title. It is 2 by default. See the `fontface` parameter for [gpar].
#' @param title_ffamily Changes the font family of the title. Default is `NA`. See the `fontfamily` parameter for [gpar].
#' @param canvas_bg Background color for the entire canvas (the area behind the flowchart boxes). Default is `"white"`. Set to `"transparent"` or `NULL` for a transparent background; `"transparent"` background will only be noticeable when exporting drawn flowcharts via [fc_export()] and is compatible with all [fc_export()] formats except `"jpeg"` and `"bmp"`.

#' @return Invisibly returns the same object that has been given to the function, with the given arguments to draw the flowchart stored in the attributes.
#'
#' @examples
#' safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'   fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'   fc_split(group) |>
#'   fc_filter(itt == "Yes", label = "Included in ITT") |>
#'   fc_filter(pp == "Yes", label = "Included in PP") |>
#'   fc_draw()
#'
#' @export

fc_draw <- function(object, big.mark = "", box_corners = "round", arrow_angle = 30, arrow_length = grid::unit(0.1, "inches"), arrow_ends = "last", arrow_type = "closed", arrow_color = "black", arrow_lwd = 1, arrow_lineend = "round", arrow_linejoin = "round", title = NULL, title_x = 0.5, title_y = 0.9, title_color = "black", title_fs = 15, title_fface = 2, title_ffamily = NULL, canvas_bg = "white") {

  is_class(object, "fc")
  UseMethod("fc_draw")

}

#' @importFrom rlang .data
#' @export

fc_draw.fc <- function(object, big.mark = "", box_corners = "round", arrow_angle = 30, arrow_length = grid::unit(0.1, "inches"), arrow_ends = "last", arrow_type = "closed", arrow_color = "black", arrow_lwd = 1, arrow_lineend = "round", arrow_linejoin = "round", title = NULL, title_x = 0.5, title_y = 0.9, title_color = "black", title_fs = 15, title_fface = 2, title_ffamily = NULL, canvas_bg = "white") {

  # Check for valid corners argument
  if (!box_corners %in% c("round", "sharp")) {
    cli::cli_abort("The {.arg box_corners} argument must be {.val round} or {.val sharp}.")
  }

  if (box_corners == "round") {
    rect_type <- grid::roundrectGrob
  } else {
    rect_type <- grid::rectGrob
  }

  if (!(arrow_lineend %in% c("round", "butt", "square"))) {
    cli::cli_abort("The {.arg arrow_lineend} argument must be {.val round}, {.val butt}, or {.val square}")
  }

  if (!(arrow_linejoin %in% c("round", "mitre", "bevel"))) {
    cli::cli_abort("The {.arg arrow_linejoin} argument must be {.val round}, {.val mitre}, or {.val bevel}")
  }

  #Initialize grid
  grid::grid.newpage()

  # Draw background rectangle covering the entire viewport
  if (canvas_bg != "transparent" && !is.null(canvas_bg)) {
    grid::grid.rect(gp = grid::gpar(fill = canvas_bg, col = NA))
  }

  object0 <- object #to return the object unaltered

  #We have to return the parameters of the function in the attribute of object$fc
  params <- c("big.mark", "box_corners", "arrow_angle", "arrow_length", "arrow_ends", "arrow_type", "arrow_color", "arrow_lwd", "arrow_lineend", "arrow_linejoin", "title", "title_x", "title_y", "title_color", "title_fs", "title_fface", "title_ffamily", "canvas_bg")
  attr_draw <- purrr::map(params, ~get(.x))
  names(attr_draw) <- params

  attr(object0$fc, "draw") <- attr_draw

  if(tibble::is_tibble(object$fc)) object$fc <- list(object$fc)

  # Incorporate the update_numbers helper to update text values based on big.mark:
  if(big.mark != "") {
    object <- update_numbers(object, big.mark = big.mark)
  }

  plot_fc <- purrr::map(object$fc, ~.x |>
                          dplyr::mutate(
                            #Recalculate row number
                            id = dplyr::row_number(),
                            bg = purrr::pmap(list(.data$x, .data$y, .data$text, .data$type, .data$group, .data$just, .data$text_color, .data$text_fs, .data$text_fface, .data$text_ffamily, .data$text_padding, .data$bg_fill, .data$border_color, .data$width, .data$height), function(...) {
                              arg <- list(...)
                              names(arg) <- c("x", "y", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "text_padding", "bg_fill", "border_color", "width", "height")
                              if(!is.na(arg$width) & !is.na(arg$height)) {

                                Gmisc::boxGrob(arg$text, x = arg$x, y = arg$y, just = arg$just, txt_gp = grid::gpar(col = arg$text_color, fontsize = arg$text_fs/arg$text_padding, fontface = arg$text_fface, fontfamily = arg$text_ffamily, cex = arg$text_padding), box_gp = grid::gpar(fill = arg$bg_fill, col = arg$border_color), width = arg$width, height = arg$height, box_fn = rect_type)

                              } else if(!is.na(arg$width)) {

                                Gmisc::boxGrob(arg$text, x = arg$x, y = arg$y, just = arg$just, txt_gp = grid::gpar(col = arg$text_color, fontsize = arg$text_fs/arg$text_padding, fontface = arg$text_fface, fontfamily = arg$text_ffamily, cex = arg$text_padding), box_gp = grid::gpar(fill = arg$bg_fill, col = arg$border_color), width = arg$width, box_fn = rect_type)

                              } else if(!is.na(arg$height)) {

                                Gmisc::boxGrob(arg$text, x = arg$x, y = arg$y, just = arg$just, txt_gp = grid::gpar(col = arg$text_color, fontsize = arg$text_fs/arg$text_padding, fontface = arg$text_fface, fontfamily = arg$text_ffamily, cex = arg$text_padding), box_gp = grid::gpar(fill = arg$bg_fill, col = arg$border_color), height = arg$height, box_fn = rect_type)

                              } else {

                                Gmisc::boxGrob(arg$text, x = arg$x, y = arg$y, just = arg$just, txt_gp = grid::gpar(col = arg$text_color, fontsize = arg$text_fs/arg$text_padding, fontface = arg$text_fface, fontfamily = arg$text_ffamily, cex = arg$text_padding), box_gp = grid::gpar(fill = arg$bg_fill, col = arg$border_color), box_fn = rect_type)

                              }

                            })
                          )
  )

  #Plot the boxes:
  for(i in 1:length(plot_fc)) {
    for(j in 1:nrow(plot_fc[[i]])) {
      print(plot_fc[[i]]$bg[[j]])
    }
  }

  #Plot the connections:
  for(i in 1:length(plot_fc)) {

    #Identify each step of the process of connecting the flowchart
    step <- plot_fc[[i]] |>
      dplyr::distinct(.data$y, .data$type)

    if(nrow(step) > 1) {
      for(j in 2:nrow(step)) {
        ids <- plot_fc[[i]] |>
          dplyr::filter(.data$y == step$y[j], .data$type == step$type[j]) |>
          dplyr::pull(.data$id)

        type <- unique(plot_fc[[i]][["type"]][ids])

        if(type == "split") {

          for(k in ids) {

            group_par <- unlist(stringr::str_split(plot_fc[[i]][["group"]][k], " // "))
            group_par <- paste(utils::head(group_par, -1), collapse = " // ")

            #If there is only one group, the parent box is the right before them
            if(group_par == "") {

              id_par <- plot_fc[[i]] |>
                dplyr::filter(.data$id < min(ids), .data$type != "exclude") |>
                dplyr::last() |>
                dplyr::pull(.data$id)

            } else {

              id_par <- plot_fc[[i]] |>
                dplyr::filter(.data$id < min(ids), .data$group == group_par, .data$type != "exclude") |>
                dplyr::last() |>
                dplyr::pull(.data$id)

            }

            #If it exists because now the initial box can be hided
            if(length(id_par) > 0) {
              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id_par]], plot_fc[[i]]$bg[[k]], type = "N", lty_gp = getOption("connectGrob", default = grid::gpar(col = arrow_color, fill = arrow_color, lwd = arrow_lwd, lineend = arrow_lineend, linejoin = arrow_linejoin)), arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))
            }

          }

        } else if(type == "filter") {

          for(k in ids) {

            #Get the parent box (the last with the same x coordinate)
            id <- plot_fc[[i]] |>
              dplyr::filter(.data$x == plot_fc[[i]][["x"]][k], .data$id < plot_fc[[i]][["id"]][k]) |>
              dplyr::last() |>
              dplyr::pull(.data$id)

            #If it exists because now the initial box can be hided
            if(length(id) > 0) {
              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id]], plot_fc[[i]]$bg[[k]], type = "vertical", lty_gp = getOption("connectGrob", default = grid::gpar(col = arrow_color, fill = arrow_color, lwd = arrow_lwd, lineend = arrow_lineend, linejoin = arrow_linejoin)), arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))
            }

          }

        } else if(type == "exclude") {

          for(k in ids) {

            print(Gmisc::connectGrob(plot_fc[[i]]$bg[[k - 1]], plot_fc[[i]]$bg[[k]], type = "-", lty_gp = getOption("connectGrob", default = grid::gpar(col = arrow_color, fill = arrow_color, lwd = arrow_lwd, lineend = arrow_lineend, linejoin = arrow_linejoin)), arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))

          }

        } else if(type == "stack") {

          #Find the ending boxes of the previous flow chart (before stack)
          id_last <- plot_fc[[i]] |>
                    dplyr::filter(dplyr::row_number() < ids[1], .data$end) |>
                    #Arrange in function of the order they appear in the x-coordinate
                    dplyr::arrange(.data$x) |>
                    dplyr::pull(.data$id)

          if(length(id_last) == 1 & length(ids) > 1) {

            for(k in ids) {

              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id_last]], plot_fc[[i]]$bg[[k]], type = "N", lty_gp = getOption("connectGrob", default = grid::gpar(col = arrow_color, fill = arrow_color, lwd = arrow_lwd, lineend = arrow_lineend, linejoin = arrow_linejoin)), arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))

            }

          } else if (length(ids) == 1 & length(id_last) > 1) {

            for(k in id_last) {

              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[k]], plot_fc[[i]]$bg[[ids]], type = "L", lty_gp = getOption("connectGrob", default = grid::gpar(col = arrow_color, fill = arrow_color, lwd = arrow_lwd, lineend = arrow_lineend, linejoin = arrow_linejoin)), arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))

            }


          } else if (length(ids) == length(id_last)) {
            #They have the same number of boxes
            for(k in 1:length(ids)) {
              #vertical connection
              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id_last[k]]], plot_fc[[i]]$bg[[ids[k]]], type = "vertical", lty_gp = getOption("connectGrob", default = grid::gpar(col = arrow_color, fill = arrow_color, lwd = arrow_lwd, lineend = arrow_lineend, linejoin = arrow_linejoin)), arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))
            }

          } else {
            #It should never enter here because of fc_stack()
            cli::cli_abort("Flowcharts can't be united because they have a different number of boxes in their connecting levels.")
          }

        }

      }
    }

  }

  #Plot title
  if(!is.null(title)) {

    grid::grid.text(title, x = title_x, y = title_y, gp = grid::gpar(col = title_color, fontsize = title_fs, fontface = title_fface, fontfamily = title_ffamily))

  }

  invisible(object0)

}
