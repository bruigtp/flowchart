#' @title fc_draw
#' @description This function allows to draw the flowchart from a fc object.
#'
#' @param object fc object that we want to draw.
#' @param big.mark character. Used to specify the thousands separator for patient count values. Defaults is no separator (`""`); if not empty used as mark between every 3 digits (ex: `big.mark = ","` results in `1,000` instead of `1000`).
#' @param box_corners Indicator of whether to draw boxes with round (`"round"`) vs non-round (`"sharp"`) corners. Default is `"round"`.
#' @param arrow_angle The angle of the arrow head in degrees, as in `arrow`.
#' @param arrow_length A unit specifying the length of the arrow head (from tip to base), as in `arrow`.
#' @param arrow_ends One of "last", "first", or "both", indicating which ends of the line to draw arrow heads, as in `arrow`.
#' @param arrow_type One of "open" or "closed" indicating whether the arrow head should be a closed triangle, as in `arrow`.
#' @param title The title of the flowchart. Default is NULL (no title).
#' @param title_x x coordinate for the title. Default is 0.5.
#' @param title_y y coordinate for the title. Default is 0.9.
#' @param title_color Color of the title. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param title_fs Font size of the title. It is 15 by default. See the `fontsize` parameter for \code{\link{gpar}}.
#' @param title_fface Font face of the title. It is 2 by default. See the `fontface` parameter for \code{\link{gpar}}.
#' @param title_ffamily Changes the font family of the title. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}.

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

fc_draw <- function(object, big.mark = "", box_corners = "round", arrow_angle = 30, arrow_length = grid::unit(0.1, "inches"), arrow_ends = "last", arrow_type = "closed", title = NULL, title_x = 0.5, title_y = 0.9, title_color = "black", title_fs = 15, title_fface = 2, title_ffamily = NULL) {

  is_class(object, "fc")
  UseMethod("fc_draw")

}

#' @importFrom rlang .data
#' @export

fc_draw.fc <- function(object, big.mark = "", box_corners = "round", arrow_angle = 30, arrow_length = grid::unit(0.1, "inches"), arrow_ends = "last", arrow_type = "closed", title = NULL, title_x = 0.5, title_y = 0.9, title_color = "black", title_fs = 15, title_fface = 2, title_ffamily = NULL) {

  # Check for valid corners argument
  if (!box_corners %in% c("round", "sharp")) {
    stop("Invalid box_corners argument: must be 'round' or 'sharp'")
  }

  if (box_corners == "round") {
    rect_type <- grid::roundrectGrob
  } else {
    rect_type <- grid::rectGrob
  }

  #Initialize grid
  grid::grid.newpage()

  object0 <- object #to return the object unaltered

  #We have to return the parameters of the function in the attribute of object$fc
  params <- c("arrow_angle", "arrow_length", "arrow_ends", "arrow_type")
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
                            bg = purrr::pmap(list(.data$x, .data$y, .data$text, .data$type, .data$group, .data$just, .data$text_color, .data$text_fs, .data$text_fface, .data$text_ffamily, .data$text_padding, .data$bg_fill, .data$border_color), function(...) {
                              arg <- list(...)
                              names(arg) <- c("x", "y", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "text_padding", "bg_fill", "border_color")
                              Gmisc::boxGrob(arg$text, x = arg$x, y = arg$y, just = arg$just, txt_gp = grid::gpar(col = arg$text_color, fontsize = arg$text_fs/arg$text_padding, fontface = arg$text_fface, fontfamily = arg$text_ffamily, cex = arg$text_padding), box_gp = grid::gpar(fill = arg$bg_fill, col = arg$border_color), box_fn = rect_type)
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
              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id_par]], plot_fc[[i]]$bg[[k]], type = "N", arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))
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
              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id]], plot_fc[[i]]$bg[[k]], type = "vertical", arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))
            }

          }

        } else if(type == "exclude") {

          for(k in ids) {

            print(Gmisc::connectGrob(plot_fc[[i]]$bg[[k - 1]], plot_fc[[i]]$bg[[k]], type = "-", arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))

          }

        } else if(type == "stack") {

          #Find the last box (in height) of the previous flow chart (before stack)
          y_last <- min(plot_fc[[i]] |>
                          dplyr::filter(dplyr::row_number() < ids[1]) |>
                          dplyr::pull(.data$y)
          )

          #Find how many boxes are in the last level
          id_last <- which(plot_fc[[i]]$y == y_last)

          if(length(ids) > length(id_last)) {

            for(k in ids) {

              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id_last]], plot_fc[[i]]$bg[[k]], type = "N", arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))

            }

          } else if (length(ids) < length(id_last)) {

            for(k in id_last) {

              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[k]], plot_fc[[i]]$bg[[ids]], type = "N", arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))

            }


          } else {
            #They have the same number of boxes
            for(k in 1:length(ids)) {
              #vertical connection
              print(Gmisc::connectGrob(plot_fc[[i]]$bg[[id_last[k]]], plot_fc[[i]]$bg[[ids[k]]], type = "vertical", arrow_obj = getOption("connectGrobArrow", default = grid::arrow(angle = arrow_angle, length = arrow_length, ends = arrow_ends, type = arrow_type))))
            }

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
