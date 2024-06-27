#' @title fc_draw
#' @description This function allows to draw the flowchart from a fc object.
#'
#' @param object fc object that we want to draw.
#' @param arrow_angle The angle of the arrow head in degrees, as in `arrow`.
#' @param arrow_length A unit specifying the length of the arrow head (from tip to base), as in `arrow`.
#' @param arrow_ends One of "last", "first", or "both", indicating which ends of the line to draw arrow heads, as in `arrow`.
#' @param arrow_type One of "open" or "closed" indicating whether the arrow head should be a closed triangle, as in `arrow`.
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
#' @importFrom rlang .data

fc_draw <- function(object, arrow_angle = 30, arrow_length = grid::unit(0.1, "inches"), arrow_ends = "last", arrow_type = "closed") {

  is_class(object, "fc")

  #Initialize grid
  grid::grid.newpage()

  object0 <- object #to return the object unaltered

  #We have to return the parameters of the function in the attribute of object$fc
  params <- c("arrow_angle", "arrow_length", "arrow_ends", "arrow_type")
  attr_draw <- purrr::map(params, ~get(.x))
  names(attr_draw) <- params

  attr(object0$fc, "draw") <- attr_draw

  if(tibble::is_tibble(object$fc)) object$fc <- list(object$fc)
  plot_fc <- purrr::map(object$fc, ~.x |>
                          dplyr::mutate(
                            bg = purrr::pmap(list(.data$x, .data$y, .data$text, .data$type, .data$group, .data$just, .data$text_color, .data$text_fs, .data$text_fface, .data$text_ffamily, .data$bg_fill, .data$border_color), function(...) {
                              arg <- list(...)
                              names(arg) <- c("x", "y", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "bg_fill", "border_color")
                              fs <- dplyr::case_when(
                                arg$type == "exclude" ~ 6,
                                TRUE ~ 8
                              )
                              Gmisc::boxGrob(arg$text, x = arg$x, y = arg$y, just = arg$just, txt_gp = grid::gpar(col = arg$text_color, fontsize = arg$text_fs, fontface = arg$text_fface, fontfamily = arg$text_ffamily), box_gp = grid::gpar(fill = arg$bg_fill, col = arg$border_color))
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
    step <- object$fc[[i]] |>
      dplyr::distinct(.data$y, .data$type)

    if(nrow(step) > 1) {
      for(j in 2:nrow(step)) {
        ids <- object$fc[[i]] |>
          dplyr::mutate(id = dplyr::row_number()) |>
          dplyr::filter(.data$y == step$y[j], .data$type == step$type[j]) |>
          dplyr::pull(.data$id)

        type <- unique(object$fc[[i]][["type"]][ids])

        if(type == "split") {

          for(k in ids) {

            group_par <- unlist(stringr::str_split(object$fc[[i]][["group"]][k], ", "))
            group_par <- paste(utils::head(group_par, -1), collapse = ", ")

            #If there is only one group, the parent box is the right before them
            if(group_par == "") {

              id_par <- object$fc[[i]] |>
                dplyr::mutate(id = dplyr::row_number()) |>
                dplyr::filter(.data$id < min(ids), .data$type != "exclude") |>
                dplyr::last() |>
                dplyr::pull(.data$id)

            } else {

              id_par <- object$fc[[i]] |>
                dplyr::mutate(id = dplyr::row_number()) |>
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
            id <- object$fc[[i]] |>
              dplyr::mutate(id = dplyr::row_number()) |>
              dplyr::filter(.data$x == object$fc[[i]][["x"]][k], .data$id < min(ids)) |>
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
          y_last <- min(object$fc[[i]] |>
                          dplyr::filter(dplyr::row_number() < ids[1]) |>
                          dplyr::pull(.data$y)
          )

          #Find how many boxes are in the last level
          id_last <- which(object$fc[[i]]$y == y_last)

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

  invisible(object0)

}
