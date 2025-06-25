#' @title fc_theme
#' @description This function allows you to change the appearance of all boxes of a flowchart at once.
#'
#' @param object fc object.
#' @param text_pattern Text pattern for all the boxes, except the initial and exclusion ones.
#' @param text_pattern_init Text pattern for the initial box.
#' @param text_pattern_exc Text pattern for the exclusion box.
#' @param just Justification for the text: left, center or right.
#' @param text_color Color of the text. See the `col` parameter for \code{\link{gpar}}.
#' @param text_fs Font size of the text. See the `fontsize` parameter for \code{\link{gpar}}.
#' @param text_fface Font face of the text. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily Changes the font family of the text. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding Changes the text padding inside the box. This number has to be greater than 0.
#' @param bg_fill Box background color. See the `fill` parameter for \code{\link{gpar}}.
#' @param border_color Box border color. See the `col` parameter for \code{\link{gpar}}.
#' @param width Width of the box. Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param height Height of the box. Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param just_exc Justification for the text of the exclude box: left, center or right.
#' @param text_color_exc Color of the text of the exclude box. See `text_color`.
#' @param text_fs_exc Font size of the text of the exclude box. See `text_fs`.
#' @param text_fface_exc Font face of the text of the exclude box. See the `fontface` parameter for \code{\link{gpar}}. See `text_fface`.
#' @param text_ffamily_exc Changes the font family of the text of the exclude box. See the `fontfamily` parameter for \code{\link{gpar}}. See `text_ffamily`.
#' @param text_padding_exc Changes the text padding inside the exclude box. This number has to be greater than 0.
#' @param bg_fill_exc Exclude box background color. See `bg_fill`.
#' @param border_color_exc Box background color of the exclude box. See `border_color`.
#' @param width_exc Width of the exclude box. Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param height_exc Height of the box. Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param text_color_title Color of the title text.
#' @param text_fs_title Font size of the title text.
#' @param text_fface_title Font face of the title text. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily_title Changes the font family of the title text. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding_title Changes the title text padding inside the box. This number has to be greater than 0.
#' @param bg_fill_title Title box background color.
#' @param border_color_title Title box border color.
#' @param width_title Width of the title box. Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param height_title Height of the title box. Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#'
#' @return List with the dataset and the flowchart parameters with their modifications.
#'
#' @examples
#' safo |>
#'   dplyr::filter(!is.na(group)) |>
#'   as_fc(label = "Randomized patients") |>
#'   fc_split(group) |>
#'   fc_theme(text_fs = 11, text_color = "#324C54", text_fface = 2, bg_fill = "#ADD8E6") |>
#'   fc_draw()
#'
#' @export

fc_theme <- function(object, text_pattern = NULL, text_pattern_init = NULL, text_pattern_exc = NULL, just = NULL, text_color = NULL, text_fs = NULL, text_fface = NULL, text_ffamily = NULL, text_padding = NULL, bg_fill = NULL, border_color = NULL, width = NULL, height = NULL, just_exc = NULL, text_color_exc = NULL, text_fs_exc = NULL, text_fface_exc = NULL, text_ffamily_exc = NULL, text_padding_exc = NULL, bg_fill_exc = NULL, border_color_exc = NULL, width_exc = NULL, height_exc = NULL, text_color_title = NULL, text_fs_title = NULL, text_fface_title = NULL, text_ffamily_title = NULL, text_padding_title = NULL, bg_fill_title = NULL, border_color_title = NULL, width_title = NULL, height_title = NULL) {

  is_class(object, "fc")
  UseMethod("fc_theme")

}

fc_theme <- function(object, text_pattern = NULL, text_pattern_init = NULL, text_pattern_exc = NULL, just = NULL, text_color = NULL, text_fs = NULL, text_fface = NULL, text_ffamily = NULL, text_padding = NULL, bg_fill = NULL, border_color = NULL, width = NULL, height = NULL, just_exc = NULL, text_color_exc = NULL, text_fs_exc = NULL, text_fface_exc = NULL, text_ffamily_exc = NULL, text_padding_exc = NULL, bg_fill_exc = NULL, border_color_exc = NULL, width_exc = NULL, height_exc = NULL, text_color_title = NULL, text_fs_title = NULL, text_fface_title = NULL, text_ffamily_title = NULL, text_padding_title = NULL, bg_fill_title = NULL, border_color_title = NULL, width_title = NULL, height_title = NULL) {

  #Change text patterns:

  #In all boxes (except init, exc and title_split)
  if(!is.null(text_pattern)) {

    id_all <- which(!object$fc$type %in% c("init", "exclude", "title_split"))

    object$fc$text_pattern[id_all] <- if(is.expression(text_pattern)) list(text_pattern) else text_pattern

    for(i in id_all) {

      label_all <- object$fc$label[[i]]

      if(is.expression(label_all) | is.expression(text_pattern)) {

        if(is.expression(text_pattern)) {

          text_pattern_exp <- gsub("\\{label\\}", "", as.character(text_pattern)) |>
            stringr::str_glue(.envir = rlang::as_environment(object$fc[i, ]))

          text_pattern_exp <- tryCatch(
            parse(text = text_pattern_exp),
            error = \(e) {
              list(as.character(text_pattern_exp))
            })

          if(is.character(object$fc$text)) {
            object$fc$text <- list(object$fc$text)
          }

          object$fc[i, ] <- object$fc[i, ] |>
            dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_all[[1]], y = text_pattern_exp[[1]]))))

        } else {

          text_pattern_exp <- gsub("\\{label\\}", "", text_pattern)

          object$fc[i, ] <- object$fc[i, ] |>
            dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_all[[1]], y = stringr::str_glue(text_pattern_exp)))))

        }

      } else if(is.character(label_all) & is.character(text_pattern)) {

        object$fc[i, ] <- object$fc[i, ] |>
          dplyr::mutate(text = as.character(stringr::str_glue(text_pattern)))

      } else {

        cli::cli_abort("The {.arg text_pattern} must be either a character or expression.")

      }

    }

  }

  #Initial box
  if(!is.null(text_pattern_init)) {

    id_init <- which(object$fc$type == "init")

    object$fc$text_pattern[id_init] <- if(is.expression(text_pattern_init)) list(text_pattern_init) else text_pattern_init

    for(i in id_init) {

      label_init <- object$fc$label[[i]]

      if(is.expression(label_init) | is.expression(text_pattern_init)) {

        if(is.expression(text_pattern_init)) {

          text_pattern_exp <- gsub("\\{label\\}", "", as.character(text_pattern_init)) |>
            stringr::str_glue(.envir = rlang::as_environment(object$fc[i, ]))

          text_pattern_exp <- tryCatch(
            parse(text = text_pattern_exp),
            error = \(e) {
              list(as.character(text_pattern_exp))
            })

          if(is.character(object$fc$text)) {
            object$fc$text <- list(object$fc$text)
          }

          object$fc[i, ] <- object$fc[i, ] |>
            dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_init[[1]], y = text_pattern_exp[[1]]))))

        } else {

          text_pattern_exp <- gsub("\\{label\\}", "", text_pattern_init)

          object$fc[i, ] <- object$fc[i, ] |>
            dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_init[[1]], y = stringr::str_glue(text_pattern_exp)))))

        }

      } else if(is.character(label_init) & is.character(text_pattern_init)) {

        object$fc[i, ] <- object$fc[i, ] |>
          dplyr::mutate(text = as.character(stringr::str_glue(text_pattern_init)))

      } else {

        cli::cli_abort("The {.arg text_pattern_init} must be either a character or expression.")

      }

    }

  }

  #Exclude:
  if(!is.null(text_pattern_exc)) {

    id_exc <- which(object$fc$type == "exclude")

    object$fc$text_pattern[id_exc] <- if(is.expression(text_pattern_exc)) list(text_pattern_exc) else text_pattern_exc

    for(i in id_exc) {

      label_exc <- object$fc$label[[i]]

      if(is.expression(label_exc) | is.expression(text_pattern_exc)) {

        if(is.expression(text_pattern_exc)) {

          text_pattern_exp <- gsub("\\{label\\}", "", as.character(text_pattern_exc)) |>
            stringr::str_glue(.envir = rlang::as_environment(object$fc[i, ]))

          text_pattern_exp <- tryCatch(
            parse(text = text_pattern_exp),
            error = \(e) {
              list(as.character(text_pattern_exp))
            })

          if(is.character(object$fc$text)) {
            object$fc$text <- list(object$fc$text)
          }

          object$fc[i, ] <- object$fc[i, ] |>
            dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_exc[[1]], y = text_pattern_exp[[1]]))))

        } else {

          text_pattern_exp <- gsub("\\{label\\}", "", text_pattern_exc)

          object$fc[i, ] <- object$fc[i, ] |>
            dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_exc[[1]], y = stringr::str_glue(text_pattern_exp)))))

        }

      } else if(is.character(label_exc) & is.character(text_pattern_exc)) {

        object$fc[i, ] <- object$fc[i, ] |>
          dplyr::mutate(text = as.character(stringr::str_glue(text_pattern_exc)))

      } else {

        cli::cli_abort("The {.arg text_pattern_exc} must be either a character or expression.")

      }

    }

  }

  #Get the arguments different from NULL in a parameterizated way
  arg <- rlang::fn_fmls_names()

  #Find what we have to change
  nmodify <- tibble::tibble(
    arg = arg[arg != "object"]
  ) |>
      dplyr::mutate(
        is_null = purrr::map_lgl(.data$arg, ~is.null(get(.)))
      ) |>
      dplyr::filter(!.data$is_null & !grepl("^text_pattern", .data$arg)) |>
      dplyr::mutate(
        value = purrr::map(.data$arg, ~get(.))
      )

  nmodify_exc <- nmodify |>
    dplyr::filter(grepl("_exc$", .data$arg)) |>
    dplyr::mutate(
      arg = gsub("_exc$", "", .data$arg)
    )

  nmodify_title <- nmodify |>
    dplyr::filter(grepl("_title$", .data$arg)) |>
    dplyr::mutate(
      arg = gsub("_title$", "", .data$arg)
    )

  nmodify_rest <- nmodify |>
    dplyr::filter(!grepl("_title$", .data$arg) & !grepl("_exc$", .data$arg))

  if(nrow(nmodify) > 0) {

    #Change it
    if(nrow(nmodify_rest) > 0) {
      object$fc <- object$fc |>
        dplyr::mutate(dplyr::across(nmodify_rest$arg, ~dplyr::case_when(! .data$type %in% c("exclude", "title_split") ~ nmodify_rest$value[[which(nmodify_rest$arg == dplyr::cur_column())]], .default = .)))
    }

    if(nrow(nmodify_exc) > 0) {

      if(sum(object$fc$type == "exclude") == 0) {

        cli::cli_warn("The arguments for the excluded box parameters were not applied because no excluded boxes were found in the flowchart. Therefore, no change was applied for these boxes.")

      }

      #Change the excluded boxes
      object$fc <- object$fc |>
        dplyr::mutate(dplyr::across(nmodify_exc$arg, ~dplyr::case_when(.data$type == "exclude" ~ nmodify_exc$value[[which(nmodify_exc$arg == dplyr::cur_column())]], .default = .)))

    }

    if(nrow(nmodify_title) > 0) {

      if(sum(object$fc$type == "title_split") == 0) {

        cli::cli_warn("The arguments for the title box parameters were not applied because no title boxes were found in the flowchart. Therefore, no change was applied for these boxes.")

      }

      #Change the title boxes
      object$fc <- object$fc |>
        dplyr::mutate(dplyr::across(nmodify_title$arg, ~dplyr::case_when(.data$type == "title_split" ~ nmodify_title$value[[which(nmodify_title$arg == dplyr::cur_column())]], .default = .)))

    }

  } else {

    if(is.null(text_pattern) & is.null(text_pattern_init) & is.null(text_pattern_exc)) cli::cli_warn("No parameter will be modified as all arguments are set to {.val NULL}.")

  }

  object


}
