#' @title fc_filter
#' @description This function allows to filter the flowchart in function of a expression that returns a logic value that are defined in terms of the variables in the database. It will generate one box per group showing the number of rows of the group that matches the condition, and will retain only those rows in the data base.
#'
#' @param object fc object that we want to filter.
#' @param filter Expression that returns a logical value and are defined in terms of the variables in the data frame. The data base will be filtered by this expression, and it will create a box showing the number of rows satisfying this condition.
#' @param label Character that will be the title of the box. By default it will be the evaluated condition.
#' @param text_pattern Structure that will have the text in each of the boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)".
#' @param show_exc Logical value. If TRUE a box showing the number of excluded rows will be added to the flow chart.
#' @param direction_exc One of "left" or "right" indicating if the exclusion box goes into the left direction or in the right direction. By default is "right".
#' @param label_exc Character that will be the title of the added box showing the excluded patients. By default it will show "Excluded".
#' @param text_pattern_exc Structure that will have the text in each of the excluded boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)".
#' @param sel_group Specify if the filtering has to be done only by one of the previous groups. By default is NULL.
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default.
#' @param text_fs Font size of the text. It is 8 by default.
#' @param bg_fill Box background color. It is white by default.
#' @param border_color Box border color. It is black by default.
#' @param just_exc Justification for the text of the exclude box: left, center or right. Default is center.
#' @param text_color_exc Color of the text of the exclude box. It is black by default.
#' @param text_fs_exc Font size of the text of the exclude box. It is 6 by default.
#' @param bg_fill_exc Exclude box background color. It is white by default.
#' @param border_color_exc Box background color of the exclude box. It is black by default.
#' @return List with the filtered dataset and the flowchart with the resulting filtered box.
#'
#' @examples
#' clinic_patient |>
#'   as_fc(label = "Patients included") |>
#'   fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) |>
#'   fc_draw()
#'
#' @export
#' @importFrom rlang .data

fc_filter <- function(object, filter, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", show_exc = FALSE, direction_exc = "right", label_exc = "Excluded", text_pattern_exc = "{label}\n {n} ({perc}%)", sel_group = NULL, just = "center", text_color = "black", text_fs = 8, bg_fill = "white", border_color = "black", just_exc = "center", text_color_exc = "black", text_fs_exc = 6, bg_fill_exc = "white", border_color_exc = "black") {

  is_class(object, "fc")

  filter_txt <- paste(deparse(substitute(filter)), collapse = "")
  filter_txt <- gsub("  ", "", filter_txt)
  if(is.null(label)) {
    label <- filter_txt
  }

  #Fiquem les posicions horitzontals
  if(!is.null(attr(object$data, "groups"))) {
    nhor <- nrow(attr(object$data, "groups"))
  } else {
    nhor <- 1
  }

  xval <-  seq(0, 1, by = 1/(nhor + 1))
  xval <- xval[!xval %in% c(0, 1)]

  new_fc <- object$data |>
    dplyr::summarise(
      n = sum({{filter}}, na.rm = TRUE),
      N = dplyr::n()
    ) |>
    dplyr::mutate(
      x = xval,
      y = NA,
      perc = round(.data$n*100/.data$N, 2),
      text = as.character(stringr::str_glue(text_pattern)),
      type = "filter",
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      bg_fill = bg_fill,
      border_color = border_color
    )

  group0 <- names(attr(object$data, "groups"))
  group0 <- group0[group0 != ".rows"]

  if(length(group0) > 0) {
    new_fc <- new_fc |>
      tidyr::unite("group", tidyselect::all_of(group0), sep = ", ") |>
      dplyr::ungroup()
  } else {
    new_fc <- new_fc |>
      dplyr::mutate(group = NA)
  }


  if(is.null(sel_group)) {

    new_fc <- new_fc |>
      dplyr::select("x", "y", "n", "N", "perc", "text", "type", "group", "just", "text_color", "text_fs", "bg_fill", "border_color")

  } else {

    new_fc <- new_fc |>
      dplyr::filter(.data$group %in% sel_group) |>
      dplyr::select("x", "y", "n", "N", "perc", "text", "type", "group", "just", "text_color", "text_fs", "bg_fill", "border_color")

  }


  #remove the id previous to adding the next one
  if(!is.null(object$fc)) {
    object$fc <- object$fc |>
      dplyr::select(-"id")
  }


  object$fc <- rbind(
    object$fc |>
      dplyr::mutate(old = TRUE),
    new_fc |>
      dplyr::mutate(old = FALSE)
  ) |>
    dplyr::mutate(
      y = update_y(.data$y, .data$type, .data$x)
    )

  #If we have to add the filter box
  if(show_exc) {

    add_x <- dplyr::case_when(
      direction_exc == "right" ~ 0.15,
      direction_exc == "left" ~ -0.15,
      TRUE ~ NA
    )

    #Calculate the middle distance between the box and the parent
    new_fc <- object$fc |>
      dplyr::filter(!.data$old)

    #The label in the text_pattern references to label_exc and not to label
    label <- label_exc

    new_fc2 <- new_fc |>
      dplyr::mutate(
        parent = purrr::map(.data$x, ~object$fc |>
                              dplyr::filter(.data$x == .x, .data$old) |>
                              dplyr::last() |>
                              dplyr::select("y", "n")
        ),
        x = .data$x + add_x,
        y = purrr::map2_dbl(.data$parent, .data$y, ~(.y + .x$y)/2),
        n = purrr::map2_int(.data$parent, .data$n, ~.x$n - .y),
        N = purrr::map_int(.data$parent, ~.x$n),
        perc = purrr::map2_dbl(.data$n, .data$N, ~round(.x*100/.y, 2)),
        text = as.character(stringr::str_glue(text_pattern_exc)),
        type = "exclude",
        group = NA,
        just = just_exc,
        text_color = text_color_exc,
        text_fs = text_fs_exc,
        bg_fill = bg_fill_exc,
        border_color = border_color_exc
      ) |>
      dplyr::select(-"parent")

    new_fc3 <- NULL
    for(i in 1:nrow(new_fc2)) {
      new_fc3 <- rbind(new_fc3, new_fc[i,], new_fc2[i,])
    }

    object$fc <- rbind(
      object$fc |> dplyr::filter(.data$old),
      new_fc3 |>
        tibble::as_tibble()
    )

  }

  object$fc <- object$fc |>
    dplyr::select(-"old") |>
    dplyr::mutate(
      id = dplyr::row_number()
    ) |>
    dplyr::relocate("id")

  #Quan fem un filter la bbdd ha de quedar filtrada
  object$data <- object$data |>
    dplyr::filter({{filter}})

  object


}
