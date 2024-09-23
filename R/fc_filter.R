#' @title fc_filter
#' @description This function allows to filter the flowchart in function of a expression that returns a logic value that are defined in terms of the variables in the database. It will generate one box per group showing the number of rows of the group that matches the condition, and will retain only those rows in the data base.
#'
#' @param object fc object that we want to filter.
#' @param filter Expression that returns a logical value and are defined in terms of the variables in the data frame. The data base will be filtered by this expression, and it will create a box showing the number of rows satisfying this condition.
#' @param N Number of rows after the filter in case `filter` is NULL.
#' @param label Character that will be the title of the box. By default it will be the evaluated condition.
#' @param text_pattern Structure that will have the text in each of the boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)".
#' @param perc_total logical. Should percentages be calculated using the total number at the beginning of the flowchart? Default is FALSE, meaning that they will be calculated using the number at the parent leaf.
#' @param show_exc Logical value. If TRUE a box showing the number of excluded rows will be added to the flow chart.
#' @param direction_exc One of "left" or "right" indicating if the exclusion box goes into the left direction or in the right direction. By default is "right".
#' @param label_exc Character that will be the title of the added box showing the excluded patients. By default it will show "Excluded".
#' @param text_pattern_exc Structure that will have the text in each of the excluded boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)".
#' @param sel_group Specify if the filtering has to be done only by one of the previous groups. By default is NULL.
#' @param round_digits Number of digits to round percentages. It is 2 by default.
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param text_fs Font size of the text. It is 8 by default. See the `fontsize` parameter for \code{\link{gpar}}.
#' @param text_fface Font face of the text. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily Changes the font family of the text. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding Changes the text padding inside the box. Default is 1. This number has to be greater than 0.
#' @param bg_fill Box background color. It is white by default. See the `fill` parameter for \code{\link{gpar}}.
#' @param border_color Box border color. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param just_exc Justification for the text of the exclude box: left, center or right. Default is center.
#' @param text_color_exc Color of the text of the exclude box. It is black by default. See `text_color`.
#' @param text_fs_exc Font size of the text of the exclude box. It is 6 by default. See `text_fs`.
#' @param text_fface_exc Font face of the text of the exclude box. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}. See `text_fface`.
#' @param text_ffamily_exc Changes the font family of the text of the exclude box. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}. See `text_ffamily`.
#' @param text_padding_exc Changes the text padding inside the exclude box. Default is 1. This number has to be greater than 0.
#' @param bg_fill_exc Exclude box background color. It is white by default. See `bg_fill`.
#' @param border_color_exc Box background color of the exclude box. It is black by default. See `border_color`.
#' @param offset_exc Amount of space to add to the distance between the box and the excluded box (in the x coordinate). If positive, this distance will be larger. If negative, it will be smaller. The default is NULL (no offset).
#' @return List with the filtered dataset and the flowchart parameters with the resulting filtered box.
#'
#' @examples
#' safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'   fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'   fc_draw()
#'
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=

fc_filter <- function(object, filter = NULL, N = NULL, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", perc_total = FALSE, show_exc = FALSE, direction_exc = "right", label_exc = "Excluded", text_pattern_exc = "{label}\n {n} ({perc}%)", sel_group = NULL, round_digits = 2, just = "center", text_color = "black", text_fs = 8, text_fface = 1, text_ffamily = NA, text_padding = 1, bg_fill = "white", border_color = "black", just_exc = "center", text_color_exc = "black", text_fs_exc = 6, text_fface_exc = 1, text_ffamily_exc = NA, text_padding_exc = 1, bg_fill_exc = "white", border_color_exc = "black", offset_exc = NULL) {

  is_class(object, "fc")

  filter <- paste(deparse(substitute(filter)), collapse = "")
  filter <- gsub("  ", "", filter)

  if(filter == "NULL" & is.null(N)) {
    stop("Either `filter` or `N` arguments have to be specified.")
  }else if(filter != "NULL" & !is.null(N)) {
    stop("`filter` and `N` arguments cannot be specified simultaneously.")
  }

  if(filter == "NULL") {
    num <- length(grep("filter\\d+", names(object$data)))
    filter <- stringr::str_glue("filter{num + 1}")

    if(is.null(attr(object$data, "groups"))) {
      if(length(N) > 1) {
        stop("The length of `N` has to be 1.")
      }
    } else {
      if(length(N) != nrow(attr(object$data, "groups"))) {
        stop(stringr::str_glue("The length of `N` has to match the number of groups in the dataset: {nrow(attr(object$data, 'groups'))}"))
      }
    }


    object$data$row_number_delete <- 1:nrow(object$data)
    #select rows to be true the filter
    nrows <- dplyr::group_rows(object$data)
    filt_rows <- unlist(purrr::map(seq_along(nrows), function (x) {
      if(N[x] > length(nrows[[x]])) {
        stop("The number of rows after the filter specified in N can't be greater than the original number of rows")
      } else {
        nrows[[x]][1:N[x]]
      }
      }))

    object$data <- object$data |>
      dplyr::mutate(
        "{filter}" := dplyr::case_when(
          .data$row_number_delete %in% filt_rows ~ TRUE,
          TRUE ~ FALSE
        )
      ) |>
      dplyr::select(-"row_number_delete")

  }

  if(is.null(label)) {
    label <- filter
  }

  group0 <- names(attr(object$data, "groups"))
  group0 <- group0[group0 != ".rows"]

  filter_to_parse <- filter
  new_fc <- object$data |>
    dplyr::summarise(
      n = sum(rlang::eval_tidy(rlang::parse_expr(filter_to_parse)), na.rm = TRUE),
      N = dplyr::n()
    )

  if(is.null(group0)) {
    new_fc$group <- NA
  } else {
    new_fc <- new_fc |>
      tidyr::unite("group", c(tidyselect::all_of(group0)), sep = ", ", na.rm = TRUE)
  }

  new_fc <- new_fc |>
    dplyr::left_join(object$fc |> dplyr::filter(.data$type != "exclude") |> dplyr::select("x", "group"), by = "group") |>
    dplyr::group_by(.data$group) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()

  if(perc_total) {
    N_total <- unique(
      object$fc |>
        dplyr::filter(is.na(.data$group)) |>
        dplyr::pull("N")
    )
    new_fc <- new_fc |>
      dplyr::mutate(
        N_total = N_total
      )
  } else {
    new_fc <- new_fc |>
      dplyr::mutate(
        N_total = .data$N
      )
  }

  if(text_padding == 0 | text_padding_exc == 0) {
    stop("Text padding cannot be equal to zero")
  }

  new_fc <- new_fc |>
    dplyr::mutate(
      y = NA,
      perc = round(.data$n*100/.data$N_total, round_digits),
      text = as.character(stringr::str_glue(text_pattern)),
      type = "filter",
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      text_fface = text_fface,
      text_ffamily = text_ffamily,
      text_padding = text_padding,
      bg_fill = bg_fill,
      border_color = border_color
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-N_total)


  if(is.null(sel_group)) {

    new_fc <- new_fc |>
      dplyr::select("x", "y", "n", "N", "perc", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "text_padding", "bg_fill", "border_color")

  } else {

    new_fc <- new_fc |>
      dplyr::filter(.data$group %in% sel_group) |>
      dplyr::select("x", "y", "n", "N", "perc", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "text_padding", "bg_fill", "border_color")

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
    #For the box to not escape the margins:
    x_margin <- new_fc |>
      dplyr::mutate(
        limit = dplyr::case_when(
          .data$x + add_x <= 0.05 ~ -.data$x/2,
          .data$x + add_x >= 0.95 ~ (1 - .data$x)/2,
          TRUE ~ NA
        )
      ) |>
      dplyr::filter(!is.na(.data$limit))

    if(nrow(x_margin) > 0) {
      min_add <- min(abs(x_margin$limit))
      add_x <- dplyr::case_when(
        sign(add_x) != sign(min_add) ~ min_add*(-1),
        TRUE ~ min_add
      )
    }

    if(!is.null(offset_exc)) {
      add_x <- add_x + offset_exc
    }

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
        N = purrr::map_int(.data$parent, ~.x$n)
      )

    if(perc_total) {
      N_total <- unique(
        object$fc |>
          dplyr::filter(is.na(.data$group)) |>
          dplyr::pull("N")
      )
      new_fc2 <- new_fc2 |>
        dplyr::mutate(
          N_total = N_total
        )
    } else {
      new_fc2 <- new_fc2 |>
        dplyr::mutate(
          N_total = .data$N
        )
    }


    new_fc2 <- new_fc2 |>
      dplyr::mutate(
        perc = purrr::map2_dbl(.data$n, .data$N_total, ~round(.x*100/.y, round_digits)),
        text = as.character(stringr::str_glue(text_pattern_exc)),
        type = "exclude",
        just = just_exc,
        text_color = text_color_exc,
        text_fs = text_fs_exc,
        text_fface = text_fface_exc,
        text_ffamily = text_ffamily_exc,
        text_padding = text_padding_exc,
        bg_fill = bg_fill_exc,
        border_color = border_color_exc
      ) |>
      dplyr::select(-"parent", -"N_total")

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
    dplyr::filter(rlang::eval_tidy(rlang::parse_expr(filter_to_parse)))

  object


}
