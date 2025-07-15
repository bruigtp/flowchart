#' @title fc_filter
#' @description This function allows to filter the flowchart in function of a expression that returns a logic value that are defined in terms of the variables in the database. It will generate one box per group showing the number of rows of the group that matches the condition, and will retain only those rows in the data base.
#'
#' @param object fc object that we want to filter.
#' @param filter Expression that returns a logical value and are defined in terms of the variables in the data frame. The data base will be filtered by this expression, and it will create a box showing the number of rows satisfying this condition.
#' @param N Number of rows after the filter in case `filter` is NULL.
#' @param label Character or expression that will be the title of the box. By default it will be the evaluated condition.
#' @param text_pattern Character or expression defining the structure that will have the text in each of the boxes. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)". If text_pattern or label is an expression, the label is always placed at the beginning of the pattern, followed by a line break where the structure specified by text_pattern is placed.
#' @param perc_total logical. Should percentages be calculated using the total number of rows at the beginning of the flowchart? Default is FALSE, meaning that they will be calculated using the number at the parent leaf.
#' @param show_exc Logical value. If TRUE a box showing the number of excluded rows will be added to the flow chart.
#' @param direction_exc One of "left" or "right" indicating if the exclusion box goes into the left direction or in the right direction. By default is "right".
#' @param label_exc Character or expression that will be the title of the added box showing the excluded patients. By default it will show "Excluded".
#' @param text_pattern_exc Character or expression defining the structure that will have the text in the exclude box. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n \{n\} (\{perc\}\%)". If text_pattern or label is an expression, the label is always placed at the beginning of the pattern, followed by a line break where the structure specified by text_pattern_exc is placed.
#' @param sel_group Select the group in which to perform the filter. The default is NULL. Can only be used if the flowchart has previously been split. If the flowchart has more than one group, it can either be given the full name as it is stored in the `$fc` component (separated by '\\'), or it can be given as a vector with the names of each group to be selected.
#' @param round_digits Number of digits to round percentages. It is 2 by default.
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param text_fs Font size of the text. It is 8 by default. See the `fontsize` parameter for \code{\link{gpar}}.
#' @param text_fface Font face of the text. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily Changes the font family of the text. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding Changes the text padding inside the box. Default is 1. This number has to be greater than 0.
#' @param bg_fill Box background color. It is white by default. See the `fill` parameter for \code{\link{gpar}}.
#' @param border_color Box border color. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param width Width of the box. If NA, it automatically adjusts to the content (default). Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param height Height of the box. If NA, it automatically adjusts to the content (default). Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param just_exc Justification for the text of the exclude box: left, center or right. Default is center.
#' @param text_color_exc Color of the text of the exclude box. It is black by default. See `text_color`.
#' @param text_fs_exc Font size of the text of the exclude box. It is 6 by default. See `text_fs`.
#' @param text_fface_exc Font face of the text of the exclude box. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}. See `text_fface`.
#' @param text_ffamily_exc Changes the font family of the text of the exclude box. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}. See `text_ffamily`.
#' @param text_padding_exc Changes the text padding inside the exclude box. Default is 1. This number has to be greater than 0.
#' @param bg_fill_exc Exclude box background color. It is white by default. See `bg_fill`.
#' @param border_color_exc Box background color of the exclude box. It is black by default. See `border_color`.
#' @param offset_exc Amount of space to add to the distance between the box and the excluded box (in the x coordinate). If positive, this distance will be larger. If negative, it will be smaller. This number has to be at least between 0 and 1 (plot limits) and the resulting x coordinate cannot exceed these plot limits. The default is NULL (no offset).
#' @param width_exc Width of the exclude box. If NA, it automatically adjusts to the content (default). Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @param height_exc Height of the box. If NA, it automatically adjusts to the content (default). Must be an object of class \code{\link{unit}} or a number between 0 and 1.
#' @return List with the filtered dataset and the flowchart parameters with the resulting filtered box.
#'
#' @examples
#' safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'   fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#'   fc_draw()
#'
#' @export

fc_filter <- function(object, filter = NULL, N = NULL, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", perc_total = FALSE, show_exc = FALSE, direction_exc = "right", label_exc = "Excluded", text_pattern_exc = "{label}\n {n} ({perc}%)", sel_group = NULL, round_digits = 2, just = "center", text_color = "black", text_fs = 8, text_fface = 1, text_ffamily = NA, text_padding = 1, bg_fill = "white", border_color = "black", width = NA, height = NA, just_exc = "center", text_color_exc = "black", text_fs_exc = 6, text_fface_exc = 1, text_ffamily_exc = NA, text_padding_exc = 1, bg_fill_exc = "white", border_color_exc = "black", offset_exc = NULL, width_exc = NA, height_exc = NA) {

  is_class(object, "fc")
  UseMethod("fc_filter")

}

#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=

fc_filter.fc <- function(object, filter = NULL, N = NULL, label = NULL, text_pattern = "{label}\n {n} ({perc}%)", perc_total = FALSE, show_exc = FALSE, direction_exc = "right", label_exc = "Excluded", text_pattern_exc = "{label}\n {n} ({perc}%)", sel_group = NULL, round_digits = 2, just = "center", text_color = "black", text_fs = 8, text_fface = 1, text_ffamily = NA, text_padding = 1, bg_fill = "white", border_color = "black", width = NA, height = NA, just_exc = "center", text_color_exc = "black", text_fs_exc = 6, text_fface_exc = 1, text_ffamily_exc = NA, text_padding_exc = 1, bg_fill_exc = "white", border_color_exc = "black", offset_exc = NULL, width_exc = NA, height_exc = NA) {

  filter <- paste(deparse(substitute(filter)), collapse = "")
  filter <- gsub("  ", "", filter)

  if(filter == "NULL" & is.null(N)) {
    cli::cli_abort("Either {.arg filter} or {.arg N} arguments must be specified.")
  }else if(filter != "NULL" & !is.null(N)) {
    cli::cli_abort("The {.arg filter} and {.arg N} arguments cannot be specified simultaneously.")
  }

  if(!is.null(sel_group)) {

    if(!all(is.na(object$fc$group))) {

      if(length(sel_group) > 1) {
        sel_group <- paste(sel_group, collapse = " // ")
      }

      if(!any(sel_group %in% object$fc$group)) {
        cli::cli_abort(
          c(
            "The specified {.arg sel_group} does not match any group of the flowchart.",
            "i" = "Found groups in the flowchart: {object$fc$group[!is.na(object$fc$group)]}"
          )
        )
      }

    } else {

      cli::cli_abort("Cannot supply {.arg sel_group} because no groups exist in the flowchart yet, as no previous split has been performed.")

    }

  }

  if(filter == "NULL") {
    num <- length(grep("filter\\d+", names(object$data)))
    filter <- stringr::str_glue("filter{num + 1}")

    if(is.null(attr(object$data, "groups"))) {
      if(length(N) > 1) {
        cli::cli_abort("The length of {.arg N} has to be 1.")
      }
    } else {
      if(length(N) != nrow(attr(object$data, "groups"))) {
        if(is.null(sel_group)) {
          cli::cli_abort("The length of {.arg N} has to match the number of groups in the dataset: {nrow(attr(object$data, 'groups'))}")
        } else {
          if(length(N) != length(sel_group)) {
            cli::cli_abort("The length of {.arg N} has to match the number of selected groups in {.arg sel_group}")
          }
        }
      }
    }


    object$data$row_number_delete <- 1:nrow(object$data)
    #select rows to be true the filter
    tbl_groups <- attr(object$data, "groups")

    if(!is.null(tbl_groups)) {

      #Exclude zeros (when sel_group is used in a split previously)
      tbl_groups <- tbl_groups |>
        dplyr::filter(lengths(.data$.rows) != 0)

      if(!is.null(sel_group)) {

        tbl_groups <- tbl_groups |>
          #Deal with missings from sel_group variables
          dplyr::mutate(dplyr::across(!".rows", ~dplyr::case_when(is.na(.) & !grepl("sel_group$", dplyr::cur_column()) ~ "NA", .default = .))) |>
          tidyr::unite("groups", -".rows", sep = " // ", na.rm = TRUE)

        if(sel_group %in% tbl_groups$groups) {

          tbl_groups <- tbl_groups |>
            dplyr::filter(.data$groups == sel_group)

        } else {

          cli::cli_abort("The specified {.arg sel_group} is not a grouping variable of the data. It has to be one of: {tbl_groups$groups}")

        }

      }

      filt_rows <- unlist(purrr::map(1:nrow(tbl_groups), function (x) {
        if(N[x] > length(tbl_groups$.rows[[x]])) {
          cli::cli_abort("The number of rows after the filter specified in {.arg N} can't be greater than the original number of rows.")
        } else {
          tbl_groups$.rows[[x]][1:N[x]]
        }
      }))

    } else {

      nrows <- 1:nrow(object$data)

      if(N > length(nrows)) {
        cli::cli_abort("The number of rows after the filter specified in {.arg N} cannot exceed the original number of rows.")
      }

      filt_rows <- nrows[1:N]

    }



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
    ) |>
    #Exclude zeros (when sel_group is used in a split previously)
    dplyr::filter(N != 0)

  if(is.null(group0)) {

    new_fc$group <- NA

    new_fc <- new_fc |>
      dplyr::left_join(object$fc |> dplyr::filter(.data$type != "exclude") |> dplyr::select("x", "group"), by = "group") |>
      dplyr::group_by(.data$group) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()

  } else {

    new_fc <- new_fc |>
      dplyr::ungroup() |>
      #Deal with missings from sel_group variables
      dplyr::mutate(dplyr::across(dplyr::all_of(group0), ~dplyr::case_when(is.na(.) & !grepl("sel_group$", dplyr::cur_column()) ~ "NA", .default = .))) |>
      tidyr::unite("group", dplyr::all_of(group0), sep = " // ", na.rm = TRUE) |>
      dplyr::left_join(object$fc |> dplyr::filter(.data$type != "exclude") |> dplyr::select("x", "group"), by = "group") |>
      dplyr::mutate(group = factor(.data$group, levels = unique(.data$group))) |>
      dplyr::group_by(.data$group) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::mutate(group = as.character(.data$group))

  }


  if(perc_total) {
    N_total <- unique(
      object$fc |>
        dplyr::filter(.data$y == max(.data$y)) |>
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
    cli::cli_abort("Text padding cannot be equal to zero.")
  }

  new_fc <- new_fc |>
    dplyr::mutate(
      y = NA,
      perc = round(.data$n*100/.data$N_total, round_digits),
      type = "filter",
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      text_fface = text_fface,
      text_ffamily = text_ffamily,
      text_padding = text_padding,
      bg_fill = bg_fill,
      border_color = border_color,
      width = width,
      height = height
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-N_total)

  if(is.expression(label) | is.expression(text_pattern)) {

    if(is.expression(text_pattern)) {

      text_pattern_exp <- gsub("\\{label\\}", "", as.character(text_pattern)) |>
        stringr::str_glue(.envir = rlang::as_environment(new_fc))

      text_pattern_exp <- tryCatch(
        parse(text = text_pattern_exp),
        error = \(e) {
          list(as.character(text_pattern_exp))
        })

      new_fc <- new_fc |>
        dplyr::mutate(text = list(substitute(atop(x, y), list(x = label[[1]], y = text_pattern_exp[[1]]))))

    } else {

      text_pattern_exp <- gsub("\\{label\\}", "", text_pattern)

      new_fc <- new_fc |>
        dplyr::mutate(text = list(substitute(atop(x, y), list(x = label[[1]], y = stringr::str_glue(text_pattern_exp)))))

    }

  } else if(is.character(label) & is.character(text_pattern)) {

    new_fc <- new_fc |>
      dplyr::mutate(text = as.character(stringr::str_glue(text_pattern)))

  } else {

    cli::cli_abort("The {.arg label} and {.arg text_pattern} must be either characters or expressions.")

  }

  new_fc <- if(is.expression(label)) new_fc |> dplyr::mutate(label = list(label)) else new_fc |> dplyr::mutate(label = label)

  new_fc <- if(is.expression(text_pattern)) new_fc |> dplyr::mutate(text_pattern = list(text_pattern)) else new_fc |> dplyr::mutate(text_pattern = text_pattern)

  new_fc <- new_fc |>
    dplyr::relocate(c("label", "text_pattern", "text"), .after = "perc")

  if(is.null(sel_group)) {

    new_fc <- new_fc |>
      dplyr::select("x", "y", "n", "N", "perc", "label", "text_pattern", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "text_padding", "bg_fill", "border_color", "width", "height")

  } else {

    new_fc <- new_fc |>
      dplyr::filter(.data$group %in% sel_group) |>
      dplyr::select("x", "y", "n", "N", "perc", "label", "text_pattern", "text", "type", "group", "just", "text_color", "text_fs", "text_fface", "text_ffamily", "text_padding", "bg_fill", "border_color", "width", "height")

  }


  #remove the id previous to adding the next one
  if(!is.null(object$fc)) {

    object$fc <- object$fc |>
      dplyr::select(-"id") |>
      dplyr::mutate(old = TRUE)

    #If we select a group, it only updates the box in the group so the other group remains being the end of the flowchart
    if(is.null(sel_group)) {

      object$fc <- object$fc |>
        dplyr::mutate(end = FALSE)

    } else {

      object$fc <- object$fc |>
        dplyr::mutate(
          end = dplyr::case_when(
            .data$group %in% sel_group ~ FALSE,
            .default = .data$end
          )
        )

    }


  }

  object$fc <- rbind(
    object$fc,
    new_fc |>
      dplyr::mutate(end = TRUE,
                    old = FALSE)
  ) |>
    dplyr::mutate(
      y = update_y(.data$y, .data$type, .data$x, .data$group)
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
      dplyr::select(-"label") |>
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

    if(!is.null(offset_exc)) {
      if(!all(new_fc2$x >= 0 & new_fc2$x <= 1)) {
        cli::cli_abort(
          c(
            "The x-coordinate cannot exceed the plot limits 0 and 1.",
            "i" = "The argument {.arg offset_exc} has to be set to a smaller number."
          )
        )
      }
    }

    if(perc_total) {
      N_total <- unique(
        object$fc |>
          dplyr::filter(.data$y == max(.data$y)) |>
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
        type = "exclude",
        just = just_exc,
        text_color = text_color_exc,
        text_fs = text_fs_exc,
        text_fface = text_fface_exc,
        text_ffamily = text_ffamily_exc,
        text_padding = text_padding_exc,
        bg_fill = bg_fill_exc,
        border_color = border_color_exc,
        width = width_exc,
        height = height_exc
      ) |>
      dplyr::select(-"parent", -"N_total")

    if(is.expression(label_exc) | is.expression(text_pattern_exc)) {

      if(is.expression(text_pattern_exc)) {

        text_pattern_exc_exp <- gsub("\\{label\\}", "", as.character(text_pattern_exc)) |>
          stringr::str_glue(.envir = rlang::as_environment(new_fc2))

        text_pattern_exc_exp <- tryCatch(
          parse(text = text_pattern_exc_exp),
          error = \(e) {
            list(as.character(text_pattern_exc_exp))
          })

        new_fc2 <- new_fc2 |>
          dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_exc[[1]], y = text_pattern_exc_exp[[1]]))))

      } else {

        text_pattern_exc_exp <- gsub("\\{label\\}", "", text_pattern_exc)

        new_fc2 <- new_fc2 |>
          dplyr::mutate(text = list(substitute(atop(x, y), list(x = label_exc[[1]], y = stringr::str_glue(text_pattern_exc_exp)))))

      }

    } else if(is.character(label_exc) & is.character(text_pattern_exc)) {

      new_fc2 <- new_fc2 |>
        dplyr::mutate(text = as.character(stringr::str_glue(text_pattern_exc)))

    } else {

      cli::cli_abort("The {.arg label_exc} and {.arg text_pattern_exc} must be either characters or expressions.")

    }

    new_fc2 <- if(is.expression(label_exc)) new_fc2 |> dplyr::mutate(label = list(label_exc)) else new_fc2 |> dplyr::mutate(label = label_exc)

    new_fc2 <- if(is.expression(text_pattern_exc)) new_fc2 |> dplyr::mutate(text_pattern = list(text_pattern_exc)) else new_fc2 |> dplyr::mutate(text_pattern = text_pattern_exc)

    new_fc2 <- new_fc2 |>
      dplyr::relocate(c("label", "text_pattern", "text"), .after = "perc")

    new_fc3 <- NULL
    for(i in 1:nrow(new_fc2)) {
      new_fc3 <- rbind(new_fc3, new_fc[i,], new_fc2[i,])
    }

    object$fc <- rbind(
      object$fc |> dplyr::filter(.data$old),
      new_fc3 |>
        tibble::as_tibble() |>
        dplyr::mutate(
          end = FALSE,
          .before = "old"
        )
    )

  }

  object$fc <- object$fc |>
    dplyr::select(-"old") |>
    dplyr::mutate(
      id = dplyr::row_number()
    ) |>
    dplyr::relocate("id")


  #Filter the database
  if(is.null(sel_group)) {

    object$data <- object$data |>
      dplyr::filter(rlang::eval_tidy(rlang::parse_expr(filter_to_parse)))

  } else {

    groups <- names(attr(object$data, "groups"))
    groups <- groups[groups != ".rows"]

    filter_to_parse <- stringr::str_glue("{filter_to_parse} | temp_var_PauSatorra_12345 != '{sel_group}'")
    object$data <- object$data |>
      dplyr::ungroup() |>
      #Deal with missings from sel_group variables
      dplyr::mutate(dplyr::across(dplyr::all_of(groups), ~dplyr::case_when(is.na(.) & !grepl("sel_group$", dplyr::cur_column()) ~ "NA", .default = .))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      tidyr::unite("temp_var_PauSatorra_12345", dplyr::all_of(groups), sep = " // ", na.rm = TRUE, remove = FALSE) |>
      dplyr::filter(rlang::eval_tidy(rlang::parse_expr(filter_to_parse))) |>
      dplyr::select(-"temp_var_PauSatorra_12345")
  }

  object

}
