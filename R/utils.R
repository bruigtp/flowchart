# Internal functions used in the package:

#' @title update_x
#' @description Function to update the horizontal position of the previuosly created flow charts, to merge two flowcharts used in fc_merge().
#'
#'@param x old horizontal position of the boxes
#'@param i position of the flowchart within the list of flowcharts.
#'@param n number of total flowcharts to merge
#'@keywords internal
#'
update_x <- function(x, i, n) {

  scale <- 1/n

  xval <- seq((i - 1)*scale, i*scale, by = scale/(length(unique(x)) + 1))
  xval <- xval[-c(1, length(xval))]

  as.numeric(as.character(factor(x, levels = sort(unique(x)), labels = xval)))

}

#' @title update_y
#' @description Function to update the vertical position of the previuosly created boxes, to add the new ones with fc_split() or fc_filter().
#'
#'@param y old height of the boxes
#'@param type type of box
#'@param x horizontal position of the boxes
#'@keywords internal
#'
update_y <- function(y, type, x, group) {

  tbl_y <- tibble::tibble("y" = y, "type" = type, "x" = x, "group" = group) |>
    dplyr::filter(.data$type != "exclude") |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::mutate(
      id_pre = purrr::map(dplyr::row_number(), function (rn) {
        if(rn == 1) {
          NA
        } else {
          1:(rn - 1)
        }
      })
    )

  tbl_ynew <- tbl_y |>
    dplyr::filter(!is.na(.data$group) | is.na(.data$y)) |>
    dplyr::mutate(
      group_split = purrr::map(.data$group, ~unlist(stringr::str_split(., " // "))),
      all_groups = purrr::map(.data$group_split, function (x) purrr::map_chr(rev(1:length(x)), ~paste(x[1:.], collapse = " // "))),
      id_same_group = purrr::map2(.data$id_pre, .data$all_groups, function (x, y) {
          x[purrr::map_lgl(x, ~is.na(tbl_y$group[.]) | any(y %in% tbl_y$group[.]))]
      })
    )

  for(i in 1:nrow(tbl_ynew)) {

    tbl_yadd <- tbl_y |>
      dplyr::filter(.data$id %in% c(tbl_ynew$id[i], tbl_ynew$id_same_group[[i]])) |>
      dplyr::mutate(
        nboxes = dplyr::n(),
        y = setdiff(rev(seq(0, 1, by = 1/(dplyr::n() + 1))), c(0, 1))
      ) |>
      dplyr::select("y", "id", "nboxes") |>
      dplyr::rename_all(~dplyr::case_when(. != "id" ~ stringr::str_glue("{.}{i}"), .default = .))

    tbl_y <- tbl_y |>
      dplyr::left_join(tbl_yadd, by = "id")

  }

  tbl_y <- tbl_y |>
    dplyr::group_by(y) |>
    tidyr::fill(tidyselect::starts_with("y"), tidyselect::starts_with("nboxes"), .direction = "downup") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      nmax = as.numeric(which.max(dplyr::across(tidyselect::starts_with("nboxes"), ~ .)))
    )

  id_exc <- which(type == "exclude")

  if(length(id_exc) > 0) {
    y[-id_exc] <- purrr::map_dbl(1:nrow(tbl_y), ~tbl_y[[paste0("y", tbl_y$nmax[.])]][.])
  } else {
    y <- purrr::map_dbl(1:nrow(tbl_y), ~tbl_y[[paste0("y", tbl_y$nmax[.])]][.])
  }

  #Update those of type exclude (they have to be in-between the two boxes)
  if(length(id_exc) > 0) {
    for(i in 1:length(id_exc)) {

      #Find the two boxes, one above another below the exclude box.
      x_filt <- x[id_exc[i] - 1]
      id_par <- which(x == x_filt)
      id_par <- utils::tail(id_par[id_par < (id_exc[i] - 1)], 1)
      y[id_exc[i]] <- (y[id_par] + y[id_exc[i] - 1])/2

    }
  }

  y

}

#' @title update_y_stack
#' @description Function to update the vertical position of the previuosly created flowcharts, to stack the two flowcharts with fc_stack().
#'
#'@param y old vertical position of the boxes
#'@param x old horizontal position of the boxes
#'@param type type of the boxes
#'@param i position of the flowchart within the list of flowcharts.
#'@param n number of total flowcharts to merge
#'@keywords internal
#'
update_y_stack <- function(y, x, type, i, n) {

  scale <- 1/n

  i <- (n + 1) - i

  yval <- seq((i - 1)*scale, i*scale, by = scale/(length(unique(y)) + 1))
  yval <- yval[-c(1, length(yval))]

  y <- as.numeric(as.character(factor(y, levels = sort(unique(y)), labels = yval)))

  #Update those of type exclude (they have to be in-between the two boxes)
  id_exc <- which(type == "exclude")
  if(length(id_exc) > 0) {
    for(i in 1:length(id_exc)) {

      #Find the two boxes, one above another below the exclude box.
      x_filt <- x[id_exc[i] - 1]
      id_par <- which(x == x_filt)
      id_par <- utils::tail(id_par[id_par < (id_exc[i] - 1)], 1)
      y[id_exc[i]] <- (y[id_par] + y[id_exc[i] - 1])/2

    }
  }

  y

}

#' @title update_y_stack_unite
#' @description Function to update the vertical position of the previuosly created flowcharts, to stack the two flowcharts with fc_stack(), when `unite` is TRUE.
#'
#'@param y old vertical position of the boxes
#'@param x old horizontal position of the boxes
#'@param type type of the boxes
#'@keywords internal
#'
update_y_stack_unite <- function(y, x, type) {

  yval <- y[type != "exclude"]
  nbox <- cumsum(!duplicated(yval))
  ynew <- rev(setdiff(seq(0, 1, by = 1/(max(nbox) + 1)), c(0, 1)))
  yval <- ynew[nbox]

  y[type != "exclude"] <- yval

  #Update those of type exclude (they have to be in-between the two boxes)
  id_exc <- which(type == "exclude")
  if(length(id_exc) > 0) {
    for(i in 1:length(id_exc)) {

      #Find the two boxes, one above another below the exclude box.
      x_filt <- x[id_exc[i] - 1]
      id_par <- which(x == x_filt)
      id_par <- utils::tail(id_par[id_par < (id_exc[i] - 1)], 1)
      y[id_exc[i]] <- (y[id_par] + y[id_exc[i] - 1])/2

    }
  }

  y

}


#' @title is_class
#' @description Function to check if an object is from a given class.
#'
#'@param x element to check
#'@param class desired class to check
#'@keywords internal
#'
is_class <- function(x, class) {
  if (!inherits(x, class)) {
    stringr::str_glue("Expecting object of class {class}") |>
      stop(call. = FALSE)
  }
}
