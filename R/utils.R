# Internal functions used in the package:

#' @title update_x
#' @description Function to update the horizontal position of the previuosly created flow charts, to merge two flowcharts used in fc_merge().
#'
#'@param x old horizontal position of the boxes
#'@param i position of the flowchart within the list of flowcharts.
#'@param n number of total flowcharts to merge
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
#'
update_y <- function(y, type, x) {

  yold <- unique(y[type != "exclude"])
  yval <-  rev(seq(0, 1, by = 1/(length(yold) + 1)))
  yval <- yval[!yval %in% c(0, 1)]

  for(i in 1:length(yold)) {

    y <- dplyr::case_when(
      y %in% yold[i] ~ yval[i],
      TRUE ~ y
    )

  }

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

#' @title update_y_stack
#' @description Function to update the vertical position of the previuosly created flow charts, to stack the two flowcharts with fc_stack().
#'
#'@param y old vertical position of the boxes
#'@param x old horizontal position of the boxes
#'@param type type of the boxes
#'@param i position of the flowchart within the list of flowcharts.
#'@param n number of total flowcharts to merge
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


#' @title is_class
#' @description Function to check if an object is from a given class.
#'
#'@param x element to check
#'@param class desired class to check
#'
is_class <- function(x, class) {
  if (!inherits(x, class)) {
    stringr::str_glue("Expecting object of class {class}") |>
      stop(call. = FALSE)
  }
}
