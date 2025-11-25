#' summary method for `fc` object
#'
#' @title summary.fc
#' @param object À `fc` object created using flowchart functions
#' @param ... Not used
#' @export
summary.fc <- function(object, ...) {

  is_class(object, "fc")

  # build matrix: rows = ids, cols = filters, splits
  if(length(object) == 2) {

    boxes <- nrow(object$fc)
    filters <- sum(object$fc$type == 'filter')
    splits <- sum(object$fc$type == 'split')
    mat <- matrix(c(boxes, filters, splits), nrow = 1, ncol = 3, byrow = FALSE)
    colnames(mat) <- c("boxes", "filters", "splits")
    rownames(mat) <- 1

    tbl <- as.table(mat)

    return(tbl)

  } else {

    boxes <- purrr::map_dbl(object$fc, ~nrow(.))
    filters <- purrr::map_dbl(object$fc, ~sum(.$type == 'filter'))
    splits <- purrr::map_dbl(object$fc, ~sum(.$type == 'split'))
    mat <- matrix(c(boxes, filters, splits), nrow = length(object$fc), ncol = 3, byrow = FALSE)
    colnames(mat) <- c("boxes", "filters", "splits")
    rownames(mat) <- object$id

    tbl <- as.table(mat)

    return(tbl)

  }

}
