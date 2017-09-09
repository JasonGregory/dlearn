#' Returns a list subset.
#'
#' @param data Data frame that you want to explore.
#' @param column Column name that you want to select.
#' @param list_name For non-list columns keep names for automic vectors. Default is FALSE.
#' @return A subset of a list.
#' @export

lselect <- function(data, column, list_name = FALSE) {
  column <- enquo(column)
  col <- data %>% select(!!column)
  col_class <- class(col[[1]][1])

  if (col_class == "list") {
    # Later use similar logic as this to determine the type of list. class(temp[[1]][[1]])[1]
    ldata <- col %>% tidyr::unnest(.id = "name")
  } else {
    ldata <- col %>% unlist(use.names = list_name)
  }
  return(ldata)
}
