#' Expression Left-Join
#'
#' Left-Join a data frame with another based on parsed expressions.
#'
#' This function has been designed as a helper function to join a data frame with another based on
#' key column which is by expressions contained in a string defined for each data frame.
#' This may help to avoid to bloat the code with multiple data-pre-processing-steps by altering the structure
#' of the data frames to make them compatible for dplyrs left_join-function.
#'
#' @param data A data frame.
#' @param data2 A second data frame.
#' @param key_name A name for a interim key variable which is automatically removed afterwards. Is named "by_key" by default and should be changed if this column name is already present in data or data2.
#' @param data_expr A expression in a string containing the directions to produce the key variable in data.
#' @param data2_expr A expression in a string containing the directions to produce the key variable in data2.
#' @param use_cols A optional character vector. Specify the columns to extract in data2.
#'
#' @importFrom dplyr mutate left_join all_of select
#' @importFrom rlang sym parse_expr
#'
#' @return
#' @export
#'
#' @examples
left_join_expr <- function(data,
                        data2,
                        key_name = "by_key",
                        data_expr,
                        data2_expr,
                        use_cols = NULL) {



  join <- data2 |>
    dplyr::mutate(!!rlang::enquo(key_name) := !!rlang::parse_expr(data2_expr))

  if (!is.null(use_cols)) {
    join <- join |>    dplyr::select(dplyr::all_of(use_cols), dplyr::all_of(key_name))
  }


  data |>
    dplyr::mutate(!!rlang::enquo(key_name) := !!rlang::parse_expr(data_expr)) |>
    dplyr::left_join(join, by = key_name) |>   dplyr::select(-!!key_name)
}
