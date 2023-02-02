bind_coords <- function(data,
                        data_coordinates,
                        key_name,
                        key_data,
                        key_coords,
                        use_cols) {
  data |>
    dplyr::mutate(!!rlang::sym(key_name) := !!rlang::parse_expr(key_data)) |>
    dplyr::left_join(
      data_coordinates |> dplyr::mutate(!!rlang::sym(key_name) := !!rlang::parse_expr(key_coords)) |> dplyr::select(all_of(use_cols), all_of(key_name)),
      by = key_name) |>   dplyr::select(-!!key_name)
}
