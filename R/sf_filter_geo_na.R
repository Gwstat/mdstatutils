sf_filter_geo_na <- function(df, show.complete = T) {
  if (show.complete == T) {  df |>  (function(x) x |> dplyr::filter(!is.na(sf::st_dimension(x))))() }
  if (show.complete == F) {  df |>  (function(x) x |> dplyr::filter(is.na(sf::st_dimension(x))))() }
}
