kde_on_points <- function(griddata,
                          data,
                          bandwidth = 500,
                          rel = TRUE)  {
  data |>
    sf::st_transform("+init=epsg:3857") |>
    SpatialKDE::kde(band_width = 500,
                    grid = griddata |> sf::st_transform("+init=epsg:3857")) |>
    dplyr::pull(kde_value) |>
    (\(x) {
      if (rel == TRUE) { x <- x/max(x, na.rm = TRUE)}
      x
    })()
}
