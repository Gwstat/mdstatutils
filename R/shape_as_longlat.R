shape_as_longlat <- function(file,
                             enc = "ENCODING=WINDOWS-1252") {
  # "ENCODING=UTF-8"

  file |>
    sf::st_read(options = enc) |>
    sf::st_transform(4326) |>
    (\(x) {
      sf::st_crs(x) <- "+proj=longlat +init=epsg:4326"
      x
    })()
}
