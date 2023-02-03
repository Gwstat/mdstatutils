shape_as_longlat <- function(file,
                             enc = "ENCODING=WINDOWS-1252",
                             force_utf = F){
  if (force_utf == T) {
    enc <- "ENCODING=UTF-8"
  }
  #

  file |>
    sf::st_read(options = enc) |>
    sf::st_transform(4326) |>
    (\(x) {
      sf::st_crs(x) <- "+proj=longlat +init=epsg:4326"
      x
    })()
}
