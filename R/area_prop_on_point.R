area_prop_on_point <- function(Points,
                               Areas,
                               radius = 1000,
                               proportion = TRUE,
                               pb = TRUE) {


  Points |>
    sf::st_buffer(radius) |>
    sf::st_intersection(Areas) |>
    # spawn_progressbar() |>
    (\(x) {
      x$area <- sf::st_area(x)
      x
    })()


}
