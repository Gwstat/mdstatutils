area2d_kde <- function(polygons = STT,
                       col = "Straßenkriminalität",
                       burnin = 5,
                       samples = 10,
                       gridsize = 100,
                       spatial_object = FALSE) {
  data <- polygons |> dplyr::mutate(centroid = sf::st_centroid(geometry)) |>
    dplyr::pull(centroid) |>
    sf::st_coordinates() |>
    as.data.frame() |>
    cbind(polygons |> dplyr::pull(col)) |>
    dplyr::rename(N = 3)

  Shapefile <- polygons |>
    dplyr::select(geometry, N = dplyr::all_of(col))  |> sf::st_zm() |> sf::as_Spatial()


  est <- Kernelheaping::dshapebivr(data = data,
                                   burnin = burnin,
                                   samples = samples,
                                   adaptive = FALSE,
                                   shapefile = Shapefile,
                                   gridsize = gridsize,
                                   boundary = TRUE)

  if (spatial_object == FALSE) {

    est <- data.frame(expand.grid(long = est$Mestimates$eval.points[[1]],
                                  lat = est$Mestimates$eval.points[[2]]),
                      Density = est$Mestimates$estimate |>  as.vector())

  }
  return(est)
}

