
area2d_kde_on_point <- function(Points = Hausnummern |> sf::st_coordinates() |> as.matrix(),
                                Pointnames = Hausnummern |> dplyr::pull(Adresse),
                                polygons = STT,
                                col = "Straßenkriminalität",
                                burnin = 5,
                                samples = 10,
                                gridsize = 100) {

  kdes <- area2d_kde(polygons = polygons,
                     col = col,
                     burnin = burnin,
                     samples = samples,
                     gridsize = gridsize,
                     spatial_object = FALSE)



  dt1 <-data.table::data.table(Points)
  dt2 <- data.table::data.table(as.data.frame(kdes[,1:2]))

  out <- dt1[, nearest_dt2 := apply(raster::pointDistance(as.matrix(dt1),
                                                          as.matrix(dt2),
                                                          lonlat = FALSE), 1,
                                    which.min)][] |>
    tibble::as.tibble() |>
    (\(x) {

      density <- kdes[x$nearest_dt2,3]

      x |>  dplyr::mutate(dens = density)

    })()

  if (!is.null(Pointnames)) {
    out <- out |> dplyr::mutate(name = Pointnames)
  }



}
