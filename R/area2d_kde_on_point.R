
#' Title
#'
#' @param Points
#' @param Pointnames
#' @param polygons
#' @param col
#' @param burnin
#' @param samples
#' @param gridsize
#'
#' @return
#' @import data.table
#' @export
#'
#' @examples
area2d_kde_on_point <- function(Points = Hausnummern,
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


  dt1 <- Points |> sf::st_coordinates() |> as.matrix()
  dt2 <- as.data.frame(kdes[,1:2])
  out <- dt1 |>
    cbind(RANN::nn2(dt2, dt1, k = 1) |> sapply(cbind) ) |> tibble::as_tibble() |>
    (\(x) {
      density <- kdes[x$nn.idx,3]
      x |>  dplyr::mutate(dens = density)
    })()

  if (!is.null(Pointnames)) {
    out <- out |> dplyr::mutate(name = Pointnames)
  }

  return(out)

}
