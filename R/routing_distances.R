routing_distances = function(coords_from,
                             names_from = NULL,
                             coords_to,
                             names_to = NULL,
                             method = "osrm",
                             osrm_server = NULL,
                             max_meters = NULL,
                             max_n = NULL,
                             osrm_per_seconds = 111.11,
                             loading_bar = TRUE


) {

  # loadNamespace("geosphere")




  if (!is.null(osrm_server)) {
    op <- getOption("osrm.server")
    options(osrm.server = osrm_server)
    on.exit(options(osrm.server = op))
  }





  # Checks coords_from ----
  if (!is.matrix(coords_from) |
      (ncol(coords_from) != 2) |
      nrow(coords_from) == 0) {
    stop(paste0("\n Error: coords_from has to be a matrix with dim(coords_from) = c(n,2) and n > 0."))
  }

  if(mode(coords_from) != "numeric") {
    coords_from <- coords_from |> as.numeric() |> matrix(ncol = 2)
    warning(paste0("\n Warning: coords_from converted to numeric matrix."))

  }

  if(any(is.na(coords_from))) {
    coords_from_which_complete <- coords_from |> apply(1, function(x) !any(is.na(x)))
    coords_from <- coords_from[coords_from_which_complete, ] |> matrix(ncol = 2)
    warning(paste0("\n Warning: Rows of coords_from containing NAs omitted."))
  }

  if(nrow(coords_from) == 0) {
    stop(paste0("\n Error: nrow(coords_from) is 0."))
  }

  if(!is.null(names_from)) {

    if(!is.vector(names_from)) {
      warning(paste0("Warning: names_from is no vector and is converted to NULL."))
      names_from <- NULL
    } else {
      if(exists("coords_from_which_complete")) {
        names_from <- names_from[coords_from_which_complete]
      }

      if(!(nrow(coords_from) == length(names_from))) {
        warning(paste0("Warning: nrow(coords_from) is not equal length(names_from). names_from converted to NULL."))
        names_from <- NULL
      }

    }
  }
  # Checks coords_to ----
  if (!is.matrix(coords_to) |
      (ncol(coords_to) != 2) |
      nrow(coords_to) == 0) {
    stop(paste0("\n Error: coords_to has to be a matrix with dim(coords_to) = c(n,2) and n > 0."))
  }

  if(mode(coords_to) != "numeric") {
    coords_to <- coords_to |> as.numeric() |> matrix(ncol = 2)
    warning(paste0("\n Warning: coords_to converted to numeric matrix."))

  }

  if(any(is.na(coords_to))) {
    coords_to_which_complete <- coords_to |> apply(1, function(x) !any(is.na(x)))
    coords_to <- coords_to[coords_to_which_complete, ] |> matrix(ncol = 2)
    warning(paste0("\n Warning: Rows of coords_to containing NAs omitted."))
  }

  if(nrow(coords_to) == 0) {
    stop(paste0("\n Error: nrow(coords_to) is 0."))
  }

  if(!is.null(names_to)) {

    if(!is.vector(names_to)) {
      warning(paste0("Warning: names_to is no vector and is converted to NULL."))
      names_to <- NULL
    } else {
      if(exists("coords_to_which_complete")) {
        names_to <- names_to[coords_to_which_complete]
      }

      if(!(nrow(coords_to) == length(names_to))) {
        warning(paste0("Warning: nrow(coords_to) is not equal length(names_to). names_to converted to NULL."))
        names_to <- NULL
      }

    }
  }

  # Checks method

  if (!(method %in% c("osrm", "euclidean"))) {
    stop(paste0("Error: Argument method has to be \"osrm\" or \"euclidean\"."))
  }
















  # long range -180 180
  # lat -90 90





  if (is.null(max_n)) {
    max_n <- length(coords_to)
  }






  dist_m <- geosphere::distm(coords_from,
                             coords_to)

  if (is.null(max_meters)) {
    max_meters <- max(dist_m)
  }



  choice <- lapply(1:nrow(dist_m),
                   function(i) {
                     which(
                       (dist_m[i,] <= sort(dist_m[i,])[max_n]) &
                         (dist_m[i,] <= max_meters)
                     )
                   }
  )




  if (method == "osrm") {

    calls <- lapply(choice, function(x) {length(x)}) |> unlist() |> sum()

    time <- calls/osrm_per_seconds


    if (loading_bar == TRUE) {
      print(paste0("Estimated time for ", calls, " interations: ",
                   round(time), " seconds (",
                   round(time/3600,2)," hours). Starttime: ",
                   Sys.time()))

    }


  }

  choice




  if (loading_bar == TRUE) { pb <- txtProgressBar(min = 0, max = nrow(coords_from), style = 3) }

  if (method == "osrm") {
    distance <- lapply(1:nrow(coords_from),
                       function(zeile) {
                         # print(paste0("Calculating distance ", text, "..."))

                         from <- c(coords_from[zeile,1], coords_from[zeile,2])

                         if (loading_bar == TRUE) {setTxtProgressBar(pb, zeile)}

                         if(length(choice[[zeile]]) > 0) {
                           out <- sapply(choice[[zeile]],
                                         function(spalte) {
                                           to <- c(coords_to[spalte,1],coords_to[spalte,2])
                                           dst <- osrm::osrmRoute(src = from, dst = to,
                                                                  overview=FALSE
                                                                  #,osrm.server = osrm_server
                                           )[2] * 1000

                                           # if (!is.null(names_to)) {
                                           #   dst <- dst |> setNames(names_to[spalte])
                                           # }

                                           dst


                                         }) #|> sort() |> as.data.frame()

                           # if (!is.null(names_to)) {
                           #   out <- data.frame(name = rownames(out),
                           #                   distance = out[,1])
                           # } else {
                           #   colnames(out) <- "distance"
                           # }




                           out




                         } else { NA }
                       }
    )
  } else if (method == "euclidean") {

    distance <-   lapply(1:length(choice), function(i) {

      out <- dist_m[i, choice[[i]]]


    })


  }

  # names_to ----
  distance <- lapply(1:length(distance),
                     function(i) {
                       v <- distance[[i]]
                       if (!is.null(names_to)) {
                         df <- data.frame(
                           name = names_to[choice[[i]]],
                           dist = v
                         )
                       } else {
                         df <- data.frame(
                           name = 1:length(v),
                           dist = v
                         )

                       }
                       df
                     })




  # names_from ----

  distance



  # Sorting ----
  distance <- lapply(1:length(distance),
                     function(i)
                     {
                       df <- distance[[i]]
                       df[order(df$dist),, drop = FALSE]

                     })

  if (!is.null(names_from)) {
    distance <- distance |> setNames(names_from)

  }

  return(distance)

}
