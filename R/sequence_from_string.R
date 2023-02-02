sequence_from_string <- function(x, sep = "-", sep2 = ", ") {

  x |>  strsplit(sep) |>
    lapply(function(i) {
      if(length(i) > 1) {
        out <-  as.integer(i[1]):as.integer(i[2]) |>
          stringr::str_pad(width = 3, side = "left", pad = "0") |>
          paste0(collapse = sep2)
      } else {
        out <- i
      }
      return(out)
    }) |> unlist()


}
