first_digit <- function(x) {
  x |> stringr::str_locate_all("[:digit:]") |>
    lapply(function(x) as.data.frame(x)) |>
    lapply(function(x) x[,-2]) |>
    lapply(function(x) ifelse(length(x) > 0, head(x,1), NA)) |>
    unlist()
}
