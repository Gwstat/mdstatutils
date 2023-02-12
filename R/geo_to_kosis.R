geo_to_kosis <- function(Kosis, Hausnummern) {
  left_join_expr(data = Kosis,
              data2 = Hausnummern,
              data_expr = "paste0((.R03U1) |> as.character() |> stringr::str_pad(5,pad = '0'), .R03U2, .R03U3) |> tolower() |> stringr::str_trim(side = 'both')",
              data2_expr = "(paste0(Str_Nummer, Hausnummer) |> stringr::str_trim())",
              use_cols = c("Str_Name", "Hausnummer", "geometry"))

}
