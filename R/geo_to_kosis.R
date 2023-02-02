geo_to_kosis <- function(Kosis, Hausnummern) {
  bind_coords(data = Kosis,
              data_coordinates = Hausnummern,
              key_name = "key",
              key_data = "paste0((.R03U1) |> as.character() |> stringr::str_pad(5,pad = '0'), .R03U2, .R03U3) |> tolower() |> stringr::str_trim(side = 'both')",
              key_coords = "paste0(Str_Nummer, Hausnummer) |> stringr::str_trim()",
              use_cols = c("Str_Name", "Hausnummer", "geometry"))

}
