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

# mdstatutils::nextcloud_path() |>
#   paste0("/Amt12/12.2/Ressourcen") |>
#   paste0("/Lagedaten Mietspiegel") |>
#   paste0("/SGB2") |>
#   list.files(full.names = T) |>
#   openxlsx::read.xlsx()


# BA_to_STB <- mdstatutils::nextcloud_path() |>
#   paste0("/SGBII-Daten") |>
#   paste0("/BA-Einteilung-STB_Stadtviertel_endgültig.xlsx") |>
#   openxlsx::read.xlsx(startRow = 8) |>
#   tibble::as_tibble() |>
#   dplyr::mutate(Bezirke = stringr::str_pad(Bezirke, width = 3, side = "left", pad = "0")) |>
#   dplyr::mutate(STBalt = dplyr::coalesce(STBalt, Bezirke)) |>
#   dplyr::rename(Stadtviertel_Name = 2) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern = "Krökentorviertel/Breiter Weg",
#                                          replacement = "Krökentorviertel/Breiter Weg NA",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern = "SV Hohepfortestrasse",
#                                          replacement = "SV Hohepfortestraße",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern = "NF Ost",
#                                          replacement = "Neustädter Feld Ost",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern = "Meseberger/Milchweg",
#                                          replacement = "Meseberger Weg/Milchweg",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern = "Weizengrund/Birkenallee/Röthe",
#                                          replacement = "Weizengrund/Birkenallee",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern =     "Alt Diesdorf/Niederndodeleber Str.",
#                                          replacement = "Alt Diesdorf/Niederndodeler Str.",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern =     "Viertel Universitätsklinikum",
#                                          replacement = "Viertel Universtätsklinikum",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern =     "Engpaß/Gewerbegebiet Buckau",
#                                          replacement = "Engpaß/gewerbegebiet Buckau",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern =     "SV Alt Westerhüsen Ost",
#                                          replacement = "Alt Westerhüsen Ost",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern =     "WG Welsleber Straße",
#                                          replacement = "Wohngebiet Welsleber Straße",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern =     "St.Pauli/A.-Puschkin-Straße",
#                                          replacement = "St. Pauli/Alexander-Puschkin-Straße",
#                                          Stadtviertel_Name)) |>
#   dplyr::mutate(Stadtviertel_Name = gsub(pattern =     "Beimssviertel",
#                                          replacement = "Beimsviertel",
#                                          Stadtviertel_Name)) |>
#   dplyr::left_join(mdstatutils::nextcloud_path() |>
#                      paste0("/SGBII-Daten")  |>
#                      paste0("/BA_STB seit 2018.xlsx") |>
#                      openxlsx::read.xlsx() |>
#                      dplyr::mutate(Stadtviertel = stringr::str_pad(Stadtviertel, 9, "left", "0")),
#                    by = "Stadtviertel_Name")
#
#
#
#
# AK <- mdstatutils::nextcloud_path() |>
#   paste0("/SGBII-Daten")  |>
#   paste0("/15003000_AK_2021.CSV") |>
#   read.csv2(header = FALSE) |>
#   tibble::as_tibble() |>
#   dplyr::select(ID = 1, key = 2)
#
# # BA_keys <-  nextcloud_path() |>
# #   paste0("/SGBII-Daten")  |>
# #   paste0("/BA_STB seit 2018.xlsx") |>
# #   openxlsx::read.xlsx() |>
# #   dplyr::mutate(Stadtviertel = stringr::str_pad(Stadtviertel, 9, "left", "0"))
#
# BG_Daten <- mdstatutils::nextcloud_path() |>
#   paste0("/SGBII-Daten")  |>
#   paste0("/15003000_BG_2021.CSV") |>
#   read.csv2(header = FALSE) |>
#   dplyr::filter(V2 == 15003000) |>  # Magdeburg
#   tibble::as_tibble() |>
#   dplyr::select(ID = V1, n = V6) |>
#   dplyr::left_join(AK, by = "ID") |>
#   dplyr::rename(Stadtviertel = key) |>
#   dplyr::group_by(Stadtviertel) |>
#   dplyr::summarise(n = sum(n, na.rm = TRUE)) |>
#   dplyr::left_join(BA_to_STB, by = "Stadtviertel") |>
#   dplyr::select(Gemeindeschlüssel, Stadtviertel, Stadtviertel_Name, BA_Bezirk = Bezirke, STBs = STBalt, n)
#
# BA_STB_Shape <- mdstatutils::nextcloud_path() |>
#   paste0("/Ressourcen") |>
#   paste0("/Shapefiles") |>
#   paste0("/Abgrenzung") |>
#   paste0("/BA_STB_30_07_2018.shp") |>
#   mdstatutils::shape_as_longlat() |>
#   dplyr::mutate(SBZ_Nummer = stringr::str_pad(SBZ_Nummer, 3, "left", "0")) |>
#   dplyr::select(STB, geometry) |>
#   dplyr::left_join(BG_Daten |> dplyr::select(STB = BA_Bezirk, n), by = "STB")  |>
#   dplyr::select(-STB) |>
#   dplyr::mutate(n = dplyr::coalesce(n, 0))

# Spalte N für Anzahl SGB II
# estTurk <- dshapebivrProp(data = dataTurk,
#                           burnin = 5,
#                           samples = 10,
#                           adaptive = FALSE,
#                           deleteShapes = berlinUnInhabitated,
#                           shapefile = berlin,
#                           gridsize = 325,
#                           boundary = TRUE,
#                           numChains = 4,
#                           numThreads = 4)

