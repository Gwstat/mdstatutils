unify_streetvector <- function(x) {

    out <- x |>
      tolower() |>
      gsub(pattern = "ä", replacement = "ae") |>
      gsub(pattern = "ü", replacement = "ue") |>
      gsub(pattern = "ö", replacement = "oe") |>
      gsub(pattern = "ß", replacement = "ss") |>
      gsub(pattern = "-", replacement = "") |>
      gsub(pattern = " ", replacement = "") |>
      gsub(pattern = "str[.]", replacement = "strasse") |>
      gsub(pattern = "\\.", replacement = "")

  return(out)
}
