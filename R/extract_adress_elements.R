extract_adress_elements <- function(x, output = "str") {
  hnz <- stringr::str_sub(x, last_digit(x)+1, nchar(x))   |> stringr::str_trim() |>
    gsub(pattern = " ", replacement = "") |> tolower()


  str <- stringr::str_remove(x, "([0-9 0-9 0-9\\-]+)(?!.*[0-9 0-9 0-9\\-])")
  str <- stringr::str_sub(str, 1, nchar(str)- nchar(hnz))

  hn  <- stringr::str_extract(x, "([0-9 0-9 0-9\\-]+)(?!.*[0-9 0-9 0-9\\-])")  |>
    gsub(pattern = " ", replacement = "")


  # if (nchar(hnz) > 0) {str <- stringr::str_sub(str, 1, nchar(str)-1)}

  if (output == "str") {out <- str}
  if (output == "hn") {out <- hn}
  if (output == "hnz") {out <- hnz}

  return(out)

}
