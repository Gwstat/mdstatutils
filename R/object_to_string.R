object_to_string <- function(x) {
  x |>
    serialize(NULL, ascii = TRUE) |>
    rawToChar()
}

string_to_object <- function(x) {
  x |>
    charToRaw() |>
    unserialize()
}
