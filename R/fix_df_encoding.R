fix_df_encoding <- function(data, encoding = "UTF-8") {
  x <- colnames(data)
  Encoding(x) <- encoding
  colnames(data) <- x

  data <- data |>
    dplyr::mutate_if(is.character, function(spalte) {
      Encoding(spalte) <- encoding
      return(spalte)
    }) |>
    dplyr::mutate_if(is.factor, function(spalte) {

      spalte <- as.character(spalte)
      Encoding(spalte) <- encoding
      spalte <- as.factor(spalte)
      return(spalte)

    })


  return(data)

}
