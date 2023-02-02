remove_after_symbol <- function(x,
                                sym,
                                include = F) {
  if (include == T)  rx <- paste0("^(.*?",sym,")")
  if (include == F)  rx <- paste0("^.*(?=(",sym,"))")
  x |> stringr::str_extract(rx)
}
