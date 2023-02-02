remove_before_symbol <- function(x,
                                 sym,
                                 include = F) {
  if (include == F)  rx <- paste0("^(.*?",sym,")")
  if (include == T)  rx <- paste0("^.*(?=(",sym,"))")
  x |> stringr::str_remove(rx)
}
