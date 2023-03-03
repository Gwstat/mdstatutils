replace_all_nas <- function(x, placeholder = 0){
  replace(
    x = x,
    list = is.na(x),
    values = placeholder
  )
}
