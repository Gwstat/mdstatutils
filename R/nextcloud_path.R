nextcloud_path <- function(x) {
  Userdir <- paste0("C:/Users/", as.character(Sys.info()[which(names(Sys.info()) == "effective_user")]))
  Ncloudname <- list.files(Userdir)[which(startsWith(list.files(Userdir), "Nextcloud"))] |> tail(1)
  Nextcloud_Pfad <- paste0(Userdir, "/", Ncloudname)
  return(Nextcloud_Pfad)
}
