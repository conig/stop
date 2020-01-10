# functions for manipulating file paths

#' dropbox_root
#'
#' Finds the root of a dropbox path. Useful if you use dropbox on two different machines and you want your paths to work.
#' @export dropbox_root

dropbox_root = function(){

  u_p = Sys.getenv("USERPROFILE")
  files = list.files(u_p, full.names = T)
  is_db = grepl("Dropbox", files)

  normalizePath(files[is_db])

}

#' explore_wd
#'
#' explore working directory
#' @export explore_wd

explore_wd = function(){
  shell.exec(getwd())

}
