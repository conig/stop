#' store
#' @param x object to store
#' @export

store <- function(x = NULL){
  path = paste0(system.file(package = "stop"), "/store.rds")
  if(is.null(x)) return(readRDS(path))
  saveRDS(x,path)
}
