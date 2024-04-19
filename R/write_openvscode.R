#' write_opencode
#'
#' Write a file that opens vscode in the enclosing working directory
#' @param dir path to a folder
#' @export write_opencode
#' @details Only works for windows computers.

write_opencode <- function(dir) {
  if(Sys.info()["sysname"] != "Windows") stop("Only works on windows")
  filepath <- system.file("open vscode.bat", package = "stop")
  file.copy(filepath, dir)
}
