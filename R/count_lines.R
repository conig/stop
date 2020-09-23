#' count_lines
#'
#' counts lines of code in a directory
#' @param dir path to a folder
#' @param recursive default to true
#' @export

count_lines = function(dir, recursive = TRUE){

  files <- list.files(dir, pattern = "\\.r$", ignore.case = TRUE, recursive = recursive,
                      full.names = TRUE)
  result <- lapply(files, function(p) length(readLines(p)))
  sum(unlist(result))
}
