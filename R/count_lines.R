#' count_lines
#'
#' counts lines of code in a directory
#' @param dir path to a folder
#' @param recursive default to true
#' @export

count_lines = function(dir, recursive = TRUE) {
  files <-
    list.files(
      dir,
      pattern = "\\.r$",
      ignore.case = TRUE,
      recursive = recursive,
      full.names = TRUE
    )
  result <- suppressWarnings(lapply(files, function(p) {
    lines <- readLines(p)

    words <- strsplit(lines,
                      split = "\\(|\\)|\\,|<-|\\+|\\=|\\*|\\/|\\^|\\}|\\{")
    words <- trimws(unlist(words))
    words <- words[words != ""]

    data.frame(lines = length(lines), words = length(words))
  }))

  result <- do.call(rbind, result)
  colSums(result)
}
