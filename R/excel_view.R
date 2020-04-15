#' view_csv
#'
#' Opens a data.frame in excel
#' @param df the data.frame object
#' @export view_csv

view_csv = function(df){
  location = tempfile(fileext = ".csv")
  df = poorman::rownames_to_column(data.frame(df))
  utils::write.csv(
    df,
    location,
    row.names = FALSE)
  Sys.sleep(0.1)
  shell.exec(location)
}
