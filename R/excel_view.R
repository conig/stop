#' view_csv
#'
#' Opens a data.frame in excel
#' @param df the data.frame object
#' @export view_csv

view_csv = function(df){
  tf = tempfile()
  location = paste0(tf,".csv")
  utils::write.csv(
    poorman::rownames_to_column(df),
    location,
    row.names = FALSE)
  shell.exec(location)
}
