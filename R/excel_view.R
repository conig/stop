#' view_csv
#'
#' Opens a data.frame in excel
#' @param df the data.frame object
#' @export view_csv

view_csv = function(df){
  tf = tempfile()
  location = paste0(tf,".csv")
  utils::write.csv(df,location)
  shell.exec(location)
}
