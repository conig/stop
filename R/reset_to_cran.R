
#' reset_packages_to_cran
#'
#' finds version mismatches and updates back to current cran version
#' @export reset_packages_to_cran

reset_packages_to_cran = function(){

  pc = data.table::data.table(utils::installed.packages())

  new = data.table::data.table(remotes::available_packages())
  ok = pc[pc$Package %in% new$Package,]

  for(i in seq_along(ok$Package)){
    message(ok$Package[i], i)
    temp_new = new$Version[new$Package == ok$Package[i]]
    current_version = ok$Version[i]

    if(temp_new != current_version){
      message("CRAN version is different!!")
      utils::install.packages(ok$Package[i])

    }

  }


}

