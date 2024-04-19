#' search_targets
#'
#' Search for target names within a manifest
#' @param pattern pattern to search for
#' @export

search_targets <- function(pattern) {
  require(data.table)
  man <- targets::tar_manifest() |>
    data.table()
  man$name[man$name %like% pattern]
}
