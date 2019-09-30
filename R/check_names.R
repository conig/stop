#' check_names
#'
#' Checks a data.frame, matrix, or list for a set of names. If they don't match, best matches are provided in a message to the user
#' @param x a data.frame, matrix, or list
#' @param vars column names to check
#' @export check_names


check_names <- function (x, vars) {
  vars = unique(vars)

  name_data = names(data.frame(x))

  error_names = vars[!vars %in% name_data]

  find_name = function(n) {
    prob_name = name_data[agrep(n, name_data)]

    if (length(prob_name) == 1) {
      return(glue::glue("'{n}' ['{prob_name}'?]"))
    } else{
      return(n)
    }


  }

  error_names = unlist(lapply(error_names, find_name))

  ifelse(length(error_names) > 1, stem <- "names", "name")

  mess1 = glue::glue(
    "{length(error_names)} {stem} could not be found: {paste(error_names, collapse = ', ')}."
  )

  if (length(error_names) > 0) {
    stop(mess1, call. = F)
  }

}

