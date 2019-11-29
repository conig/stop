#' unpack_fns
#'
#' Load all functions from a package into the global environment
#' @param package name
#' @param env environment
#' @export unpack_fns

unpack_fns = function(package, env = globalenv()){

  x = as.list(getNamespace(package))
  for(i in seq_along(x)){
    if(class(x[[i]]) == "function")

    assign(names(x)[i], x[[i]], envir = env)
    assign("%>%", dplyr::`%>%`, envir = env)
  }

}

#' peruse_fns
#'
#' Returns all functions from a package
#' @param package name
#' @param cat concatenate and print output?
#' @export peruse_fns

peruse_fns = function(package, cat = T){

  x = as.list(getNamespace(package))
  x = x[lapply(x, class) == "function"]

 out = paste(x, collapse = "\n")

 if(cat) return(cat(out))
 out

}



#' assign_args
#'
#' Load all argument defaults for a function for testing purposes
#' @param fn function name
#' @param env environment
#' @export assign_args

assign_args = function(fn, env = globalenv()){

  args = as.list(args(fn))
  args = args[nchar(names(args)) > 0]
  args = args[nchar(args) > 0 | is.null(args) | is.character(args)]

  args = args[names(args) != "..."]

  for(i in seq_along(args)){
    assign(names(args)[[i]], eval(args[[i]]), envir = env)
  }

}

#'find_fn
#'
#'find a function hiding in a folder of R docs
#'@param fn function name
#'@param folder folder name
#'@param open whether or not to try and open the file.
#'@export find_fn

find_fn = function(fn, folder = getwd(), open = T) {
  message("searching in ", getwd(),"...")
  files = list.files(path = folder,recursive = T, pattern = "\\.[rR]$", full.names = T) # all r files in dir

  sources = lapply(files, function(e) { # create list

  get_functions(e[[1]])

   # filter out non-functions
  })

  names(sources) = files # name each element with the file it was taken from

  file_name = names(unlist(sapply(sources, function(w) which(w %in% fn)))) # get the path containing the fn
  message("\n")
  if(nchar(file_name)>0) message("Found: ")
  message(file_name) # tell the user the file name
  if(open) shell.exec(file_name) # open the file, (windows only)

}

#' get_functions
#'
#' gets functions from source file without running it.
#' @param filename

get_functions = function(filename) {
  is_function = function (expr) {
    if (!is_assign(expr))
      return(FALSE)
    value = expr[[3]]
    is.call(value) && as.character(value[[1]]) == 'function'
  }

  function_name = function (expr){
    as.character(expr[[2]])
  }

  is_assign = function (expr){
    is.call(expr) &&
    as.character(expr[[1]]) %in% c('=', '<-', 'assign')
  }
  file_parsed = parse(filename) #parse file
  functions = Filter(is_function, file_parsed)
  function_names = unlist(Map(function_name, functions))

  return(function_names)

}

#' get_functions
#'
#' gets functions from source file without running it.
#' @param m commit message
#' @param branch branch name
#' @export update_git

update_git = function(m = "update", branch = NULL) {
  push = "git push"
  if (!is.null(branch)) {
    push = glue::glue("git push -u origin {branch}")
  }
  command = glue::glue('git add . & git commit -m "{m}" & {push}')
  message(command)
  shell(command)
}


