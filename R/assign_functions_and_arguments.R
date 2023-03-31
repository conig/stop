#' unpack_fns
#'
#' Load all functions from a package into the global environment
#' @param package name
#' @param env environment
#' @param deps a bool. If true, dependencies are also loaded.
#' @export unpack_fns

unpack_fns = function(package, env = globalenv(), deps = FALSE){

  x = as.list(getNamespace(package))
  for(i in seq_along(x)){
    if("function" %in% class(x[[i]]))

    assign(names(x)[i], x[[i]], envir = env)
    assign("%>%", poorman::`%>%`, envir = env)
  }

  if(deps) load_dependencies(package)

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

assign_args = function(fn = rstudioapi::getSourceEditorContext(), env = globalenv()){

  args = as.list(args(fn))
  args = args[nchar(names(args)) > 0]
  args = args[nchar(args) > 0 | is.null(args) | is.character(args)]

  args = args[names(args) != "..."]

  for(i in seq_along(args)){
    assign(names(args)[[i]], eval(args[[i]]), envir = env)
  }

}

#' assign_targets
#'
#' Load all argument defaults for a function for testing purposes
#' @param fn function name
#' @param env environment
#' @export

assign_targets = function(fn, env = globalenv()){

  args = as.list(args(fn))
  args = names(args)

  for(i in seq_along(args)){
   targets::tar_load(args[[i]], envir = env)
  }

}

#' assign_targets.addin
#'
#' assign_targets.addin

assign_targets.addin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  requireNamespace("targets")
  targets::tar_load_globals()
   # assign("context", context, envir = globalenv())

  if (length(context$selection) == 0) {
    return(
      rstudioapi::showDialog(title = ":'(", message = "This addin will not work in the visual markdown editor. Please switch to the source editor.")
    )
  }

  contents <- context$contents
  start <- context$selection[[1]]$range$start
  end <- context$selection[[1]]$range$end

  section <- contents[start[1]:end[1]]
  start_trim <- substring(section[1], 1, start[2] - 1)
  end_trim <-
    substring(section[length(section)], end[2], nchar(section[length(section)]))

  if (nchar(start_trim) > 0) {
    section[1] <- gsub(start_trim, "", section[1], fixed = TRUE)
  }

  if(nchar(end_trim) > 0){
  section[length(section)] <-
    gsub(end_trim, "", section[length(section)], fixed = TRUE)
  }

  section <- unlist(strsplit(section, split = ","))
  section <- trimws(section)

  cat("loading targets...\n")
  for (i in seq_along(section)) {
    targets::tar_load(section[i], envir = globalenv())
    if (section[i] %in% ls(envir = globalenv())) {
      cat(crayon::green(glue::glue("({i}/{length(section)}) {section[i]} loaded"), "\n"))
    } else{
      cat(crayon::red(glue::glue("({i}/{length(section)}) {section[i]} failed"), "\n"))
    }
  }
}


#' evaluate.arguments
#'
#' evaluate_arguments.addin

evaluate_arguments.addin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  #assign("context", context, envir = globalenv())
  if (length(context$selection) == 0) {
    return(
      rstudioapi::showDialog(title = ":'(", message = "This addin will not work in the visual markdown editor. Please switch to the source editor.")
    )
  }

  contents <- context$contents
  start <- context$selection[[1]]$range$start
  end <- context$selection[[1]]$range$end

  section <- contents[start[1]:end[1]]
  start_trim <- substring(section[1], 1, start[2] - 1)
  end_trim <-
    substring(section[length(section)], end[2], nchar(section[length(section)]))

  if (nchar(start_trim) > 0) {
    section[1] <- gsub(start_trim, "", section[1], fixed = TRUE)
  }

  if (nchar(end_trim) > 0) {
    section[length(section)] <-
      gsub(end_trim, "", section[length(section)], fixed = TRUE)
  }

  section <- unlist(strsplit(section, split = ","))
  section <- trimws(section)

  eval_names <- gsub(" \\=.*", "", section)

  cat("evaluating arguments...\n")
  for (i in seq_along(section)) {
    eval(parse(text = section[i]), envir = globalenv())

    if (eval_names[i] %in% ls(envir = globalenv())) {
      cat(crayon::green(
        glue::glue("({i}/{length(eval_names)}) {eval_names[i]} loaded"),
        "\n"
      ))
    } else{
      cat(crayon::red(
        glue::glue("({i}/{length(v)}) {eval_names[i]} failed"),
        "\n"
      ))

    }

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
#' @param path the filepath you wish to process

get_functions = function(path) {
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
  file_parsed = parse(path) #parse file
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

#' load_dependencies
#'
#' loads all dependencies for a package
#' @param package a string
#' @export load_dependencies

load_dependencies = function(package){
  deps = unlist(tools::package_dependencies(package))

  for(i in seq_along(deps)){
    require(deps[i], character.only = TRUE)
  }

}

#' clip_fun
#'
#' @param fun a function name
#' @export

clip_fun = function(fun){
  fun <- paste(as.character(deparse(fun)), collapse = "\n")
  clipr::write_clip(fun)
}

