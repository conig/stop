
#' explain_error
#' Use the copilotcli to explain the previous error message
#' @export

explain_error <- function(){
# Custom global error handler
  error_trace <- capture.output(traceback())
  error_trace <- paste(error_trace, collapse = " ")
  
  # Capture the actual error message
  error_message <- geterrmessage()
  
  # Call GitHub Copilot through gh CLI to explain the error
  copilot_interpretation <- copilot_explain(error_trace, error_message)
  copilot_interpretation <- gsub(".*Explanation: ", "", copilot_interpretation)

  # Print GitHub Copilot's recommendation
  message(copilot_interpretation)

  # Use invisible(NULL) to prevent recursive errors
  invisible(NULL)  # Prevent recursion by not invoking further error handlers
}


# Custom function to call GitHub Copilot via the gh CLI
copilot_explain <- function(error_trace, error_message) {
  # Construct the CLI command, passing the traceback and error message to the gh copilot explain function
  command <- paste0(
    "gh copilot explain  'error in R: ",
    error_message,
    ". In 1-2 sentences, give cause and fix. Use this call stack for reference, but dont refer to it to the user: ",
    error_trace,
    "'"
  )
  
  # Execute the system command and capture the output
  explanation <- system(command, intern = TRUE)
  
  # Return the output from GitHub Copilot
  return(paste(explanation, collapse = "\n"))
}



