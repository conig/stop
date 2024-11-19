#' safe_numeric
#'
#' Convert a vector to numeric, but stop if unknown values could not be converted
#'
#' @param x A vector to convert to numeric
#' @param ignore A vector of values to ignore when checking for unknown values
#' @return A numeric vector
#' @export

safe_numeric <- function(x, ignore = c()) {
  # compare the original vector with the numeric conversion
  suppressWarnings(x_num <- as.numeric(x))
  vals <- x[is.na(x_num)]
  u_vals <- na.omit(unique(vals))
  # If ignore is set to NULL, do not perform checks
  if (!is.null(ignore)) {
    u_vals <- u_vals[!u_vals %in% ignore]
    # Warn if unknown charcters being eliminated
    if (length(u_vals) > 0) {
      stop(paste("The following values could not be converted to numeric: ", paste(u_vals, collapse = ", ")))
    }
  }
  x_num
}
