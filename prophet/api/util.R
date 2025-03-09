send_http_error <- function(res, code, message) {
  res$status <- code
  list(error=message)
}

convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}

get_with_default <- function(value, default) {
  if (is.null(value) || value == "") {
    default
  } else {
    value
  }
}
