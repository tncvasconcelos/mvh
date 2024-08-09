#' Download a file
#'
#' An internal function to download a file from a URL.
#'
#' @param url A character string of the URL to download from.
#' @param destfile A character string of the destination file path.
#'
#' @keywords internal
download.file.int <- function(url, destfile) {
  tryCatch({
    download.file(url, destfile, mode = "wb")
  }, error = function(e) {
    message("Error downloading ", url)
  })
}

#' Create a directory
#'
#' This function creates a directory if it doesn't already exist.
#'
#' @param dir_name A character string of the directory name to create.
#'
#' @export
create_directory <- function(dir_name) {
  # Check if the directory already exists
  if (!dir.exists(dir_name)) {
    # Create the directory
    dir.create(dir_name)
    message("Directory '", dir_name, "' created successfully.")
  } else {
    message("Directory '", dir_name, "' already exists.")
  }
}
