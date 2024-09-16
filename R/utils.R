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


print_names <- function(names) {
  n <- length(names)
  if (n == 1) {
    c(names)
  } else if (n == 2) {
    c(names[1], "&", names[2])
  } else {
    c(paste(names[1:(n-1)], collapse = ", "), "&", names[n])
  }
}


#' Convert Coordinates to WKT Square Polygon
#'
#' This function generates a square polygon in Well-Known Text (WKT) format
#' centered on the provided coordinates and with the specified buffer distance.
#'
#' @param lat Numeric. Latitude of the center of the square.
#' @param lon Numeric. Longitude of the center of the square.
#' @param buffer_distance Numeric. The distance from the center to each side of the square.
#'
#' @return A character string representing the square polygon in WKT format.
#' @importFrom sf st_polygon st_sfc st_as_text
#' @export
#'
#' @examples
#' coordinates_to_wkt_square_polygon(lat = 45.0, lon = -93.0, buffer_distance = 0.01)
coordinates_to_wkt_square_polygon <- function(lat,lon,buffer_distance) {
  # Calculate the coordinates of the square's corners
  square_coords <- matrix(c(
    lon - buffer_distance, lat - buffer_distance,  # Bottom-left corner
    lon + buffer_distance, lat - buffer_distance,  # Bottom-right corner
    lon + buffer_distance, lat + buffer_distance,  # Top-right corner
    lon - buffer_distance, lat + buffer_distance,  # Top-left corner
    lon - buffer_distance, lat - buffer_distance   # Closing the square (same as bottom-left)
  ), ncol = 2, byrow = TRUE)
  # Create the square polygon
  square_polygon <- sf::st_polygon(list(square_coords))
  square_polygon_sf <- sf::st_sfc(square_polygon, crs = 4326)
  # Convert the square polygon to WKT format
  wkt_square_polygon <- sf::st_as_text(square_polygon_sf)
  return(wkt_square_polygon)
}

#' Download a File Safely
#'
#' This function downloads a file from a URL to a specified destination. If a file with the
#' same name already exists, it appends a counter to the file name to avoid overwriting.
#'
#' @param url Character. The URL from which to download the file.
#' @param destfile Character. The destination file path where the downloaded file will be saved.
#'
#' @return A character string representing the path to the saved file.
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils download.file
#'
download_file_safe <- function(url, destfile) {
  # Check if the file already exists
  if (file.exists(destfile)) {
    # Get the file extension
    file_extension <- tools::file_ext(destfile)
    # Get the base name without the extension
    base_name <- tools::file_path_sans_ext(destfile)
    counter <- 1
    # Loop until we find a non-existing file name
    while (file.exists(destfile)) {
      # Create a new file name with the counter
      destfile <- paste0(base_name, "_", counter, ".", file_extension)
      counter <- counter + 1
    }
  }
  # Download the file with the new or original name
  download.file(url, destfile)
  # Return the name of the file that was saved
  return(destfile)
}

#' Download a File with Retry Mechanism
#'
#' This function attempts to download a file from a URL multiple times until
#' it succeeds or the maximum number of attempts is reached.
#'
#' @param url Character. The URL from which to download the file.
#' @param destfile Character. The destination file path where the downloaded file will be saved.
#' @param max_attempts Integer. The maximum number of download attempts before giving up.
#'
#' @return Logical. TRUE if the download succeeded, FALSE otherwise.
#'
download_with_retry <- function(url, destfile, max_attempts) {
  attempt <- 1
  while (attempt <= max_attempts) {
    tryCatch({
      download_file_safe(url, destfile)
      return(TRUE)
    }, error = function(e) {
      message("Attempt ", attempt, " failed: ", e$message)
      attempt <- attempt + 1
      Sys.sleep(2)  # Sleep before retrying
    })
  }
  message("Failed to download after ", max_attempts, " attempts: ", url)
  return(FALSE)
}
