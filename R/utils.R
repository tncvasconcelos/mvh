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
  wkt_square_polygon <- st_as_text(square_polygon_sf)
  return(wkt_square_polygon)
}


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
