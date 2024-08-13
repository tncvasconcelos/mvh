#' Resize an image
#'
#' This function resizes an image to fall within a specified megapixel range.
#'
#' @param file_name A character string of the file path of the image to resize.
#' @param min_megapixels A numeric value of the minimum desired megapixels.
#' @param max_megapixels A numeric value of the maximum desired megapixels.
#'
#' @return A magick image object of the resized image.
#'
#' @importFrom magick image_read image_info image_resize image_write geometry_size_pixels
#' @export
resize.image <- function(file_name, min_megapixels = 20, max_megapixels = 25) {
  # Load the image
  img <- magick::image_read(file_name)

  # Get the current dimensions of the image
  current_width <- magick::image_info(img)$width
  current_height <- magick::image_info(img)$height

  # Calculate the current megapixels
  current_megapixels <- (current_width * current_height) / 1e6

  # Check if the current megapixels are already within the desired range
  if (current_megapixels >= min_megapixels && current_megapixels <= max_megapixels) {
    return(img)
  }

  # Calculate the scaling factor needed to get within the desired megapixel range
  scaling_factor <- sqrt(min_megapixels / current_megapixels)
  scaled_width <- round(current_width * scaling_factor)
  scaled_height <- round(current_height * scaling_factor)

  # Ensure the scaled image is not too large
  if (scaled_width * scaled_height / 1e6 > max_megapixels) {
    scaling_factor <- sqrt(max_megapixels / current_megapixels)
    scaled_width <- round(current_width * scaling_factor)
    scaled_height <- round(current_height * scaling_factor)
  }

  # Resize the image
  resized_img <- magick::image_resize(img, magick::geometry_size_pixels(scaled_width, scaled_height, preserve_aspect = TRUE))

  return(resized_img)
}
