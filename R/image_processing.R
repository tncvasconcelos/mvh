#' Resize an image
#'
#' This function resizes an image to fall within a specified megapixel range.
#'
#' @param file_name A character string of the file path of the image to resize.
#' @param max_megapixels A numeric value of the maximum desired megapixels.
#'
#' @return A magick image object of the resized image.
#'
#' @importFrom magick image_read image_info image_resize image_write geometry_size_pixels
#' @export
resize.image <- function(file_name, current_megapixels, max_megapixels) {
  # Load the image
  img <- magick::image_read(file_name)
  # Check if the current megapixels are already within the desired range
  if (current_megapixels <= max_megapixels) {
    return(img)
  }

  # Calculate the scaling factor to resize within the desired megapixel range
  scaling_factor <- sqrt(max_megapixels / current_megapixels)
  scaled_width <- round(current_width * scaling_factor)
  scaled_height <- round(current_height * scaling_factor)

  # Resize the image with new dimensions, preserving the aspect ratio
  resized_img <- magick::image_resize(img, magick::geometry_size_pixels(scaled_width, scaled_height, preserve_aspect = TRUE))

  return(resized_img)
}
