#' Resize an image
#'
#' This function resizes an image to fall within a specified megapixel range.
#'
#' @param file_name A character string of the file path of the image to resize.
#' @param current_megapixels A numeric value of the current megapixels.
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
  info <- image_info(img)
  original_width <- info$width
  original_height <- info$height
  aspect_ratio <- original_width / original_height
  max_pixels <- max_megapixels * 1e6
  new_width <- sqrt(max_pixels / aspect_ratio)
  new_height <- new_width / aspect_ratio

  # Resize the image with new dimensions, preserving the aspect ratio
  resized_img <- magick::image_resize(img, geometry = paste0(round(new_width), "x", round(new_height)))

  return(resized_img)
}
