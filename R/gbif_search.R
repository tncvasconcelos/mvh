#' Search for specimen metadata
#'
#' This function searches GBIF for records with images of preserved specimens for a given species.
#'
#' @param species_name A character string of the scientific name of the species to search for.
#' @param ... Additional arguments passed to occ_search function.
#'
#' @return A data frame containing metadata for the found specimens, including media URLs and licenses.
#'
#' @importFrom rgbif occ_search
#' @export
search.specimen.metadata <- function(species_name, ...) {
  #--------------------------------------
  # Search GBIF for records with images
  Sys.sleep(2)
  all_gbif_data <- occ_search(scientificName = species_name , mediaType = "StillImage", basisOfRecord="PRESERVED_SPECIMEN", ...)
  #--------------------------------------
  # Extract URL and licence type
  metadata <- as.data.frame(all_gbif_data$data)
  metadata$media_url <- NA
  metadata$license <- NA
  for(obs_index in 1:nrow(metadata)) {
    media_info <- all_gbif_data$media[[obs_index]][[1]]
    if("identifier" %in% names(media_info[[1]])) {
      metadata$media_url[obs_index] <- media_info[[1]]$identifier
      metadata$license <- media_info[[1]]$license
    }
  }
  #cat("Search for", species_name, "done!", "\n")
  metadata <- subset(metadata, !grepl("inaturalist",metadata$media_url)) # removing inaturalist images
  return(metadata)
}

#' Download specimen images
#'
#' This function downloads specimen images based on the provided metadata and optionally resizes them.
#'
#' @param metadata A data frame containing specimen metadata, as returned by search.specimen.metadata().
#' @param resize A character vector specifying resize options (currently not used in the function).
#' @param dir_name A character string specifying the directory to save the downloaded images.
#' @importFrom utils download.file
#'
#' @export
download.specimen.image <- function(metadata, dir_name="my_virtual_collection", resize=NULL) {
  create_directory(dir_name)
  for(specimen_index in 1:nrow(metadata)) {
    species_name <- metadata$species[specimen_index]
    gbif_key <- metadata$key[specimen_index]
    media <- metadata$media_url[specimen_index]
    file_name <- paste0(dir_name,"/",paste0(gsub(" ","_",species_name),"_", gbif_key,".jpeg"))
    Sys.sleep(2)
    try(download.file.int(media, file_name))
    try(try_img <- resize.image(file_name))
    if(exists("try_img")) {
      image_write(try_img, file_name)
      cat("resized","\n")
      remove("try_img")
      if(!is.null(resize)) {
        try(try_img <- resize.image(file_name, min_megapixels=resize[1], max_megapixels=resize[2]))
        if(exists("try_img")) {
          image_write(try_img, file_name)
          cat("resized","\n")
          remove("try_img")
        }
      }
    }
  }
}
