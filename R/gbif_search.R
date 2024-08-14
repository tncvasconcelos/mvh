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
search_specimen_metadata <- function(species_name, ...) {
  #--------------------------------------
  # Search GBIF for records with images
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
  metadata <- metadata[!is.na(metadata$media_url),]
  cat(nrow(metadata), "records of", species_name, "found with media data.\n")
  return(metadata)
}

#' Download specimen images
#'
#' This function downloads specimen images based on the provided metadata and optionally resizes them.
#'
#' @param metadata A data frame containing specimen metadata, as returned by search.specimen.metadata().
#' @param resize A character vector specifying resize options (currently not used in the function).
#' @param dir_name A character string specifying the directory to save the downloaded images.
#' @param sleep Numeric. Number of seconds to wait between downloads.
#' @importFrom utils download.file
#'
#' @export
download_specimen_images <- function(metadata,
  dir_name="my_virtual_collection2",
  resize=NULL,
  sleep=2) {
  create_directory(dir_name)

  # Initialize the 'status' and 'error_message' columns
  metadata$status <- NA
  metadata$error_message <- NA

  for(specimen_index in 1:nrow(metadata)) {
    species_name <- metadata$species[specimen_index]
    gbif_key <- metadata$key[specimen_index]
    media <- metadata$media_url[specimen_index]
    file_name <- paste0(dir_name,"/",paste0(gsub(" ","_",species_name),"_", gbif_key,".jpeg"))
    # Attempt to download the file
    download <- try(download.file(media, file_name), silent = TRUE)
    Sys.sleep(sleep)
    if(!inherits(download, "try-error")) {  # Check if download succeeded
      metadata$status[specimen_index] <- "succeeded"

      # Attempt to resize the image if required
      if(!is.null(resize)) {
        try_img <- try(resize.image(file_name, min_megapixels=resize[1], max_megapixels=resize[2]), silent = TRUE)
        if(!inherits(try_img, "try-error")) {  # Check if resizing succeeded
          image_write(try_img, file_name)
          cat("resized","\n")
        } else {
          metadata$status[specimen_index] <- "failed"
          metadata$error_message[specimen_index] <- try_img[1]
        }
      }
    } else {
      metadata$status[specimen_index] <- "failed"
      metadata$error_message[specimen_index] <- download[1]
    }
    # Subset metadata to include only the selected columns
    metadata_subset <- metadata[, c("scientificName", "gbifID", "decimalLatitude", "decimalLongitude", "eventDate", "country", "status", "error_message")]
    # Save the output
    write.csv(metadata_subset, file="download_results.csv", row.names=FALSE)
  }
  return(metadata_subset)
}

# download_specimen_images <- function(metadata, dir_name="my_virtual_collection2", resize=NULL, sleep=2) {
#   create_directory(dir_name)
#   failed <- matrix(nrow=0, ncol=3)
#   for(specimen_index in 1:nrow(metadata)) {
#     species_name <- metadata$species[specimen_index]
#     gbif_key <- metadata$key[specimen_index]
#     media <- metadata$media_url[specimen_index]
#     file_name <- paste0(dir_name,"/",paste0(gsub(" ","_",species_name),"_", gbif_key,".jpeg"))
#     Sys.sleep(sleep)
#     error_message <- NULL
#     #try(download.file.int(media, file_name))
#     download <- try(download.file(media, file_name))
#     if(!class(download) == "try-error"){
#       if(!is.null(resize)) {
#         try(try_img <- resize.image(file_name, min_megapixels=resize[1], max_megapixels=resize[2]))
#         if(exists("try_img")) {
#           image_write(try_img, file_name)
#           cat("resized","\n")
#           remove("try_img")
#         }
#       }
#     } else {
#       error_message <- download[1]
#       failed <- rbind(failed, c(species_name, gbif_key, error_message))
#       write.csv(failed, file="download_failed.csv", row.names=F)
#     }
#   }
# }
