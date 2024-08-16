#' Search for specimen metadata
#'
#' This function searches GBIF for records with images of preserved specimens for a given species.
#'
#' @param taxon_name A character string of the scientific name of the taxon to search for.
#' @coordinate a coordinate point to be passed as latitude and longitude as e.g. c(42, -85) to be the centroid of the
#' polygon where the search is going to be performed
#' @buffer_distance A number in degrees for the size of the square around the centroid passed in the argument 
#' coordinate. If no argument is passed, it will set to 1.
#' @param ... Additional arguments passed to occ_search function.
#'
#' @return A data frame containing metadata for the found specimens, including media URLs and licenses.
#'
#' @importFrom rgbif occ_search
#' @export
search_specimen_metadata <- function(taxon_name=NULL, 
                                     coordinate=NULL, 
                                     buffer_distance=NULL, 
                                     limit=500, ...) {
  if(!is.null(coordinate)) {
    if(is.null(buffer_distance)){
      buffer_distance=1
    } 
    # coordinates should be passed as e.g. coordinate=c(40, -120)
    lat <- coordinate[1]
    lon <- coordinate[2]
    coordinate_plus_buffer <- coordinates_to_wkt_square_polygon(lat,lon,buffer_distance)
    kingdomKey=c(6)
  } else {
    kingdomKey=NULL
  }
  #--------------------------------------
  # Search GBIF for records with images
  all_gbif_data <- occ_search(scientificName = taxon_name , mediaType = "StillImage", basisOfRecord="PRESERVED_SPECIMEN",geometry=coordinate_plus_buffer,limit=limit,kingdomKey=kingdomKey, ...)
  #--------------------------------------
  # Extract URL and licence type
  metadata <- as.data.frame(all_gbif_data$data)

  metadata_final <- matrix(nrow=0, ncol=ncol(metadata)+2)
  for(obs_index in 1:nrow(metadata)) {
    media_info <- all_gbif_data$media[[obs_index]][[1]]
    media_info <- unlist(media_info)
    names_media_info <- names(media_info)
    if("identifier" %in% names_media_info) {
      potential_url <- media_info[which(names(media_info) %in% "identifier")]
      potential_url <- subset(potential_url, !grepl("manifest",potential_url)) # getting rid of the "manifest" urls on the identifier slots
      for(i in 1:length(potential_url)) {
        if(grepl("gbif$", potential_url[i])) {
          potential_url[i] <- gsub("gbif$","",potential_url[i])
        }
        media_license_and_url <- cbind(media_info[which(names(media_info) %in% "license")][i], unname(potential_url)[i])
        metadata_final <- rbind(metadata_final, cbind(metadata[obs_index, ], media_license_and_url))
      }
    } 
  }
  metadata_final <- as.data.frame(metadata_final)
  colnames(metadata_final) <- c(colnames(metadata), "license","media_url")
  metadata_final <- subset(metadata_final, !grepl("inaturalist",metadata_final$media_url)) # removing inaturalist images
  metadata_final <- metadata_final[!is.na(metadata_final$media_url),]
  cat(nrow(metadata_final), "records of", taxon_name, "found with media data.\n")
  return(metadata_final)
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
  dir_name="my_virtual_collection",
  resize=NULL,
  sleep=2,
  result_file_name="download_results",
  timeout_limit=300) {
  create_directory(dir_name)

  # Initialize the 'status' and 'error_message' columns
  metadata$filesize <- NA
  metadata$status <- NA
  metadata$error_message <- NA
  options(timeout = max(timeout_limit, getOption("timeout")))

  for(specimen_index in 1:nrow(metadata)) {
    species_name <- metadata$species[specimen_index]
    gbif_key <- metadata$key[specimen_index]
    media <- metadata$media_url[specimen_index]
    if(is.na(species_name)) {
      species_name <- "indet"
    }
    file_name <- paste0(dir_name,"/",paste0(gsub(" ","_",species_name),"_", gbif_key,".jpeg"))
    # Attempt to download the file
    download <- try(download_file_safe(media, file_name), silent = TRUE)
    Sys.sleep(sleep)
    if(!inherits(download, "try-error")) {  # Check if download succeeded
      metadata$status[specimen_index] <- "succeeded"
      metadata$filesize[specimen_index] <- as.data.frame(magick::image_info(magick::image_read(file_name)))[,"filesize"]
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
    metadata_subset <- metadata[, c("scientificName", "gbifID", "institutionCode", "decimalLatitude", "decimalLongitude", "eventDate", "country", "license","rightsHolder","filesize","status", "error_message")]
    
    # Save the output
    write.csv(metadata_subset, file=paste0(result_file_name, ".csv"), row.names=FALSE)
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
