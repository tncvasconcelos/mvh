#' Search for Specimen Metadata
#'
#' This function searches GBIF for records with images of preserved specimens for a given species.
#'
#' @param taxon_name A character string representing the scientific name of the taxon to search for.
#' @param coordinates A numeric vector representing a coordinate point, passed as latitude and longitude (e.g., `c(42, -85)`), to be the centroid of the polygon where the search will be performed.
#' @param buffer_distance A numeric value representing the size of the square in degrees around the centroid passed in the `coordinates` argument. If not provided, it defaults to 1.
#' @param limit Numeric. The maximum number of records to search for on GBIF. Defaults to 500.
#' @param ... Additional arguments passed to the `occ_search` function.
#'
#' @return A data frame containing metadata for the found specimens, including media URLs and licenses.
#'
#' @importFrom rgbif occ_search
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- search_specimen_metadata(taxon_name = "Puma concolor", coordinates = c(42, -85))
#' }
search_specimen_metadata <- function(taxon_name=NULL,
                                     coordinates=NULL,
                                     buffer_distance=NULL,
                                     limit=500, ...) {
  if(!is.null(coordinates)) {
    if(is.null(buffer_distance)){
      buffer_distance=1
    }
    # coordinates should be passed as e.g. coordinates=c(40, -120)
    lat <- coordinates[1]
    lon <- coordinates[2]
    coordinate_plus_buffer <- coordinates_to_wkt_square_polygon(lat,lon,buffer_distance)
    kingdomKey <- c(6)
  } else {
    kingdomKey <- NULL
    coordinate_plus_buffer <- NULL
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
    } # else {
    #  print(media_info[which(names_media_info=="references")])
    #}
  }

  metadata_final <- as.data.frame(metadata_final)
  colnames(metadata_final) <- c(colnames(metadata), "license","media_url")
  #which(!metadata$gbifID %in% metadata_final$gbifID)
  metadata_final <- subset(metadata_final, !grepl("inaturalist",metadata_final$media_url)) # removing inaturalist images
  metadata_final <- metadata_final[!is.na(metadata_final$media_url),]
  cat(nrow(metadata_final), "records of", taxon_name, "found with media data.\n")
  return(metadata_final)
}

#' Download Specimen Images
#'
#' This function downloads specimen images based on the provided metadata and optionally resizes them.
#'
#' @param metadata A data frame containing specimen metadata, as returned by `search.specimen.metadata()`.
#' @param resize Numeric. Quality percentage to resize the image, ranging from 0 to 100 (higher values mean better quality).
#' @param dir_name A character string specifying the directory to save the downloaded images.
#' @param sleep Numeric. Number of seconds to wait between downloads.
#' @param result_file_name A character string specifying the name of the output CSV file.
#' @param timeout_limit Numeric. The timeout limit (in seconds) for downloading each image.
#'
#' @return A data frame containing the metadata of the downloaded images, including their download status and file size.
#' @importFrom utils download.file write.csv
#' @importFrom magick image_info image_read image_write
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- search_specimen_metadata(taxon_name = "Myrcia splendens")
#' download_specimen_images(metadata, dir_name = "my_virtual_collection", resize = 75)
#' }
download_specimen_images <- function(metadata,
  dir_name="my_virtual_collection",
  resize=75,
  sleep=2,
  result_file_name="download_results",
  timeout_limit=300) {

  create_directory(dir_name)

  if(nrow(metadata)==0) {
    stop("No records to download in metadata.")
  }

  # Initialize the 'status' and 'error_message' columns
  metadata$filesize <- NA
  metadata$status <- NA
  metadata$error_message <- NA
  if(is.null(metadata$rightsHolder)){
    metadata$rightsHolder <- NA
  }
  if(is.null(metadata$scientificName)){
    metadata$scientificName <- NA
  }
  if(is.null(metadata$gbifID)){
    metadata$gbifID <- NA
  }
  if(is.null(metadata$institutionCode)){
    metadata$institutionCode <- NA
  }
  if(is.null(metadata$eventDate)){
    metadata$eventDate <- NA
  }
  if(is.null(metadata$country)){
    metadata$country <- NA
  }

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
        try_img <- try(magick::image_read(file_name), silent = TRUE)
        #try_img <- try(resize.image(file_name, min_megapixels=resize[1], max_megapixels=resize[2]), silent = TRUE)
        if(!inherits(try_img, "try-error")) {  # Check if resizing succeeded
          image_write(try_img, file_name, quality=resize)
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
    metadata_subset <- metadata[, c("scientificName", "gbifID", "institutionCode", "eventDate", "country", "license","rightsHolder","filesize","status", "error_message")]

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
