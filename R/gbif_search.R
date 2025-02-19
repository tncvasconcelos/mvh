#' Search for Specimen Metadata
#'
#' This function searches GBIF for records with images of preserved specimens for a given species.
#'
#' @param taxon_name A character string representing the scientific name of the taxon to search for.
#' @param coordinates A numeric vector representing a coordinate point, passed as latitude and longitude (e.g., `c(42, -85)`), to be the centroid of the polygon where the search will be performed.
#' @param buffer_distance Numeric. How far from the coordinates to search for specimens.
#' @param limit Numeric. The maximum number of records to search for on GBIF. Defaults to 500.
#' @param verbose Should the function print the number of records found with media data on console? Default is TRUE.
#' @param search_type The default is "herbarium" for herbarium specimens. The other option is "cs" for citizen science images (mainly living plants from imported from iNaturalist).
#' @param user GBIF username. If provided, the function will send a formal download request to GBIF and add a column in the output with the proper DOI citation (RECOMMENDED FOR PUBLICATIONS).
#' @param pwd GBIF password. If provided, the function will send a formal download request to GBIF and add a column in the output with the proper DOI citation (RECOMMENDED FOR PUBLICATIONS).
#' @param email GBIF email. If provided, the function will send a formal download request to GBIF and add a column in the output with the proper DOI citation (RECOMMENDED FOR PUBLICATIONS).
#' @param ... Additional arguments passed to the `occ_search` function.
#'
#' @return A data.frame containing metadata for the found specimens, including media URLs and licenses.
#'
#' @importFrom rgbif occ_search occ_download pred_in
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' \donttest{
#' metadata <- search_specimen_metadata(taxon_name = "Vaccinium", coordinates = c(42, -85))
#' }
search_specimen_metadata <- function(taxon_name=NULL,
                                     coordinates=NULL,
                                     buffer_distance=NULL,
                                     limit=500,
                                     verbose = TRUE,
                                     search_type="herbarium",
                                     user = NULL,
                                     pwd = NULL,
                                     email = NULL,
                                     ...) {
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
  if(search_type=="herbarium") {
    search_type <- "PRESERVED_SPECIMEN"
  } else if(search_type=="cs") {
    search_type <- "HUMAN_OBSERVATION"
  }

  all_gbif_data <- occ_search(scientificName = taxon_name , mediaType = "StillImage", basisOfRecord=search_type,geometry=coordinate_plus_buffer,limit=limit,kingdomKey=kingdomKey, ...)

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

  # Let's add the doi column if GBIF username is provided:
  if(!is.null(user) && !is.null(pwd) && !is.null(email)) {
    doi_output <- capture.output(occ_download(pred_in("GBIF_ID", all_gbif_data$data$gbifID),
                                              user = user,
                                              pwd = pwd,
                                              email = email))
    citation_doi <- gsub("  DOI: ","", doi_output[grep("  DOI: ", doi_output)])
    metadata_final <- cbind(metadata_final, citation_doi)
  }
  if(verbose) {
    message(nrow(metadata_final), " records of ", taxon_name, " found with media data.\n")
  }
  return(metadata_final)
}

#' Download Specimen Images
#'
#' This function downloads specimen images based on the provided metadata and optionally resizes them.
#'
#' @param metadata A data.frame containing specimen metadata, as returned by `search.specimen.metadata()`.
#' @param resize Numeric. Quality percentage to resize the image, ranging from 0 to 100 (higher values mean better quality).
#' @param max_megapixels Numeric. If the photo above is this value it will be reduced to the max_megapixels, otherwise it will remain the same quality.
#' @param dir_name A character string specifying the directory to save the downloaded images.
#' @param sleep Numeric. Number of seconds to wait between downloads.
#' @param result_file_name A character string specifying the name of the output CSV file.
#' @param timeout_limit Numeric. The timeout limit (in seconds) for downloading each image.
#' @param verbose Boolean. Whether messages are printed to console.
#'
#' @importFrom utils download.file write.csv
#' @importFrom magick image_info image_read image_write
#' @export
#'
#' @examples
#' \donttest{
#' metadata <- search_specimen_metadata(taxon_name = "Myrcia splendens")
#' download_specimen_images(metadata, dir_name = "my_virtual_collection", resize = 75)
#' }
download_specimen_images <- function(metadata,
  dir_name=file.path(tempdir(), "my_virtual_collection"),
  resize=NULL,
  max_megapixels=NULL,
  sleep=2,
  result_file_name=file.path(tempdir(), "download_results"),
  timeout_limit=300,
  verbose=TRUE) {

  if(!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }

  if(nrow(metadata)==0) {
    stop("No records to download in metadata.")
  }

  # Initialize the 'status' and 'error_message' columns
  metadata$original_filesize <- NA
  metadata$megapixels <- NA
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

  # Set timeout limit
  options(timeout = max(timeout_limit, getOption("timeout")))

  for(specimen_index in 1:nrow(metadata)) {
    species_name <- metadata$species[specimen_index]
    gbif_key <- metadata$key[specimen_index]
    media <- metadata$media_url[specimen_index]
    if(is.na(species_name)) {
      species_name <- "indet"
    }
    file_name <- file.path(dir_name, paste0(gsub(" ", "_", species_name), "_", gbif_key, ".jpeg"))
    # Attempt to download the file
    download_file_name <- try(download_file_safe(media, file_name), silent = TRUE)
    Sys.sleep(sleep)
    if(!inherits(download_file_name, "try-error")) {  # Check if download succeeded
      metadata$status[specimen_index] <- "succeeded"
      metadata$original_filesize[specimen_index] <- as.data.frame(magick::image_info(magick::image_read(download_file_name)))[,"filesize"]
      #------
      # Calculate the current megapixels
      img <- magick::image_read(download_file_name)
      current_width <- magick::image_info(img)$width
      current_height <- magick::image_info(img)$height
      current_megapixels <- (current_width * current_height) / 1e6
      #------
      metadata$megapixels[specimen_index] <- current_megapixels
      # Attempt to resize the image if required
      if(!is.null(resize)) {
        try_img <- try(magick::image_read(download_file_name), silent = TRUE)
        if(!inherits(try_img, "try-error")) {  # Check if resizing succeeded
          magick::image_write(try_img, download_file_name, quality=resize)
          if(verbose) {
            message("resized","\n")
          }
        } else {
          metadata$status[specimen_index] <- "failed"
          metadata$error_message[specimen_index] <- try_img[1]
        }
      }
      if(!is.null(max_megapixels)) {
        megapixel_perc <- round(sqrt(max_megapixels/current_megapixels) * 100)-1
        try_img <- try(magick::image_scale(img, paste0(megapixel_perc, "%")), silent = TRUE)
        if(!inherits(try_img, "try-error")) {  # Check if resizing succeeded
          magick::image_write(try_img, download_file_name)
          current_width <- magick::image_info(try_img)$width
          current_height <- magick::image_info(try_img)$height
          current_megapixels <- (current_width * current_height) / 1e6
          metadata$megapixels[specimen_index] <- round(current_megapixels,4)

          if(verbose) {
            message("image is now under indicated max megapixels","\n")
          }
        } else {
          metadata$status[specimen_index] <- "failed"
          metadata$error_message[specimen_index] <- try_img[1]
        }
      }
    } else {
      metadata$status[specimen_index] <- "failed"
      metadata$error_message[specimen_index] <- download_file_name[1]
    }
    # Subset metadata to include only the selected columns
    metadata_subset <- metadata[, c("scientificName", "gbifID", "institutionCode", "eventDate", "country", "license","rightsHolder","original_filesize","megapixels","status", "error_message")]

    # Save the output
    write.csv(metadata_subset, file=paste0(result_file_name, ".csv"), row.names=FALSE)
  }
  source_herbaria <- metadata$institutionCode[!is.na(metadata$institutionCode)]
  if(length(source_herbaria)>0) {
    message("Download completed! Don't forget to acknowledge the collections: ",
        paste(unique(source_herbaria), collapse=", "),"; if you use the specimens in your research.")
  }
  #return(metadata_subset)
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
