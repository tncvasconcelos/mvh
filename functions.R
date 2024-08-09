# Arguments:
# Function to search metadata
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
  metadata <- subset(metadata, metadata$basisOfRecord=="PRESERVED_SPECIMEN")
  metadata <- subset(metadata, !grepl("inaturalist",metadata$media_url))
  return(metadata)
}

# Function to download specimens
download.specimen.image <- function(metadata, resize=c(""), dir_name="my_virtual_collection") {
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
    }  
  }
}

# Function to download images
download.file.int <- function(url, destfile) {
  tryCatch({
    download.file(url, destfile, mode = "wb")
  }, error = function(e) {
    message("Error downloading ", url)
  })
}

# Define your function to create a directory
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


# resizing image
resize.image <- function(file_name, min_megapixels = 20, max_megapixels = 25) {
  # Load the image
  img <- image_read(file_name)
  
  # Get the current dimensions of the image
  current_width <- image_info(img)$width
  current_height <- image_info(img)$height
  
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
  resized_img <- image_resize(img, geometry_size_pixels(scaled_width, scaled_height, preserve_aspect = TRUE))
  
  return(resized_img)
}