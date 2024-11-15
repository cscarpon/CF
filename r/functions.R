#Extract information from a file path to determine what shapefiles you have
extract_info <- function(file_path) {
  # Normalize the file path
  path_normal <- normalizePath(file_path, mustWork = FALSE)
  
  # Check if the path exists
  if (!file.exists(path_normal)) {
    stop("Path does not exist: ", path_normal)
  }
  
  # Define the pattern for file extensions
  exts <- "\\.(laz|las|xyz|csv|shp|tif|tiff|json|rds)$"
  
  # List files matching the pattern
  data_list <- list.files(path_normal, pattern = exts, full.names = TRUE)
  
  # Initialize an empty data frame
  meta_df <- data.frame(
    id = numeric(),
    file_path = character(),
    file_name = character(),
    size_mb = numeric(),
    ext = character(),
    creation_date = as.POSIXct(character())
  )
  
  # Loop over the files and extract information
  for (i in seq_along(data_list)) {
    
    object_id <- i
    
    # Extract file path
    current_file_path <- data_list[i]
    
    current_file_path <- normalizePath(current_file_path, mustWork = FALSE)
    
    # Create file name
    
    base_name <- basename(current_file_path)
    
    
    # Extract file extension
    ext <- tools::file_ext(current_file_path)
    
    # Get file size
    size <- format(round(file.info(current_file_path)$size / (1024^2),2), nsmall = 2)
    
    # Get last modification date
    date <- file.info(current_file_path)$mtime
    formatted_date <- format(date, "%Y-%m-%d")
    
    # Append the information to the data frame
    meta_df <- rbind(meta_df, data.frame(id = object_id,
                                         file_path = current_file_path,
                                         file_name = base_name,
                                         size_mb = size,
                                         ext = ext,
                                         creation_date = formatted_date,
                                         stringsAsFactors = FALSE))
  }
  meta_df <- meta_df %>%
    dplyr::arrange(ext) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(id, dplyr::everything())
  
  return(meta_df)
}

# Function to for preprocessing of a raster. It aligns the source to the target, resamples and then masks the output so they have the same extent as the target
process_raster <- function(source, target, source_mask, target_mask,  method = "bilinear") {

  # Assuming you have predefined masks for source and target
  
  # source_mask <- transform_polygon_crs(source_mask, target_mask, crs)

  union_sf <- sf::st_union(source_mask, target_mask)
  union_vect <- terra::vect(union_sf)

  # Crop the source to match the target raster's extent.

  # source <- transform_raster_crs(source, target, crs)
  source <- terra::crop(source, terra::ext(union_vect))
  target <- terra::crop(target, terra::ext(union_vect))

  source <- terra::resample(source, target, method = method)
  
  # Apply masks to each of the raster layers which will be used for the difference.
  source <- terra::mask(source, union_vect)
  target <- terra::mask(target, union_vect)

  return(list(source = source, target = target, mask = union_vect))
}

# Function to generate CHM and classify the differences
diff_classify <- function(earlier, later) {
    # Compute the difference
    diff <- later - earlier
    
    # Create a raster for the magnitude of change
    # Classify the differences
    m <- c(-Inf, -10, 1,
            -10, -0.5, 2,
            -0.5, 0.5, 3,
            0.5, 10, 4,
            10, Inf, 5)
    # Create a matrix with the ranges for reclassification
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
    return(diff_class)
}

#Calculate statistics for a raster and return a data frame to plot in ggplot2

plot_stats <- function(difference_raster) {
  rast_freq <- terra::freq(difference_raster)
  rast_freq$area <- rast_freq$count * 0.5 # Square metres
  
  # Handling NaN
  rast_freq$value[is.nan(rast_freq$value)] <- "NaN"
  
  # Define class labels
  class_labels <- c("Large Loss: > 10m loss",
                    "Loss: 0.5m to 10m loss",
                    "Minimal change: -0.5m to 0.5m",
                    "Gain: 0.5m to 10m gain",
                    "Large Gain: > 10m gain",
                    "NaN")
  
  # Define class values
  class_values <- c("1", "2", "3", "4", "5", "NaN")
  
  # Convert value to a factor
  rast_freq$value <- factor(rast_freq$value, levels = class_values, labels = class_labels)
  
  # Plot the area column with different colors for each bar and a legend
  ggplot(rast_freq, aes(x = value, y = area, fill = factor(value))) +
    geom_bar(stat = "identity") +
    labs(x = "Loss and Gain", y = "Area (m^2)", fill = "Class") +
    scale_x_discrete(labels=c('Large Loss', 'Loss', 'Minimal Change', 'Growth', 'Large Growth')) +
    ggtitle("Raster Statistics for Change Detection") +
    scale_fill_manual(
      values = c("darkorange", "orange", "lightgrey", "lightgreen", "darkgreen"),
      labels = class_labels, 
      drop = FALSE,
      guide = guide_legend(ncol = 1) # Stack the legend items
    ) +
    theme_minimal(base_size = 15) +  # Set a base size for the text
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"), # Reduced legend title size
      legend.text = element_text(size = 12),  # Reduced legend text size
      legend.key.size = unit(0.8, "cm"),  # Reduced legend key size
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "right"  # Adjust the legend position if needed
    ) +
    scale_y_continuous(labels = comma)
}

# Show DF

show_df <- function(df) {
  print(df)
}

#Check to ensure SpatRaster is not empty - used for Leaflet testing
is_empty <- function(raster) {
    all(is.na(terra::values(raster)))
}

#Check to ensure sfc is not empty - used for Leaflet testing
is_empty_sfc <- function(sfc) {
    length(sfc) == 0
}

# Compare two rasters to ensure that they have the same crs.
transform_raster_crs <- function(source_raster, target_raster, crs) {
  crs_input <- paste0("EPSG:", 4326)

  # If source raster lacks a CRS, assign it the reference CRS
  if (is.na(terra::crs(source_raster))) {
    terra::crs(source_raster) <- crs_input
  }

  # Check if the CRS of both rasters are the same
  if (terra::crs(source_raster) != terra::crs(target_raster)) {
    # Transform the CRS of the source raster to match the target raster
    source_raster <- terra::project(source_raster, target_raster)
  }

  return(source_raster)
}

transform_polygon_crs <- function(source_polygon, target_polygon, crs) {
  # If source polygon lacks a CRS, assign it the reference CRS
  crs_int <- as.integer(crs)
  if (is.na(sf::st_crs(source_polygon))) {
    sf::st_crs(source_polygon) <- sf::st_crs(crs_int)
  }

  # Check if the CRS of both polygons are the same
  if (sf::st_crs(source_polygon) != sf::st_crs(target_polygon)) {
    # Transform the CRS of the source polygon to match the target polygon
    source_polygon <- sf::st_transform(source_polygon, sf::st_crs(target_polygon))
  }
  
  return(source_polygon)
}

#Mask pc is called on initially. Every point cloud object will have a mask layer.

mask_pc <- function(pc) {
    
    # Step 1:  Remove density to 1 point per square meter
    decimate <- lidR::decimate_points(pc, random(1))
    
    # Step 2: Remove duplicate (X, Y) points
    unique_points <- decimate@data %>%
      dplyr::distinct(X, Y)  # Keep only unique X, Y pairs
    
    coords_sf <- sf::st_as_sf(unique_points[, c("X", "Y")], 
                              coords = c("X", "Y"), 
                              crs = lidR::projection(pc))
    
    # Step 3: Convert the decimated points to an sf object
    coords_sf <- sf::st_as_sf(decimate@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(pc))
    
    # Step 4: Buffer the points by 3 meters
    buffered_points <- sf::st_buffer(coords_sf, dist = 3)
    
    # Step 5: Union the buffered geometries into a single geometry
    unioned_buffer <- sf::st_union(buffered_points)
    
    # Step 6: Apply a negative buffer (shrink by 3 meters)
    final_polygon <- sf::st_buffer(unioned_buffer, dist = -3)
    
    # Step 7: Ensure the polygon is valid
    final_polygon <- sf::st_make_valid(final_polygon)
    
    # Step 8: Remove holes and simplify the final polygon
    # Initialize an empty list to store the results
    final_result <- list()
    
    # Iterate over each geometry in final_polygon
    for (i in seq_along(final_polygon)) {
      # Remove holes
      no_holes <- nngeo::st_remove_holes(final_polygon[i])
      # Append to the final result list
      final_result[[i]] <- no_holes
    }
    
    # Combine all the results into a single sf object
    final_sf <- do.call(sf::st_sfc, final_result)
    final_sf <- rmapshaper::ms_simplify(final_sf, keep = 0.03, weighting = 2.0, keep_shapes = TRUE)
    final_sf <- sf::st_as_sf(final_sf)
    final_sf <- sf::st_transform(final_sf, crs = sf::st_crs(pc))
    
    return(final_sf)
}

displayMap <- function(dtm, chm, chm_diff, mask) {
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    leaflet::addMeasure(
      position = "topleft", 
      primaryLengthUnit = "meters", 
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479")
    # Add north arrow as an image
    # leafem::addLogo(file.path("./www/northNA.png"), src = "local", position = "topleft", width = 50, height = 50)
  # Transform mask if it's not NULL
  if (!is.null(mask)) {
    mask <- terra::project(mask, "EPSG:4326")
  }
  
  # Mask and project DTM if it's not NULL and mask is not NULL
  if (!is.null(dtm) && !is.null(mask)) {
    dtm_m <- terra::project(dtm, "EPSG:4326")
    dtm_m <- terra::mask(dtm_m, mask)
  } else if (!is.null(dtm)) {
    dtm_m <- terra::project(dtm, "EPSG:4326")
  } else {
    dtm_m <- NULL
  }
  
  # Mask and project CHM if it's not NULL and mask is not NULL
  if (!is.null(chm) && !is.null(mask)) {
    chm_m <- terra::project(chm, "EPSG:4326")
    chm_m <- terra::mask(chm_m, mask)
  } else if (!is.null(chm)) {
    chm_m <- terra::project(chm, "EPSG:4326")
  } else {
    chm_m <- NULL
  }
  
  # Project chm_diff if it's not NULL
  if (!is.null(chm_diff)) {
    diff <- terra::project(chm_diff, "EPSG:4326")
    diff <- terra::clamp(diff, 1, 5)
    diff_round <- round(diff)
  } else {
    diff_round <- NULL
  }
  
  # Add DTM raster image if it's not NULL
  if (!is.null(dtm_m)) {
    pal_dtm <- colorNumeric("magma", domain = values(dtm_m), na.color = "transparent")
    m <- addRasterImage(m, dtm_m, colors = pal_dtm, group = "DTM", maxBytes = Inf, opacity = 1)
  }
  
  # Add CHM raster image if it's not NULL
  if (!is.null(chm_m)) {
    pal_chm <- colorNumeric("magma", domain = values(chm_m), na.color = "transparent")
    m <- addRasterImage(m, chm_m, colors = pal_chm, group = "CHM", maxBytes = Inf, opacity = 1)
  }
  
  # Add chm_diff raster image with legend if it's not NULL
  if (!is.null(diff_round)) {
    colors <- c("darkorange", "orange", "lightgrey", "lightgreen", "darkgreen")
    pal_diff <- colorNumeric(palette = colors, domain = values(diff_round), na.color = "transparent")
    
    m <- addRasterImage(m, diff_round, colors = pal_diff, group = "Diff", maxBytes = Inf, opacity = 1)
  }
  
  # Add mask polygons if mask is not NULL
  if (!is.null(mask)) {
    m <- addPolygons(m, data = st_as_sf(mask, crs = 4326), color = "black", fill = FALSE, group = "Mask")
  }
  
  # Add layers control with all layers turned off initially except the mask
  overlayGroups <- c()
  if (!is.null(dtm_m)) overlayGroups <- c(overlayGroups, "DTM")
  if (!is.null(chm_m)) overlayGroups <- c(overlayGroups, "CHM")
  if (!is.null(diff_round)) overlayGroups <- c(overlayGroups, "Diff")
  if (!is.null(mask)) overlayGroups <- c(overlayGroups, "Mask")
  
  m <- addLayersControl(m, overlayGroups = overlayGroups, options = layersControlOptions(collapsed = FALSE))
  
  # Hide all layers except the mask
  if (!is.null(dtm_m)) m <- hideGroup(m, "DTM")
  if (!is.null(chm_m)) m <- hideGroup(m, "CHM")
  if (!is.null(diff_round)) m <- hideGroup(m, "Diff")
  m <- showGroup(m, "Mask")
  
  # Add legends for each layer but do not show them initially
  if (!is.null(dtm_m)) {
    m <- addLegend(m, pal = pal_dtm, values = values(dtm_m), position = "bottomright", title = "Digital Terrain Model (m)", layerId = "dtmLegend", opacity = 1)
  }
  
  if (!is.null(chm_m)) {
    m <- addLegend(m, pal = pal_chm, values = values(chm_m), position = "bottomright", title = "Canopy Height Model (m)", layerId = "chmLegend", opacity = 1)
  }
  
  if (!is.null(diff_round)) {
    labels <- c("< -10", "-10 to -0.5", "-0.5 to 0.5", "0.5 to 10", "> 10")
    m <- addLegend(m, colors = colors, labels = labels, position = "bottomright", title = "Change in Tree Height (m)", layerId = "diffLegend", opacity = 1)
  }
  
  return(m)
}

displayIndex <- function(index) {
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    leaflet::addMeasure(
      position = "topleft", 
      primaryLengthUnit = "meters", 
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479")
    # leafem::addLogo(file.path("./www/northNA.png"), src = "local", position = "bottomleft", width = 50, height = 50)
    # Transform index if it's not NULL
    if (!is.null(index)) {
      index <- sf::st_transform(index, 4326)
    }
  # Add index polygons if index is not NULL
  if (!is.null(index)) {
    m <- addPolygons(m, data = index, color = "black", fill = FALSE, group = "Index")
  }
  
  # Add layers control with all layers turned off initially except the mask
  overlayGroups <- c()
  if (!is.null(index)) overlayGroups <- c(overlayGroups, "Index")
  
  m <- addLayersControl(m, overlayGroups = overlayGroups, options = layersControlOptions(collapsed = FALSE))
  
  # Hide all layers except the mask
  m <- showGroup(m, "Index")
  
  return(m)
}

add_message <- function(message, rv, session = session) {
  # Handle non-character inputs by converting them to character
  if (!is.character(message)) {
    message <- as.character(message)
  }
  
  # If the message is a vector of strings, concatenate them with HTML line breaks
  if (length(message) > 1) {
    message <- paste(message, collapse = "<br>")
  }
  
  # Add a timestamp to the message
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_message <- paste0(timestamp, ": ", message)
  
  # Update the reactive values and ensure reactivity with isolate
  shiny::isolate({
    rv$console_output <- c(rv$console_output, full_message)
  })
  
  # Use the session to flush the console
  flush.console()
}

capture_output <- function(expr) {
  temp <- tempfile()
  sink(temp)
  on.exit(sink())
  on.exit(unlink(temp), add = TRUE)
  eval(expr)
  readLines(temp)
}

# Function to update the number of items in out_dir
updateOutNum <- function(rv, session = session) {
  rv$out_num <- length(list.files(rv$out_dir))
}

create_directories <- function(data_dir, save_dir) {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    message(paste("Created directory:", data_dir))
  } else {
    message(paste("Directory already exists:", data_dir))
  }
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    message(paste("Created directory:", save_dir))
  } else {
    message(paste("Directory already exists:", save_dir))
  }
}

count_time <- function(expr) {
  start.time <- Sys.time()  # Start timer
  output <- eval.parent(substitute(expr))  # Evaluate the expression in the parent environment
  end.time <- Sys.time()  # End timer
  time.taken <- end.time - start.time  # Calculate time difference
  
  # Print the time taken with a more readable message
  print(paste0("The task took ", round(time.taken, 2), " seconds to complete."))
  
  return(output)  # Return the output from the evaluated expression
}

noise_filter_buildings <- function(laz, mask, footprint,  k_sor1 = 5, m_sor1 = 3, k_sor2 = 20, m_sor2 = 5) {
  
  start.time <- Sys.time()
  
  # Step 1: Create a unique PointID in the original LAS
  laz@data$PointID <- seq_len(nrow(laz@data))
  
  if (sf::st_crs(footprint) != sf::st_crs(laz)) {
    print("Transforming footprint to mask CRS")
    footprint <- sf::st_transform(footprint, crs = sf::st_crs(laz))
  } else {
    print("CRS match")
  }
  
  
  if (sf::st_crs(mask) != sf::st_crs(laz)) {
    print("Transforming footprint to mask CRS")
    footprint <- sf::st_transform(mask, crs = sf::st_crs(laz))
  } else {
    print("CRS match")
  }
  
  diff_mask <- sf::st_difference(mask, footprint)
  
  if (sf::st_crs(diff_mask) != sf::st_crs(laz)) {
    print("Transforming mask to las CRS")
    diff_mask <- sf::st_transform(diff_mask, crs = sf::st_crs(laz))
  } else {
    print("CRS match")
  }
  
  
  print("Clipping Buildings")
  buildings <- lidR::clip_roi(laz, footprint)
  
  print("Apply Classification")
  buildings@data$Classification <- 6
  
  # Step 3: Clip the LiDAR points that intersect with the building footprints
  las_no_buildings <- lidR::clip_roi(laz, diff_mask)
  
  print("removing noise")
  
  # Step 2: Use SOR to classify noise points
  las_fix <- lidR::classify_noise(las_no_buildings, sor(k = k_sor1, m =  m_sor1))
  las_fix1 <- lidR::filter_poi(las_fix, Classification != 18)  # Remove noise points
  rm(las_fix)  # Remove intermediate objects to free memory
  
  las_fix2 <- lidR::classify_noise(las_fix1, sor(k = k_sor2, m = m_sor2))
  las_fix2 <- lidR::filter_poi(las_fix2, Classification != 18)  # Remove noise points
  rm(las_fix1)
  
  print("Classifying ground")
  
  # Step 3: Classify ground points using the 
  las_fix2 <- lidR::classify_ground(las_fix2, algorithm = csf())
  
  ground_points <- lidR::filter_ground(las_fix2)
  
  # las_clean contains only non-ground points
  las_clean <- lidR::filter_poi(las_fix2, Classification != 2)  # Non-ground
  
  print("removing cables")
  # Step 5: Segment transmission lines (Cables)
  las_clean1 <- lidR::segment_shapes(las_clean, shp_line(k = 5, th1 = 30), "Cables")
  
  # Step 6: Join back 'Cables' classification to the original las_fix (including ground points)
  las_fix2@data <- dplyr::left_join(las_fix2@data, las_clean1@data[, c("PointID", "Cables")], by = "PointID")
  
  # Remove Transmission Lines from las_clean for the next segmentation
  las_clean2 <- lidR::filter_poi(las_clean1, Cables == "FALSE")
  
  print("removing poles")
  
  # Step 7: Segment vertical poles
  las_clean3 <- lidR::segment_shapes(las_clean2, shp_vline(th1 = 5, k = 4), "Poles")
  las_fix2@data <- dplyr::left_join(las_fix2@data, las_clean3@data[, c("PointID", "Poles")], by = "PointID")
  rm(las_clean2)
  
  las_clean3 <- lidR::filter_poi(las_clean3, Poles == "FALSE")
  
  # Step 12: Filter out all points classified as Cables, Poles, Walls, or Roofs (preserve ground points)
  
  las_clean3@data <- las_clean3@data[, -c("Cables", "Poles")]
  
  las_final <- rbind(buildings, ground_points, las_clean3)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste0("It has taken ", time.taken, " to denoise your LAS file!"))
  
  return(las_final)  # Return the point cloud with segmentation and ground classification retained
}

noise_filter <- function(laz,  k_sor1 = 5, m_sor1 = 3, k_sor2 = 20, m_sor2 = 5) {
  
  start.time <- Sys.time()
  
  # Step 1: Create a unique PointID in the original LAS
  laz@data$PointID <- seq_len(nrow(laz@data))
  
  print("removing noise")
  
  # Step 2: Use SOR to classify noise points
  las_fix <- lidR::classify_noise(laz, sor(k = k_sor1, m =  m_sor1))
  las_fix1 <- lidR::filter_poi(las_fix, Classification != 18)  # Remove noise points
  rm(las_fix)  # Remove intermediate objects to free memory
  
  las_fix2 <- lidR::classify_noise(las_fix1, sor(k = k_sor2, m = m_sor2))
  las_fix2 <- lidR::filter_poi(las_fix2, Classification != 18)  # Remove noise points
  rm(las_fix1)

  ground_points <- lidR::filter_ground(las_fix2)
  
  # las_clean contains only non-ground points
  las_clean <- lidR::filter_poi(las_fix2, Classification != 2)  # Non-ground
  
  print("removing cables")
  # Step 5: Segment transmission lines (Cables)
  las_clean1 <- lidR::segment_shapes(las_clean, shp_line(k = 5, th1 = 30), "Cables")
  
  # Step 6: Join back 'Cables' classification to the original las_fix (including ground points)
  las_fix2@data <- dplyr::left_join(las_fix2@data, las_clean1@data[, c("PointID", "Cables")], by = "PointID")
  
  # Remove Transmission Lines from las_clean for the next segmentation
  las_clean2 <- lidR::filter_poi(las_clean1, Cables == "FALSE")
  
  print("removing poles")
  
  # Step 7: Segment vertical poles
  las_clean3 <- lidR::segment_shapes(las_clean2, shp_vline(th1 = 5, k = 4), "Poles")
  las_fix2@data <- dplyr::left_join(las_fix2@data, las_clean3@data[, c("PointID", "Poles")], by = "PointID")
  rm(las_clean2)
  
  las_clean3 <- lidR::filter_poi(las_clean3, Poles == "FALSE")
  
  # Step 12: Filter out all points classified as Cables, Poles, Walls, or Roofs (preserve ground points)
  
  las_clean3@data <- las_clean3@data[, -c("Cables", "Poles")]
  
  las_final <- rbind(ground_points, las_clean3)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste0("It has taken ", time.taken, " to denoise your LAS file!"))
  
  return(las_final)  # Return the point cloud with segmentation and ground classification retained
}

diff_values <- function(raster) {
  # Get the values of the raster
  
  # Get the frequency of each class
  class_freq <- freq(raster)
  
  # Calculate the total number of cells
  total_cells <- sum(class_freq[, "count"])
  
  # Calculate percentage for each class
  class_freq$percentage <- (class_freq$count / total_cells) * 100
  
  # Set the class values as row names
  rownames(class_freq) <- class_freq$value
  
  # Remove the 'value' column since it's now the row name
  class_freq <- class_freq[, -1]
  
  return(class_percentages)
}
