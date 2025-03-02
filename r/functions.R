## Web mapping 

# Function to add raster layer to shiny leaflet interface
addRasterLayer <- function(map, raster, name, color_palette) {
  if (!is.null(raster)) {
    pal <- colorNumeric(color_palette, domain = values(raster), na.color = "transparent")
    map <- addRasterImage(map, raster, colors = pal, group = name, maxBytes = Inf, opacity = 1)
  }
  return(map)
}

# Function to add legend
addLegendLayer <- function(map, raster, title, layerId, color_palette) {
  if (!is.null(raster)) {
    pal <- colorNumeric(color_palette, domain = values(raster), na.color = "transparent")
    map <- addLegend(map, pal = pal, values = values(raster), position = "bottomright", title = title, layerId = layerId, opacity = 1)
  }
  return(map)
}

# Function to display the map with the full set of rasters
displayMap <- function(dtm1, ndsm1, dtm2, ndsm2, ndsm_diff, area_mask) {
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
  
    # Transform mask if it's not NULL
    if (!is.null(area_mask)) {
      area_mask <- terra::project(area_mask, "EPSG:4326")
    }
    
    # Process all rasters
    dtm1_m <- processRaster(dtm1, area_mask)
    ndsm1_m <- processRaster(ndsm1, area_mask)
    dtm2_m <- processRaster(dtm2, area_mask)
    ndsm2_m <- processRaster(ndsm2, area_mask)
    
    # Project ndsm_diff if it's not NULL
    if (!is.null(ndsm_diff)) {
      diff <- terra::project(ndsm_diff, "EPSG:4326")
      diff <- terra::clamp(diff, 1, 5)
      diff_round <- round(diff)
    } else {
      diff_round <- NULL
    }
    
    # Add DTM and nDSM layers
    m <- addRasterLayer(m, dtm1_m, "DTM (Source)", "magma")
    m <- addRasterLayer(m, dtm2_m, "DTM (Target)", "magma")
    m <- addRasterLayer(m, ndsm1_m, "nDSM (Source)", "magma")
    m <- addRasterLayer(m, ndsm2_m, "nDSM (Target)", "magma")
    
    # Add ndsm_diff raster image with legend if it's not NULL
    if (!is.null(diff_round)) {
      colors <- c("#555599", "#b2abd2",   "#f7f7f7", "#9AE696","#448F3F")      # Updated colors
      pal_diff <- colorNumeric(palette = colors, domain = values(diff_round), na.color = "transparent")
    
      m <- addRasterImage(m, diff_round, colors = pal_diff, group = "Diff", maxBytes = Inf, opacity = 1)
    }
    
    # Add mask polygons if area_mask is not NULL
    if (!is.null(area_mask)) {
      m <- addPolygons(m, data = st_as_sf(area_mask, crs = 4326), color = "black", fill = FALSE, group = "Mask")
    }
    
    # Add layers control with all layers turned off initially except the mask
    overlayGroups <- c()
    if (!is.null(dtm1_m)) overlayGroups <- c(overlayGroups, "DTM (Source)")
    if (!is.null(dtm2_m)) overlayGroups <- c(overlayGroups, "DTM (Target)")
    if (!is.null(ndsm1_m)) overlayGroups <- c(overlayGroups, "nDSM (Source)")
    if (!is.null(ndsm2_m)) overlayGroups <- c(overlayGroups, "nDSM (Target)")
    if (!is.null(diff_round)) overlayGroups <- c(overlayGroups, "Diff")
    if (!is.null(area_mask)) overlayGroups <- c(overlayGroups, "Mask")
    
    m <- addLayersControl(m, overlayGroups = overlayGroups, options = layersControlOptions(collapsed = FALSE))
    
    # Hide all layers except the mask
    for (group in overlayGroups) {
      if (group != "Mask") {
        m <- hideGroup(m, group)
      }
    }
    m <- showGroup(m, "Mask")
    
    # Add legends for each layer but do not show them initially
    m <- addLegendLayer(m, dtm1_m, "DTM (Source)", "dtm1Legend", "magma")
    m <- addLegendLayer(m, dtm2_m, "DTM (Target)", "dtm2Legend", "magma")
    m <- addLegendLayer(m, ndsm1_m, "nDSM (Source)", "ndsm1Legend", "magma")
    m <- addLegendLayer(m, ndsm2_m, "nDSM (Target)", "ndsm2Legend", "magma")
    
    if (!is.null(diff_round)) {
      labels <- c("< -10", "-10 to -0.5", "-0.5 to 0.5", "0.5 to 10", "> 10")
      m <- addLegend(m, colors = colors, labels = labels, position = "bottomright", title = "Change in Normalized Height Model (m)", layerId = "diffLegend", opacity = 1)
    }
    
    return(m)
}

# initial map to display index polygons for the loaded spatial containers
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

## General CF Functionality

#Extract information from a file path to determine what shapefiles you have in the folder, used in the metadata extraction
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
  
  # Update the reactive values and **prepend** the new message to the top
  shiny::isolate({
    rv$console_output <- c(full_message, rv$console_output)  # Prepend instead of append
  })
  
  # Use the session to flush the console
  flush.console()
}

create_directories <- function(in_dir) {
  if (!dir.exists(in_dir)) {
    dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
    message(paste("Created directory:", in_dir))
  } else {
    message(paste("Directory already exists:", in_dir))
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

# Helper function to delete all files in a directory
delete_all_files <- function(dir) {
  if (dir.exists(dir)) {
    files <- list.files(dir, full.names = TRUE)
    lapply(files, function(file) {
      if (file.exists(file)) {
        file.remove(file)
      }
    })
    print(paste("Deleted all files in directory:", dir))
  } else {
    print(paste("Directory does not exist:", dir))
  }
} 

## Point Cloud Processing Functions

# Function to 2D polygon mask around the ground points in a point cloud
mask_pc <- function(pc) {
  
  # Step 1: reduce the number of points in the point cloud
  decimate <- lidR::decimate_points(pc, random(1))
  
  # Step 2: Keep only unique points
  unique_points <- decimate@data %>%
    dplyr::distinct(X, Y)  # Keep only unique X, Y pairs
  
  
  # Step 3: Convert the decimated points to an sf object
  coords_sf <- sf::st_as_sf(decimate@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(pc))
  
  coords_vect <- terra::vect(coords_sf)
  
  raster_template <- terra::rast(terra::ext(coords_vect), resolution = 3)
  
  rasterized <- terra::rasterize(coords_vect, raster_template, field = NULL, fun = "count")
  
  polygonized <- terra::as.polygons(rasterized, dissolve = TRUE)
  
  simplified_polygon <- terra::aggregate(polygonized)
  
  no_holes <- nngeo::st_remove_holes(sf::st_as_sf(simplified_polygon))
  
  final_sf <- rmapshaper::ms_simplify(no_holes, keep = 0.05, weighting = 0.9, keep_shapes = TRUE)
  final_sf <- sf::st_as_sf(final_sf)
  sf::st_crs(final_sf) <- sf::st_crs(pc)
  final_sf <- sf::st_transform(final_sf, crs = sf::st_crs(pc))
  
  return(final_sf)
}

# Function to filter out buildings and noise from a LiDAR point clouds with and iterative SOR Filter.
noise_filter_buildings <- function(laz, area_mask, footprint,  k_sor1 = 5, m_sor1 = 3, k_sor2 = 20, m_sor2 = 5) {

  start.time <- Sys.time()
  
  # Step 1: Create a unique PointID in the original LAS
  laz@data$PointID <- seq_len(nrow(laz@data))
  
  if (sf::st_crs(area_mask) != sf::st_crs(laz)) {
    print("Transforming footprint to mask CRS")
    area_mask <- sf::st_transform(area_mask, crs = sf::st_crs(laz))
  } else {
    print("CRS match")
  }
  
  if (sf::st_crs(footprint) != sf::st_crs(laz)) {
    print("Transforming footprint to mask CRS")
    footprint <- sf::st_transform(footprint, crs = sf::st_crs(laz))
  } else {
    print("CRS match")
  }
  

  diff_mask <- sf::st_difference(area_mask, footprint)
  
  if (sf::st_crs(diff_mask) != sf::st_crs(laz)) {
    print("Transforming mask to las CRS")
    diff_mask <- sf::st_transform(diff_mask, crs = sf::st_crs(laz))
  } else {
    print("CRS match")
  }
  
  print("Clipping Buildings")

  las_no_buildings <- lidR::clip_roi(laz, diff_mask)
  
  print("removing noise")
  
  # Step 2: Use SOR to classify noise points
  las_fix <- lidR::classify_noise(las_no_buildings, sor(k = k_sor1, m =  m_sor1))
  las_fix1 <- lidR::filter_poi(las_fix, Classification != 18)  # Remove noise points
  rm(las_fix)  # Remove intermediate objects to free memory
  
  las_fix2 <- lidR::classify_noise(las_fix1, sor(k = k_sor2, m = m_sor2))
  las_fix2 <- lidR::filter_poi(las_fix2, Classification != 18)  # Remove noise points
  rm(las_fix1)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste0("It has taken ", time.taken, " to denoise your LAS file!"))
  return(las_fix2)  # Return the point cloud with segmentation and ground classification retained

}

# Function to filter out noise from a LiDAR point clouds with and iterative SOR Filter.
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
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste0("It has taken ", time.taken, " to denoise your LAS file!"))
  
  return(las_fix2)  # Return the point cloud with segmentation and ground classification retained
}

## Morpho Functionality to conduct an icp alignment in R

# Function to capture the metadata from the original las file and to join the data to the aligned las file by index id
pc_metadata <- function(original_las, aligned_las, crs) {
  # Ensure both LAS objects are valid
  if (is.null(original_las) || is.null(aligned_las)) {
    stop("Original or aligned LAS object is NULL.")
  }
  
  # Extract LAS data
  original_data <- original_las@data
  aligned_data <- aligned_las@data
  
  # Ensure row count consistency to prevent mismatches
  min_len <- min(nrow(original_data), nrow(aligned_data))
  original_data <- original_data[1:min_len, ]
  aligned_data <- aligned_data[1:min_len, ]
  
  # Replace XYZ coordinates while retaining original metadata
  original_data$X <- aligned_data$X
  original_data$Y <- aligned_data$Y
  original_data$Z <- aligned_data$Z
  
  # Update LAS object with new coordinates
  original_las@data <- original_data
  
  # Restore original CRS
  sf::st_crs(original_las) <- crs
  
  return(original_las)
}

# Function to align two LiDAR point clouds using the Iterative Closest Point (ICP) algorithm in morpho with voxelization
align_las_icp_voxelized <- function(source_las, target_las, output_las_path, voxel_size = 0.2, max_iter = 50) {
  
  # Assign unique Point IDs before voxelization
  source_las@data$PointID <- seq_len(nrow(source_las@data))
  target_las@data$PointID <- seq_len(nrow(target_las@data))
  
  # Store original metadata
  source_metadata <- source_las@data
  target_metadata <- target_las@data
  
  # Voxelize both point clouds
  las_source_voxel <- lidR::voxelize_points(source_las, voxel_size)
  las_target_voxel <- lidR::voxelize_points(target_las, voxel_size)
  
  # Map metadata using original point 
  voxel_metadata <- target_metadata[las_target_voxel@data$point_source_id, ]
  
  # Convert voxelized LAS data to matrices
  source_pts <- as.matrix(las_source_voxel@data[, c("X", "Y", "Z")])
  target_pts <- as.matrix(las_target_voxel@data[, c("X", "Y", "Z")])
  
  # Perform ICP Alignment using icpmat()
  icp_result <- Morpho::icpmat(target_pts, source_pts, iterations = 50, threads = 12)
  
  # Apply transformation to the target points
  aligned_pts <- (icp_result$R %*% t(target_pts)) + icp_result$t
  aligned_pts <- t(aligned_pts)  # Transpose back
  
  # Create new data frame with aligned points and original metadata
  aligned_las_data <- data.frame(X = aligned_pts[,1], 
                                 Y = aligned_pts[,2], 
                                 Z = aligned_pts[,3])
  
  # Attach metadata from voxelized points
  aligned_las_data <- cbind(aligned_las_data, voxel_metadata[, -which(names(voxel_metadata) %in% c("X", "Y", "Z"))])
  
  # Create new LAS object
  aligned_las <- LAS(aligned_las_data)
  
  # Save aligned LAS file
  lidR::writeLAS(aligned_las, output_las_path)
  
  message("ICP alignment with voxelization complete. Output saved to: ", output_las_path)
  
  return(aligned_las)
}

## Polygon and Raster Processing Functions

#Check to ensure sfc is not empty - used for Leaflet testing
is_empty_sfc <- function(sfc) {
  length(sfc) == 0
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

# Function to capture the raw values of the change detection layer to create the stat_plot
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
  
  return(class_freq)

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
  
  return(list(source = source, target = target, vect_mask = union_vect))
}

# Function to generate ndsm and classify the differences
diff_classify_ndsm <- function(earlier, later) {
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

# Function to generate ndsm and classify the differences
diff_classify_dtm <- function(earlier, later) {
  # Compute the difference
  diff <- later - earlier
  
  # Create a raster for the magnitude of change
  # Classify the differences
  m <- c(-Inf, -5, 1,
         -5, -0.1, 2,
         -0.1, 0.1, 3,
         0.1, 5, 4,
         5, Inf, 5)
  # Create a matrix with the ranges for reclassification
  rclmat <- matrix(m, ncol = 3, byrow = TRUE)
  diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
  return(diff_class)
}

diff_numbers <- function(raster){
  # Get the values of the raster
  raster_values <- terra::values(raster)
  
  # Remove NA values if necessary
  raster_values <- raster_values[!is.na(raster_values)]
  
  # Get the count of each class
  class_counts <- table(raster_values)
  
  # Calculate the total number of cells
  total_cells <- sum(class_counts)
  
  # Calculate the percentage of each class
  class_percentages <- (class_counts / total_cells) * 100
  
  return(list(class_counts = class_counts, class_percentages = class_percentages))
}

#Calculate statistics for a raster and return a data frame to plot in ggplot2

plot_stats <- function(difference_raster) {
  # Get the values of the raster
  raster_values <- terra::values(difference_raster)
  raster_values <- raster_values[!is.na(raster_values)]  # Remove NA values
  
  class_counts <- table(raster_values)
  total_cells <- sum(class_counts)
  class_percentages <- (class_counts / total_cells) * 100
  
  # Define class labels (Shortened & Wrapped)
  class_labels <- c(
    "Large Decrease (< -10m)",
    "Decrease (-0.5m to -10m)",
    "Minimal Change(-0.5m to 0.5m)",
    "Gain (0.5m to 10m)",
    "Large Gain (> 10m)"
  )
  
  plot_data <- data.frame(
    class = factor(names(class_counts), levels = c("1", "2", "3", "4", "5")),
    count = as.numeric(class_counts),
    percentage = as.numeric(class_percentages)
  )
  
  plot_data$class <- factor(plot_data$class, levels = c("1", "2", "3", "4", "5"), labels = class_labels)
  
  # Create the bar chart with wrapped labels
  ggplot2::ggplot(plot_data, aes(x = class, y = count, fill = class)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 4) + 
    labs(x = "Loss and Gain", y = "Area (m^2)", fill = "Class") +
    ggtitle("Raster Statistics for Change Detection") +
    scale_fill_manual(values = c("#555599", "#b2abd2", "#f7f7f7", "#9AE696", "#448F3F"), drop = FALSE) +
    theme_minimal(base_size = 15) +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),  # 🔹 Remove angle, center text
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.6, "cm"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "right"
    ) +
    scale_y_continuous(labels = comma)
}

#Check to ensure SpatRaster is not empty - used for Leaflet testing
is_empty <- function(raster) {
  all(is.na(terra::values(raster)))
}

