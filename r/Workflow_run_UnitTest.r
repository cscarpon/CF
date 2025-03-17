source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

# renv::install(c("shiny", "shinyjs", "dplyr", "rgl", "terra", "lidR", "sf", "ggplot2",
#                 "scales", "leaflet", "leaflet.extras", "htmlwidgets", "reticulate",
#                 "zip", "leafem", "stringr", "rmapshaper", "nngeo", "shinyWidgets",
#                 "shinyBS", "shinyscreenshot"))


# change this based on CPU
set_lidr_threads(12)

sunny_bound <- st_read("G:/EMC/Projects/Sunnybrooke/Data/Boundary/Sunnybrook.shp")
sunny_bound <- st_transform(sunny_bound, crs = 26917)

buildings <- sf::st_read(file.path("./data/SB_Buildings.shp"))
buildings <- sf::st_transform(buildings, crs = 26917)

# to_dtm()        save_las()
# to_chm()        save_dtm()
# save_mask()     save_chm()
# save_pc()

# Functions

# process_raster(source, target, mask_layer)
# CHM_diff_classify(earlier, later)
# raster_stats(raster)
# mask_pc()

dir <- "./data/"
mo_dir <- mo$new(dir)
print(mo_dir$metadata)


start.time <- Sys.time() # Start timer
end.time <- Sys.time() # End timer
time.taken <- end.time - start.time # Calculate time difference

start.time <- Sys.time() # Start timer
pc_14 <- spatial_container$new(mo_dir$metadata$file_path[3])
end.time <- Sys.time() # End timer
time.taken_index15 <- end.time - start.time # Calculate time difference
print(time.taken_index15)
pc_14$set_crs(26917)

# # path_19 <- "data/TTP_2019_decimate.laz"
# pc_19 <- spatial_container$new(mo_dir$metadata$file_path[4])
# pc_19$set_crs(26917)


# # path_19 <- "data/TTP_2019_decimate.laz"
start.time <- Sys.time() # Start timer
pc_23 <- spatial_container$new(mo_dir$metadata$file_path[4])
pc_23$set_crs(26917)
end.time <- Sys.time() # End timer
time.taken_index23 <- end.time - start.time # Calculate time difference
print(time.taken_index23)


# Generating the Masks

start.time <- Sys.time() # Start timer
pc_14$mask <- mask_pc(pc_14$LPC)
end.time <- Sys.time() # End timer
time.taken_mask14 <- end.time - start.time # Calculate time difference
print(time.taken_mask14)

# pc_19$mask <- mask_pc(pc_19$LPC)

start_time <- Sys.time()
pc_23$mask <- mask_pc(pc_23$LPC)
end_time <- Sys.time()
time_taken_mask23 <- end_time - start_time
print(time_taken_mask23)


# adding the buildings

# if (sf::st_crs(buildings) != sf::st_crs(pc_14$mask)) {
#   pc_14$buildings <- sf::st_transform(buildings, sf::st_crs(pc_14$mask))
# } else {
#   pc_14$buildings <- buildings
# }
#
# if (sf::st_crs(buildings) != sf::st_crs(pc_19$mask)) {
#   pc_19$buildings <- sf::st_transform(buildings, sf::st_crs(pc_19$mask))
# } else {
#   pc_19$buildings <- buildings
# }
#
# if (sf::st_crs(buildings) != sf::st_crs(pc_23$mask)) {
#   pc_23$buildings <- sf::st_transform(buildings, sf::st_crs(pc_23$mask))
# } else {
#   pc_23$buildings <- buildings
# }


# Denoising

# pc_14$LPC <- noise_filter_buildings(pc_14$LPC, pc_14$mask, buildings)

start_time <- Sys.time()
pc_14$LPC <- noise_filter(pc_14$LPC)
end_time <- Sys.time()
time_taken_denoise14 <- end_time - start_time
print(time_taken_denoise14)

unique(pc_14$LPC@data$Classification)

# pc_19$LPC <- noise_filter_buildings(pc_19$LPC, pc_19$mask, buildings)
# pc_19$LPC <- noise_filter(pc_19$LPC)

# unique(pc_19$LPC@data$Classification)
#
# pc_23$LPC <- noise_filter_buildings(pc_23$LPC, pc_23$mask, buildings)

start_time <- Sys.time()
pc_23$LPC <- noise_filter(pc_23$LPC)
end_time <- Sys.time()
time_taken_denoise23 <- end_time - start_time
print(time_taken_denoise23)



source_path <- file.path("./tmp/source.laz")
target_path <- file.path("./tmp/target.laz")
aligned_path <- file.path("./data/aligned.laz")



lidR::writeLAS(pc_14$LPC, source_path)
lidR::writeLAS(pc_23$LPC, target_path)


source("./r/functions.R")
# Example usage with morpho
start_time <- Sys.time()
aligned_las <- align_las_icp_voxelized(pc_14$LPC, pc_23$LPC, aligned_path)
end_time <- Sys.time()
time_taken_morpho <- end_time - start_time
print(time_taken_morpho)

# Source the Python script
# icp_module <- paste0(getwd(), "/py/icp_open3d.py")

icp_module <- paste0(getwd(), "/py/icp_open3D_test.py")

reticulate::use_condaenv("icp_conda")
reticulate::source_python(icp_module)

# Run ICP
start_time <- Sys.time()
icp_aligner <- Open3DICP(source_path, target_path, voxel_size = 0.05, icp_method = "point-to-plane", use_gpu = TRUE )
aligned_file_path <- icp_aligner$align()
end_time <- Sys.time()
time_taken_open3d <- end_time - start_time
print(time_taken_open3d)

# renv::use_python("C:/Users/cscar/anaconda3/envs/EMT_conda/python.exe")

# # Source the Python script
# icp_module <- paste0(getwd(), "/py/icp_pdal.py")
#
# reticulate::source_python(icp_module)
#
# # Create instance of the ICP class
# icp_aligner <- pdal_icp(pc_14$filepath, pc_19$filepath)
#
# # Call the align method
# aligned_file_path <- icp_aligner$align()

# Process the source point cloud

pc_14A <- spatial_container$new(as.character(aligned_file_path[1]))
pc_14A$set_crs(26917)
pc_14A$mask <- mask_pc(pc_14A$LPC)

# Generating the DTM and nDSM

start_time <- Sys.time()
pc_14A$to_dtm(1)
end_time <- Sys.time()
time_taken_dtm14 <- end_time - start_time
print(time_taken_dtm14)

# pc_19$to_dtm(1)

start_time <- Sys.time()
pc_23$to_dtm(1)
end_time <- Sys.time()
time_taken_dtm23 <- end_time - start_time
print(time_taken_dtm23)

start_time <- Sys.time()
pc_14A$to_ndsm(1)
end_time <- Sys.time()
time_taken_ndsm14 <- end_time - start_time
print(time_taken_ndsm14)


# pc_19$to_ndsm(1)
start_time <- Sys.time()
pc_23$to_ndsm(1)
end_time <- Sys.time()
time_taken_ndsm23 <- end_time - start_time
print(time_taken_ndsm23)



time_names <- list("Index 15" = time.taken_index15, "Index 23" = time.taken_index23, "Denoise 15" = time_taken_denoise14, "Denoise 23" = time_taken_denoise23, "DTM 15" = time_taken_dtm14, "DTM 23" = time_taken_dtm23, "nDSM 15" = time_taken_ndsm14, "nDSM 23" = time_taken_ndsm23, "Open3D" = time_taken_open3d)




# plot(pc_19$CHM)

# This function aligns the two rasters and returns aligned raster objects.
aligned_ndsm <- process_raster(source = pc_14A$ndsm_raw, target = pc_23$ndsm_raw, source_mask = pc_14A$mask, target_mask = pc_23$mask, method = "bilinear")

source("./r/functions.R")
################################################
################################################

source_ndsm <- aligned_ndsm[[1]]
target_ndsm <- aligned_ndsm[[2]]
ndsm_mask <- aligned_ndsm[[3]]

# Function to generate CHM and classify the differences

diff_class <- diff_classify(source_ndsm, target_ndsm)

plot_stats(diff_class)

# difference_values <- diff_values(diff_class)

# Display the outputs.

displayMap(pc_14A$DTM, pc_14A$ndsm, pc_23$DTM, pc_23$ndsm, diff_class, ndsm_mask)

# Get the values of the raster

# Get the frequency of each class
class_freq <- freq(diff_class)

# Calculate the total number of cells
total_cells <- sum(class_freq[, "count"])

# Calculate percentage for each class
class_freq$percentage <- (class_freq$count / total_cells) * 100

# Set the class values as row names
rownames(class_freq) <- class_freq$value

# Remove the 'value' column since it's now the row name
class_freq <- class_freq[, -1]

return(class_percentages)

pc_23$LPC@header@EVLR
