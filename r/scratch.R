laz_23 <- readLAS("F:/Thesis/Data/Toronto/Sunnybrook/Laz/Raw/SB_23.laz")
las_19 <- readLAS("F:/Thesis/Data/Toronto/Sunnybrook/Laz/Raw/SB_2019.laz")

laz_23_ground <- filter_poi(laz_23, Classification == 2)
unique(laz_23_ground@data$Classification)

laz_19_ground <- filter_poi(las_19, Classification == 2)


laz_veg_23 <- readLAS("C:\\Users\\cscar\\CF\\data\\SB_veg_23.laz")
laz_veg_19 <- readLAS("C:\\Users\\cscar\\CF\\data\\SB_veg_19.laz")




ground_23 <- lidR::filter_ground(laz_23 )
dtm_23 <- lidR::rasterize_terrain(ground_23, 0.25, tin())

ground_19 <- lidR::filter_ground(las_19)
dtm_19 <- lidR::rasterize_terrain(ground_19, 0.25, tin())

# Step 1: Extract ground elevation from DTM at each point's (X, Y)
ground_elev <- terra::extract(dtm_23, cbind(laz_veg_23@data$X, laz_veg_23@data$Y))[,1]

# Step 2: Add to normalized Z to restore absolute elevation
laz_veg_23@data$Z_absolute <- laz_veg_23@data$Z + ground_elev

laz_veg_mask <- mask_pc(laz_veg_23)
laz_veg_mask19 <- mask_pc(laz_veg_19)

mask_veg_ground_23 <- clip_roi(laz_23_ground, laz_veg_mask)
mask_veg_ground_19 <- clip_roi(laz_19_ground, laz_veg_mask19)

summary(laz_veg_23@data$Z_absolute)

laz_veg_23@data$Z <- laz_veg_23@data$Z_absolute



# Step 1: Extract ground elevation from DTM at each point's (X, Y)
ground_elev19 <- terra::extract(dtm_19, cbind(laz_veg_19@data$X, laz_veg_19@data$Y))[,1]

# Step 2: Add to normalized Z to restore absolute elevation
laz_veg_19@data$Z_absolute <- laz_veg_19@data$Z + ground_elev19


summary(laz_veg_19@data$Z_absolute)

laz_veg_19@data$Z <- laz_veg_19@data$Z_absolute

veg_ground_23 <- rbind(laz_veg_23@data, mask_veg_ground_23@data, fill = TRUE)

# 3. Recreate the LAS object
las_combined_23 <- LAS(veg_ground_23)


veg_ground_19 <- rbind(laz_veg_19@data, mask_veg_ground_19@data, fill = TRUE)
veg_ground_19 <- veg_ground_19[!is.na(veg_ground_19$Z), ]

# 2. Combine the header (you can use the header from one of them, but update bounding box)
hdr <- laz_veg_19@header
hdr@PHB$Number_of_point_records <- nrow(veg_ground_19)

las_combined_19 <- LAS(veg_ground_19, header = hdr)

summary(laz_veg_19@data$Z)
summary(mask_veg_ground_19@data$Z)


writeLAS(laz_23 , "C:\\Users\\cscar\\CF\\data\\SB_veg_23_ground.laz")
writeLAS(las_combined_19, "C:\\Users\\cscar\\CF\\data\\SB_veg_19_ground.laz")

laz_23 <- readLAS("C:\\Users\\cscar\\CF\\data\\SB_veg_23_ground.laz")
laz_19 <- readLAS("C:\\Users\\cscar\\CF\\data\\SB_veg_19_ground.laz")

st_crs(laz_23) <- st_crs(laz_19)


# Function to 2D polygon mask around the ground points in a point cloud
mask_pc <- function(pc) {
  # Step 1: reduce the number of points in the point cloud
  decimate <- lidR::decimate_points(pc, random(1))
  
  # Step 2: Keep only unique points
  unique_points <- decimate@data %>%
    dplyr::distinct(X, Y) # Keep only unique X, Y pairs
  
  
  # Step 3: Convert the decimated points to an sf object
  coords_sf <- sf::st_as_sf(decimate@data[, c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(pc))
  
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