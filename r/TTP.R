library(lidR)
library(sf)
library(terra)

index23 <- st_read("D:/Data/Lidar/TRCA/combined.shp")

ttp_bound <- st_read("F:/Thesis/TTP/Data/TTP_BoundaryV2.shp")
ttp_bound <-st_transform(ttp_bound, crs = st_crs(index23))
ttp_bound_buff <- st_buffer(ttp_bound, 20)
ttp_bound_buff <- st_transform(ttp_bound_buff, crs = 26917)

plot(ttp_bound_buff)
ttp_bound_buff_re <- st_transform(ttp_bound_buff, crs = st_crs(laz23))

ttp_index <- st_intersection(ttp_bound, index23)

names(ttp_index)


laz_test <- readLAS(laz15_files[1])

st_crs(laz_test) <- 26917
laz_test1 <- st_transform(laz_test, 26917)
plot(laz_test1)

st_crs(laz_test) <- 26917

laz_clip_test <- clip_roi(laz_test, ttp_bound_buff)
plot(laz_clip_test)
laz_test@data[1]


laz15_dir <- "F:/Thesis/TTP/Data/LAS/2014"
laz15_files <- list.files(laz15_dir, full.names = TRUE, pattern = ".las$")
ctg15 <- readLAScatalog(laz15_files)
laz15 <- readLAS(ctg15)
st_crs(laz15) <- 26917
laz15 <- st_transform(laz15, 26917)
writeLAS(laz15, "F:/Thesis/TTP/Data/LAS/combined/2015/combined15.laz")
laz15_clip <- clip_roi(laz15, ttp_bound_buff)
writeLAS(laz15_clip, "F:/Thesis/TTP/Data/LAS/combined/2015/TTP15.laz")
laz15 <- readLAS("F:/Thesis/TTP/Data/LAS/combined/2015/TTP15.laz")

names(laz15@data)

laz19_dir <- "F:/Thesis/TTP/Data/LAS/2019"
laz19_files <- list.files(laz19_dir, full.names = TRUE, pattern = ".las$")
ctg19 <- readLAScatalog(laz19_files)
laz19 <- readLAS(ctg19)
writeLAS(laz19, "F:/Thesis/TTP/Data/LAS/combined/2019/combined19.laz")
laz19_clip <- clip_roi(laz19, ttp_bound_buff)
writeLAS(laz19_clip, "F:/Thesis/TTP/Data/LAS/combined/2019/TTP19.laz")


st_crs(laz19)

laz23_dir <- "F:/Thesis/TTP/Data/LAS/2023"
laz23_files <- list.files(laz23_dir, full.names = TRUE, pattern = ".las$")
ctg23 <- readLAScatalog(laz23_files)
laz23 <- readLAS(ctg23)
writeLAS(laz23, "F:/Thesis/TTP/Data/LAS/combined/2023/combined23.laz")
laz23_clip <- clip_roi(laz23, ttp_bound_buff_re)
laz23_clip <- st_transform(laz23_clip, 26917)
writeLAS(laz23_clip, "F:/Thesis/TTP/Data/LAS/combined/2023/TTP23.laz")

laz15_clip

decimate <- lidR::decimate_points(laz15_clip, random(1))

unique_points <- decimate@data %>%
  dplyr::distinct(X, Y)  # Keep only unique X, Y pairs

coords_sf <- sf::st_as_sf(unique_points[, c("X", "Y")], 
                          coords = c("X", "Y"), 
                          crs = lidR::projection(laz15_clip))



coords_vect <- terra::vect(coords_sf)

raster_template <- terra::rast(terra::ext(coords_vect), resolution = 3)

rasterized <- terra::rasterize(coords_vect, raster_template, field = NULL, fun = "count")

polygonized <- terra::as.polygons(rasterized, dissolve = TRUE)

simplified_polygon <- terra::aggregate(polygonized)

no_holes <- nngeo::st_remove_holes(sf::st_as_sf(simplified_polygon))

final_sf <- rmapshaper::ms_simplify(no_holes, keep = 0.8, weighting = 1, keep_shapes = TRUE)
final_sf <- sf::st_as_sf(final_sf)
sf::st_crs(final_sf) <- sf::st_crs(pc)
final_sf <- sf::st_transform(final_sf, crs = sf::st_crs(pc))

return(final_sf)

las15 <- read


reticulate::use_condaenv("icp_conda") 
