spatial_container <- setRefClass(
  "spatial_container",
  fields = list(
    xyz = "data.frame",
    LPC = "LAS",
    ndsm = "SpatRaster",
    ndsm_raw = "SpatRaster",
    DTM = "SpatRaster",
    DTM_raw = "SpatRaster",
    index = "sfc",
    mask = "sf",
    buildings = "sf",
    filepath = "character",
    filename = "character"
  ),
  methods = list(
    initialize = function(file_path = character(0)) {
      .self$filepath <- file_path
      .self$filename <- basename(file_path)

      # Determine the extension of the filepath
      ext <- tools::file_ext(file_path)

      # create a dummy raster
      dummy_spat <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, resolution = 1, vals = NA)
      .self$DTM <- dummy_spat
      .self$ndsm <- dummy_spat

      if (ext == "xyz") {
        # read the x,y,z file
        xyz_table <- read.table(file_path)

        # add the names to the columns
        names(xyz_table) <- c("X", "Y", "Z")

        # add the xyz object to the xyz list
        .self$xyz <<- xyz_table

        # create a last from the xyz object
        las_xyz <- lidR::LAS(xyz_table)

        # classify the ground points
        ground <- lidR::classify_ground(las_xyz, algorithm = pmf(ws = 5, th = 3))
        las_xyz@data$Classification <- ground@data$Classification
        las_xyz@data$ReturnNumber <- 1
        las_xyz@data$NumberOfReturns <- 1

        # append the new las file to the list
        .self$LPC <<- las_xyz

        # create and append the extent to the index
        las_sf <- sf::st_as_sf(las@data, coords = c("X", "Y"))

        # Get the bounding box
        las_extent <- sf::st_as_sfc(sf::st_bbox(las_sf))

        sf::st_crs(las_extent) <- sf::st_crs(las)

        .self$index <<- las_extent
      } else if (ext == "las" || ext == "laz") {
        # Read the las files
        las <- lidR::readLAS(file_path)
        las_crs <- sf::st_crs(las)

        # append the new las file to the list
        .self$LPC <<- las

        # create and append the extent to the index

        las_sf <- sf::st_as_sf(las@data, coords = c("X", "Y"), crs = las_crs)

        # Step 3: Get the bounding box
        las_extent <- sf::st_as_sfc(sf::st_bbox(las_sf), crs = las_crs)

        .self$index <<- las_extent
      }
    },
    set_crs = function(crs) {
      crs <- as.integer(crs)

      current_crs <- sf::st_crs(.self$LPC)

      # If there's an existing CRS
      if (is.null(current_crs)) {
        cat("The LPC does not have an associated CRS.\n")
        cat("Assigning and transforming to the new CRS...\n")
        sf::st_crs(.self$LPC) <- 4326
        sf::st_crs(.self$mask) <- 4326
        sf::st_crs(.self$index) <- 4326
        .self$LPC <- sf::st_transform(.self$LPC, crs)
        .self$index <- sf::st_transform(.self$index, crs)
      }
      if (current_crs == sf::st_crs(crs)) {
        cat("The new CRS is the same as the current CRS. No change needed.\n")
      } else {
        cat("Changing and transforming to the new CRS...\n")
        sf::st_crs(.self$LPC) <- crs
        .self$LPC <- sf::st_transform(.self$LPC, crs)
        .self$index <- sf::st_transform(.self$index, crs)
      }
    },
    get_data = function() {
      return(.self$xyz)
    },
    get_lpc = function() {
      return(.self$LPC)
    },
    to_xyz = function(path) {
      write.table(.self$xyz[, c("X", "Y", "Z")], path, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
    },
    to_dtm = function(resolution = 1) {
      ground <- lidR::filter_ground(.self$LPC)
      dtm <- lidR::rasterize_terrain(ground, resolution, tin())
      .self$DTM_raw <- dtm
      dtm_clip <- terra::mask(dtm, terra::vect(.self$mask))
      .self$DTM <- dtm_clip
    },
    to_ndsm = function(resolution = 1) {
      nlas <- .self$LPC - .self$DTM

      ndsmfull <- lidR::rasterize_canopy(nlas, res = resolution, p2r(0.6))

      # Set NA values to 0 after rasterization

      ndsmfull[is.na(ndsmfull)] <- 0

      filled <- terra::focal(ndsmfull, w = matrix(1, 5, 5), fun = median, na.rm = TRUE)

      clamp <- terra::clamp(filled, lower = 0)

      .self$ndsm_raw <- clamp
      .self$ndsm <- clamp
    },
    save_mask = function(path) {
      sf::st_write(.self$mask, path)
    },
    save_las = function(path) {
      lidR::writeLAS(.self$LPC, path)
    },
    save_dtm = function(path) {
      terra::writeRaster(.self$DTM, path, gdal = c("COMPRESS=LZW"), overwrite = TRUE)
    },
    save_ndsm = function(path) {
      terra::writeRaster(.self$ndsm, path, gdal = c("COMPRESS=LZW"), overwrite = TRUE)
    },
    save_sc = function(path) {
      save(.self, file = path)
    }
  )
)
