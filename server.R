server = function(input, output, session) {
  
  # Create a reactiveValues object to store the LAS files
  ## Selection and parameters
  rv <- reactiveValues(console_output = list(),
                       in_dir = NULL,
                       out_dir = NULL,
                       metadata = NULL,
                       sc1 = NULL,
                       sc2 = NULL,
                       footprints = NULL,
                       crs = NULL,
                       resolution = NULL,
                       processing = NULL,
                       union_mask = NULL,
                       classified_diff = NULL,
                       results = NULL,
                       current_legend = NULL,
                       out_num = 0,
                       dtm_in = "No",
                       ndsm_in = "No")
  
  #Server logic to accept the directories and plot the metadata
  # Non-reactive value to store the data directory path
  # Define the default paths
  output$photo <- renderImage({
    list(
      src = file.path("./www/steps.png"),
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  data_default <- paste0(getwd(), "/data/")
  save_drive <- paste0(getwd(), "/saves/")
  tmp_drive <- paste0(getwd(), "/tmp/")
  
  drive_list <- list(data_default, save_drive, tmp_drive)
  
  # Call the function to create directories if they don't exist
  
  for (drive in drive_list) {
    create_directories(drive)
  }
  
  rv <- reactiveValues(console_output = list(messages = "Welcome to CF"))
  
  output$console_output <- renderUI({
    lapply(seq_along(rv$console_output), function(i) {
      div(
        style = "border: 1px solid #ccc; padding: 5px; margin: 5px; background-color: #f9f9f9;",
        HTML(paste0("<strong>Message ", i, ":</strong><br>", rv$console_output[[i]]))
      )
    })
  })
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode) {
      shinyjs::runjs("document.body.classList.add('dark-mode');")
    } else {
      shinyjs::runjs("document.body.classList.remove('dark-mode');")
    }
  })
  
  observe({
    shinyjs::toggleState("PC_confirm", !is.null(input$selected_source) && !is.null(input$selected_target))
    shinyjs::toggleState("run_icp", !is.null(rv$sc1) && !is.null(rv$sc2))
    shinyjs::toggleState("run_mask", !is.null(rv$sc1) && !is.null(rv$sc2))
    shinyjs::toggleState("run_denoise", !is.null(rv$sc1) && !is.null(rv$sc2))
    shinyjs::toggleState("dtm1", !is.null(rv$sc1))
    shinyjs::toggleState("dtm2", !is.null(rv$sc2))
    shinyjs::toggleState("ndsm1", !is.null(rv$sc1))
    shinyjs::toggleState("ndsm2", !is.null(rv$sc2))
  })
  
  # Upload files
  observeEvent(input$upload_file, {
    req(input$upload_file, rv)
    save_path <- file.path(paste0(getwd(), "/data"), input$upload_file$name)
    file.copy(input$upload_file$datapath, save_path)
    
    add_message(paste0("File uploaded to: ", save_path), rv)
  })
  
  observeEvent(input$confirm, {
    # Update the reactive values for in_dir, out_dir, resolution, and crs
    
    # input dir
    in_input <- input$in_dir
    rv$in_dir <- normalizePath(in_input)
    
    #output dir
    out_input <- input$out_dir
    rv$out_dir <- normalizePath(out_input)
    
    #raster resolution
    res <- as.integer(input$resolution)
    rv$resolution <- res
    
    # spatial object crs
    crs <- as.integer(input$crs)
    rv$crs <- crs
    
    req(rv$in_dir)
    
    mo_dir <- mo$new(rv$in_dir)
    rv$metadata <- mo_dir$metadata
    
    #plot the metadata
    
    output$plotmeta <- renderTable({
      req(rv$metadata)
      rv$metadata
    })
  })
  
  ##Server logic to load source PC from Directory
  observeEvent(input$confirm, {
    req(rv$metadata)  # Ensure metadata is loaded
    
    # Filter for .laz files
    laz_names <- rv$metadata %>%
      dplyr::filter(grepl("\\.laz$ || \\.las$", file_path)) %>%
      dplyr::pull(file_name)
    
    # Update the dropdown for selecting the source point cloud
    updateSelectInput(session, "selected_source", choices = laz_names)
    updateSelectInput(session, "selected_target", choices = laz_names)
    
    # Filter for footprint files
    shp_names <- rv$metadata %>%
      dplyr::filter(grepl("\\.shp$", file_path)) %>%
      dplyr::pull(file_name)
    
    # Always include "No Footprints" option at the top
    shp_names <- c("No Footprints", shp_names)
    
    updateSelectInput(session, "selected_buildings", choices = shp_names)
    
  })
  
  observeEvent(input$PC_confirm, {
    
    showModal(modalDialog("Initializing LAS and LAS Index", footer = NULL))
    # Ensure the selections are made
    req(input$selected_source, input$selected_target, input$selected_buildings, rv$metadata)
    
    if (tools::file_ext(input$selected_source) == "rds") {
      r_data <- rv$metadata %>%
        dplyr::filter(grepl("\\.rds", file_path)) %>%
        dplyr::select(file_path, file_name)
      
      source_rd <- input$selected_source
      target_rd <- input$selected_target
      
      # Match source_laz and target_laz to their corresponding paths
      source_path <- r_data %>%
        dplyr::filter(file_name == source_rd) %>%
        dplyr::pull(file_path)
      
      # Match laz_name1 and laz_name2 to their corresponding paths
      target_path <- r_data %>%
        dplyr::filter(file_name == target_rd)  %>%
        dplyr::pull(file_path)
      
      # Retrieve the file paths from the selections
      rd_path1 <- normalizePath(source_path, winslash = "/")
      rd_path2 <- normalizePath(target_path, winslash = "/")
      
      showModal(modalDialog("Loading spatial container", footer = NULL))
      
      rv$sc1 <- load(rd_path1)
      rv$sc2 <- load(rd_path2)
      removeModal()
    } else {
      laz_data <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$|\\.las$", file_path)) %>%
        dplyr::select(file_path, file_name)
      
      source_laz <- input$selected_source
      target_laz <- input$selected_target
      buildings_shape <- input$selected_buildings
      
      # Match source_laz and target_laz to their corresponding paths
      source_path <- laz_data %>%
        dplyr::filter(file_name == source_laz) %>%
        dplyr::pull(file_path)
      
      # Match laz_name1 and laz_name2 to their corresponding paths
      target_path <- laz_data %>%
        dplyr::filter(file_name == target_laz)  %>%
        dplyr::pull(file_path)
      
      # Retrieve the file paths from the selections
      las_path1 <- normalizePath(source_path, winslash = "/")
      las_path2 <- normalizePath(target_path, winslash = "/")
      
      if (input$selected_buildings == "No Footprints") {
        rv$footprints <- NULL
      } else {
        # Filter for footprint files
        build_path <- rv$metadata %>%
          dplyr::filter(file_name == buildings_shape) %>%
          dplyr::pull(file_path)
        
        build_path <- normalizePath(build_path, winslash = "/")
        
        # Adding the footprints to the RV
        
        rv$footprints <- sf::st_read(build_path)
      }
      
      showModal(modalDialog("Initializing LAS and Index for PC 1", footer = NULL))
      
      # Process the source point cloud
      if (!is.null(las_path1)) {
        sc1 <- spatial_container$new(as.character(las_path1))
        sc1$set_crs(rv$crs)
        rv$sc1 <- sc1
      }
      
      new_message <- capture_output(print(rv$sc1$LPC))
      add_message(new_message, rv)
      
      showModal(modalDialog("Initializing LAS and Index for PC 2", footer = NULL))
      
      add_message("Initializing the Target point cloud", rv)
      
      # Process the target point cloud
      if (!is.null(las_path2)) {
        sc2 <- spatial_container$new(as.character(las_path2))
        sc2$set_crs(rv$crs)
        rv$sc2 <- sc2  
      }
      new_message <- capture_output(print(rv$sc2$LPC))
      add_message(new_message, rv)
      
      updateSelectInput(session, "io_obj", choices = c("sc1" = "sc1", "sc2" = "sc2"))
      
      output$leafletmap <- renderLeaflet({
        displayIndex(sc1$index)
      })
      
      removeModal()
    }
  })
  # Server logic to to create masks for both PCs on the source and target point clouds
  
  observeEvent(input$run_mask, {
    req(rv$sc1, rv$sc2)
    
    add_message("Generating a mask for Source and Target", rv)
    
    showModal(modalDialog("Creating mask for Source PC", footer = NULL))
    
    # Generate mask for source point cloud
    
    mask_source <- mask_pc(rv$sc1$LPC)
    rv$sc1$mask <- sf::st_transform(mask_source, crs = sf::st_crs(rv$sc1$LPC))
    
    showModal(modalDialog("Creating mask for Target PC", footer = NULL))
    
    mask_target <- mask_pc(rv$sc2$LPC)
    rv$sc2$mask <- sf::st_transform(mask_target, crs = sf::st_crs(rv$sc2$LPC))
    
    removeModal()
  })
  
  # Server logic to Denoise both PCs on the source and target point clouds
  
  observeEvent(input$run_denoise, {
    req(rv$sc1, rv$sc2)
    
    add_message("Running Denoising on Source", rv)
    
    showModal(modalDialog("Running Denoising on Source", footer = NULL))
    
    tryCatch({
      
      if (!is.null(rv$footprints)) {
        noiseless_source <- noise_filter_buildings(rv$sc1$LPC, rv$sc1$mask, rv$footprints)
        rv$sc1$LPC <- noiseless_source
        
      } else {
        
        noiseless_source <- noise_filter(rv$sc1$LPC, rv$sc1$mask)
        rv$sc1$LPC <- noiseless_source
        
      }
      
      
    }, error = function(e) {
      new_message <- paste0("An error occurred: ", e$message)
      add_message(new_message, rv)
    })
    
    add_message("Running Denoising on Target", rv)
    
    showModal(modalDialog("Running Denoising on Target", footer = NULL))
    
    tryCatch({
      
      if (!is.null(rv$footprints)) {
        noiseless_target <- noise_filter_buildings(rv$sc2$LPC, rv$sc2$mask, rv$footprints)
        rv$sc2$LPC <- noiseless_target
        
      } else {
        
        noiseless_target <- noise_filter(rv$sc2$LPC, rv$sc2$mask)
        rv$sc2$LPC <- noiseless_target
      }
      
    }, error = function(e) {
      new_message <- paste0("An error occurred: ", e$message)
      add_message(new_message, rv)
    })
    
    removeModal()
  })
  
  # Server logic to run ICP on the source and target point clouds
  
  observeEvent(input$run_icp, {
    req(rv$sc1, rv$sc2)
    
    add_message("Running ICP on Source and Target", rv)
    
    showModal(modalDialog("Running ICP on Source and Target", footer = NULL))
    
    source_path <- file.path("./tmp/source.laz")
    target_path <- file.path("./tmp/target.laz")
    
    lidR::writeLAS(rv$sc1$LPC, source_path)
    lidR::writeLAS(rv$sc2$LPC, target_path)
    
    # Source the Python script
    icp_module <- paste0(getwd(), "/py/icp_open3d.py")
    
    tryCatch({
      reticulate::use_condaenv("icp_conda")
      reticulate::source_python(icp_module)
      
      # Run ICP
      icp_aligner <- Open3DICP(source_path, target_path, voxel_size = 0.05, icp_method = "point-to-plane")
      
      # Retrieve both outputs
      icp_result <- icp_aligner$align()  
      aligned_file_path <- icp_result[[1]]  # First return value (file path)
      icp_output <- icp_result[[2]]  # Second return value (RMSE + transformation log)
      
      add_message(icp_output, rv)  # Print ICP info to console
      
      if (!is.null(aligned_file_path)) {
        aligned_las <- lidR::readLAS(aligned_file_path)
        
        # Retain attributes while updating XYZ
        rv$sc1$LPC <- pc_metadata(rv$sc1$LPC, aligned_las, rv$crs)
        
        add_message("ICP Alignment successful. LAS classification retained.", rv)
      }
    }, error = function(e) {
      add_message(paste0("Error during ICP: ", e$message), rv)
    })
    removeModal()
  })
  
  #Building the DTM for the first PC with Text Prompts
  observeEvent(input$dtm1, {
    
    showModal(modalDialog("Creating a DTM for the Source PC", footer = NULL))
    
    req(rv$sc1, rv$resolution)
    tryCatch({
      
      # Check if any points are classified as ground (Classification == 2)
      if (sum(rv$sc1$LPC@data$Classification == 2, na.rm = TRUE) == 0) {
        message("No ground points detected, running classify_ground()...")
        
        # Apply ground classification method (you can adjust the parameters)
        rv$sc1$LPC <- lidR::classify_ground(rv$sc1$LPC, csf(sloop_smooth = TRUE, rigidness = 2))
        
      } else {
        
        message("Ground points already exist, skipping classification.")
        
      }
      
      #Calling the DTM Function
      rv$sc1$to_dtm(rv$resolution)
      
      #Printing the DTM statistics
      object_message <- capture_output(print(rv$sc1$DTM))
      
      add_message(object_message, rv)
      
    }, error = function(e){
      new_message <- paste("Error in creating DTM:", e$message)
      add_message(new_message, rv)
    })
    removeModal()
    
  })
  
  observeEvent(input$dtm2, {
    
    #Ensuring that the resolution and the PC are selected
    req(rv$sc2, rv$resolution)
    
    showModal(modalDialog("Creating a DTM for the Target PC", footer = NULL))
    #Pushing the text prompt for DTM2 to the console
    
    tryCatch({
      
      # Check if any points are classified as ground (Classification == 2)
      if (sum(rv$sc2$LPC@data$Classification == 2, na.rm = TRUE) == 0) {
        message("No ground points detected, running classify_ground()...")
        
        # Apply ground classification method (you can adjust the parameters)
        rv$sc2$LPC <- lidR::classify_ground(rv$sc2$LPC, csf(sloop_smooth = TRUE, rigidness = 2))
      } else {
        message("Ground points already exist, skipping classification.")
      }
      
      #Calling the DTM Function
      rv$sc2$to_dtm(rv$resolution)
      updateSelectInput(session, "selected_target_raster", choices = c("DTM" = "sc2$DTM"))
      
      #Printing the DTM statistics
      object_message <- capture_output(print(rv$sc2$DTM))
      add_message(object_message, rv)
      
    }, error = function(e){
      new_message <- paste("Error in creating DTM:", e$message)
      add_message(new_message, rv)
    })
    removeModal()
  })
  
  observeEvent(input$ndsm1, {
    
    req(rv$sc1, rv$resolution)
    
    showModal(modalDialog("Creating an nDSM for the Source PC", footer = NULL))
    
    new_message <- paste0("Generating nDSM for Source")
    add_message(new_message, rv)
    
    tryCatch({
      
      rv$sc1$to_ndsm(resolution = rv$resolution)
      
      object_message <- capture_output(print(rv$sc1$ndsm))
      add_message(object_message, rv)
    }, error = function(e){
      new_message <- paste("Error in creating nDSM:", e$message)
      add_message(new_message, rv)
    })
    removeModal()
  })
  
  observeEvent(input$ndsm2, {
    
    req(rv$sc2, rv$resolution)
    showModal(modalDialog("Creating an nDSM for the Target PC", footer = NULL))
    
    new_message <- paste0("Generating nDSM for Target")
    add_message(new_message, rv)
    
    tryCatch({
      
      rv$sc2$to_ndsm(resolution = rv$resolution)
      
      object_message <- capture_output(print(rv$sc2$ndsm))
      add_message(object_message, rv)
    }, error = function(e) {
      new_message <- paste("Error in creating nDSM for Target:", e$message)
      add_message(new_message, rv)
    })
    removeModal()
  })
  
  observeEvent(input$selected_processing, {
    req(input$selected_processing != "")  # Ensure valid input
    
    new_message <- paste0("Raster processing selected: ", input$selected_processing)
    add_message(new_message, rv)
    
    # Perform further processing based on the selected raster type
    rv$processing <- input$selected_processing
  })

  
  observeEvent(input$align_rasters, {
    req(rv$sc1, rv$sc2, rv$processing)
    
    showModal(modalDialog("Aligning Rasters", footer = NULL))
    
    new_message <- "Running Raster Alignment"
    add_message(new_message, rv)
    
    
    if (rv$processing == "nDSM") {
      
      tryCatch({
        aligned_nDSM <- process_raster(source = rv$sc1$ndsm_raw, target = rv$sc2$ndsm_raw, source_mask = rv$sc1$mask, target_mask = rv$sc2$mask, method = "bilinear")
        
        rv$source_raster <- aligned_nDSM[[1]]
        rv$target_raster <- aligned_nDSM[[2]]
        rv$union_mask <- aligned_nDSM[[3]]
        
        
        new_message <- "Raster Alignment Complete"
        add_message(new_message, rv)
        
      }, error = function(e){
        new_message <- paste("Error in aligning nDSM:", e$message)
        add_message(new_message, rv)
      })
    } else if (rv$processing == "DTM") {
      
      tryCatch({
        aligned_dtm <- process_raster(source = rv$sc1$DTM_raw, target = rv$sc2$DTM_raw, source_mask = rv$sc1$mask, target_mask = rv$sc2$mask, method = "bilinear")
        
        rv$source_raster <- aligned_dtm[[1]]
        rv$target_raster <- aligned_dtm[[2]]
        rv$union_mask <- aligned_dtm[[3]]
        
        new_message <- "Raster Alignment Complete"
        add_message(new_message, rv)
        
      }, error = function(e){
        new_message <- paste("Error in aligning DTMs:", e$message)
        add_message(new_message, rv)
      })
    }
    removeModal()
  })
  
  #Button logic to classify the difference between the two ndsms
  
  observeEvent(input$classify_raster, {
    # Ensure the DTMs exist and have been processed for each spatial_obj
    req(rv$source_raster, rv$target_raster, rv$union_mask, rv$footprints, rv$resolution, rv$crs)
    new_message <- "Running Classification"
    add_message(new_message, rv)
    
    tryCatch({
      
      # Create a new raster with the same extent as SB_Change and 1m resolution
      template_raster <- terra::rast(extent = terra::ext(rv$source_raster), resolution = rv$resolution)
      
      
      # Step 3: Rasterize the building polygons onto the new 1m raster grid
      buildings_raster <- terra::rasterize(terra::vect(rv$footprints), template_raster, background = NA)
      
      terra::crs(buildings_raster) <- terra::crs(rv$source_raster)
      
      # Step 4: Set the overlapping raster cells in SB_Change to 0 where buildings exist
      rv$source_raster[!is.na(buildings_raster)] <- 0
      rv$target_raster[!is.na(buildings_raster)] <- 0
      
      classified_diff <- diff_classify(rv$source_raster, rv$target_raster)
      diff_class <- terra::mask(classified_diff, rv$union_mask)
      
      # Save the processed raster in the rv list so it can be accessed elsewhere
      rv$classified_diff <- diff_class
      
      new_message <- "Classification Complete"
      add_message(new_message, rv)
      
    }, error = function(e){
      new_message <- paste("Error in Classifying nDSMs:", e$message)
      add_message(new_message, rv)
    })
  })
  
  
  ## Plot Source LAS
  observeEvent(input$plot_source, {
    output$plot3D <- rgl::renderRglwidget({
      req(rv$sc1$LPC)
      pc1_decimate <- lidR::decimate_points(rv$sc1$LPC, random(1))
      lidR::plot(pc1_decimate, bg = "white")
      rglwidget()
    })
  })
  
  ## Plot Target LAS
  
  observeEvent(input$plot_target, {
    output$plot3D <- rgl::renderRglwidget({
      req(rv$sc2$LPC)
      pc2_decimate <- lidR::decimate(rv$sc2$LPC, random(1))
      lidR::plot(pc2_decimate , bg = "white")
      rglwidget()
    })
  })
  
  ##Plot Results
  
  observeEvent(input$plot_results, {
    output$plot2D <- renderPlot({
      req(rv$classified_diff)
      plot_stats(rv$classified_diff)
    })
  })
  
  ##Plot Webmap
  
  observeEvent(input$plot_leaf, {
    tryCatch({
      
      output$leafletmap <- renderLeaflet({
        displayMap(rv$sc1$DTM, rv$sc1$ndsm, rv$sc2$DTM, rv$sc2$ndsm, rv$classified_diff, rv$union_mask)
      })
    }, error = function(e) {
      print(paste("An error occurred while plotting the map:", e$message))
    })
  })
  
  
  observeEvent(input$leafletmap_groups, {
    legend <- NULL
    
    if ("DTM (Source)" %in% input$leafletmap_groups) {
      legend <- "DTM (Source)"
    } else if ("DTM (Target)" %in% input$leafletmap_groups) {
      legend <- "DTM (Target)"
    } else if ("nDSM (Source)" %in% input$leafletmap_groups) {
      legend <- "nDSM (Source)"
    } else if ("nDSM (Target)" %in% input$leafletmap_groups) {
      legend <- "nDSM (Target)"
    } else if ("Diff" %in% input$leafletmap_groups) {
      legend <- "Diff"
    }
    
    rv$current_legend <- legend
    
    leafletProxy("leafletmap") %>% clearControls()
    
    # creating the legend.
    if (!is.null(rv$current_legend)) {
      
      if (rv$current_legend == "DTM (Source)") {
        leafletProxy("leafletmap") %>%
          addLegend(pal = colorNumeric("magma", domain = values(rv$sc1$DTM), na.color = "transparent"), 
                    values = values(rv$sc1$DTM), position = "bottomright", title = "DTM (Source)", 
                    layerId = "dtm1Legend", opacity = 1)
      } else if (rv$current_legend == "DTM (Target)") {
        leafletProxy("leafletmap") %>%
          addLegend(pal = colorNumeric("magma", domain = values(rv$sc2$DTM), na.color = "transparent"), 
                    values = values(rv$sc2$DTM), position = "bottomright", title = "DTM (Target)", 
                    layerId = "dtm2Legend", opacity = 1)
      } else if (rv$current_legend == "nDSM (Source)") {
        leafletProxy("leafletmap") %>%
          addLegend(pal = colorNumeric("magma", domain = values(rv$sc1$ndsm), na.color = "transparent"), 
                    values = values(rv$sc1$ndsm), position = "bottomright", title = "nDSM (Source)", 
                    layerId = "ndsm1Legend", opacity = 1)
      } else if (rv$current_legend == "nDSM (Target)") {
        leafletProxy("leafletmap") %>%
          addLegend(pal = colorNumeric("magma", domain = values(rv$sc2$ndsm), na.color = "transparent"), 
                    values = values(rv$sc2$ndsm), position = "bottomright", title = "nDSM (Target)", 
                    layerId = "ndsm2Legend", opacity = 1)
      } else if (rv$current_legend == "Diff") {
        leafletProxy("leafletmap") %>%
          addLegend(colors = c("#6cBD66", "#9AE696", "#f7f7f7", "#b2abd2", "#555599"), 
                    labels = c("> 10", "0.5 to 10", "-0.5 to 0.5", "-10 to -0.5", "< -10"),
                    position = "bottomright", title = "Change in Normalized Surface Height (m)", 
                    layerId = "diffLegend", opacity = 1)
      }
    }
  })
  
  ## Save Buttons
  
  # Selecting which SC to save
  selected_las <- reactive({
    req(input$io_obj)
    rv[[input$io_obj]]
  })
  
  #Saving the xyz from the PCC
  
  observeEvent(input$save_las, {
    req(rv$sc1, rv$sc2, rv$out_dir)
    
    filename <- rv$sc1$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    out_dir <- rv$out_dir
    path <- paste0(out_dir, "/", file_name, ".laz")
    rv$sc1$save_las(path)
    
    #saving sc2
    filename <- rv$sc2$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    path <- paste0(out_dir, "/", file_name , ".laz")
    rv$sc2$save_las(path)
    
  })
  
  # saving the spatial container
  
  observeEvent(input$save_sc, {
    req(rv$sc1, rv$sc2, rv$out_dir)
    
    # saving sc1
    filename <- rv$sc1$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    out_dir <- rv$out_dir
    path <- paste0(out_dir, "/", file_name, ".rds")
    rv$sc1$save_sc(path)
    
    #saving sc2
    filename <- rv$sc2$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    path <- paste0(out_dir, "/", file_name, ".rds")
    rv$sc2$save_sc(path)
    
  })
  
  observeEvent(input$save_dtm, {
    req(rv$sc1, rv$sc2, rv$out_dir)
    
    # saving sc1
    filename <- rv$sc1$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    out_dir <- rv$out_dir
    path <- paste0(out_dir, "/", file_name, "_dtm.tif")
    rv$sc1$save_dtm(path)
    
    #saving sc2
    filename <- rv$sc2$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    path <- paste0(out_dir, "/", file_name, "_dtm.tif")
    rv$sc2$save_dtm(path)
    
  })
  
  observeEvent(input$save_ndsm, {
    req(rv$sc1, rv$sc2, rv$out_dir)
    
    # saving sc1
    filename <- rv$sc1$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    out_dir <- rv$out_dir
    path <- paste0(out_dir, "/", file_name, "_ndsm.tif")
    rv$sc1$save_ndsm(path)
    
    #saving sc2
    filename <- rv$sc2$filename
    file_name <- stringr::str_split(filename, "\\.")[[1]][1]
    path <- paste0(out_dir, "/", file_name, "_ndsm.tif")
    rv$sc2$save_ndsm(path)
  })
  
  observeEvent(input$save_mask, {
    
    # Ensure selected_las() is not NULL
    
    req(selected_las(), rv$out_dir)  
    
    filepath <- selected_las()$filepath
    filename <- basename(filepath)
    filesplit <- strsplit(filename, "\\.")[[1]]
    final_name <- filesplit[1]
    out_dir <- rv$out_dir
    
    path <- paste0(out_dir, "/", final_name, "_mask.shp")
    selected_las()$save_mask(path)
    print(paste("Mask saved to:", path))
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("all_files", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      req(rv$out_dir)
      save_dir <- rv$out_dir
      file_zip <- list.files(save_dir, full.names = TRUE)
      zip::zipr(file, file_zip)
    },
    contentType = "application/zip"
  )
  
  # Clean up uploaded files when the session ends, excluding base files
  session$onSessionEnded(function() {
    print("Session ended")
    
    # Safely access reactive values
    req(isolate(rv$in_dir), isolate(rv$out_dir))
    in_dir <- isolate(rv$in_dir)
    out_dir <- isolate(rv$out_dir)
    
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
    
    # Delete specific files from in_dir (retain base files)
    if (dir.exists(in_dir)) {
      base_files <- c("SB_15_dec.laz", "SB_19_dec.laz", "SB_23_dec.laz", "SB_19.laz", "SB_23.laz", "SB_Buildings.shp", "SB_Buildings.dbf", "SB_Buildings.shx", "SB_Buildings.prj")
      uploaded_files <- list.files(in_dir, full.names = TRUE)
      print("Uploaded files at session end:")
      print(uploaded_files)
      
      lapply(uploaded_files, function(file) {
        if (file.exists(file) && !basename(file) %in% base_files) {
          result <- file.remove(file)
          if (result) {
            print(paste("Deleted file:", file))
          } else {
            warning(paste("Failed to delete file:", file))
          }
        }
      })
    } else {
      warning("in_dir does not exist:", in_dir)
    }
    
    # Delete all files in out_dir
    delete_all_files(out_dir)
  })
}
