library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

ui <- navbarPage(
  title = "CloudFlux (CF)",

  # Move JavaScript and styling to the header
  header = tagList(
    useShinyjs(), # Enable JavaScript features
    tags$head(
      tags$style(HTML("
      
        /* ===================== GLOBAL LAYOUT ===================== */
        html, body {
            height: 100vh;
            margin: 0;
            overflow: hidden;
        }
        
        /* ===================== INTRODUCTION PAGE ===================== */
        .intro-page {
            padding: 20px;
        }
        
        /* ===================== SIDEBAR ===================== */
        .sidebar-content {
            padding: 0.5vw;
            padding-top: 2vh;
            height: 100vh;
            width: 30%;           
            position: fixed;
            top: 0;
            left: 0;
            overflow-y: auto;
            background-color: #f9f9f9;
        }
        
        /* ===================== INPUT CONTAINER (One Element Per Line) ===================== */
        .input-container {
            display: flex;
            flex-direction: column;
            align-items: flex-start;
            padding-top: 3vh;
        }
        
        .input-container .shiny-input-container,
        .input-container .btn {
            width: 100%;               /* Full-width inputs and buttons */
        }
        
        /* ===================== BUTTON CONTAINER ===================== */
        .button-container {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
            gap: 0.5vw;
        }
        
        .button-container .btn {
            width: 100%;
        }
        .button-confirm {
          display: flex;
          justify-content: left; 
          width: 100%;
          margin-top: 0.5vh;
        }       
        
        /* ===================== MAIN PANEL ===================== */
        .main-panel {
            margin-left: 30%;  /* Matches sidebar width */
            width: 100%;        /* Remaining content space */
            height: 100vh;
            display: flex;
            flex-direction: column;
        }
        /* ===================== MAIN CONTENT (Leaflet/Plots) ===================== */
        .main-content {
            flex-grow: 1;
            height: 100%;
            width: 100%;
        }
        /* ===================== CONSOLE OUTPUT ===================== */
        .console-container {
            position: fixed;
            bottom: 0;
            left: 30%;
            width: 100%;
            height: 25vh;
            background: #f8f9fa;
            border-top: 2px solid #ccc;
            box-sizing: border-box;
            overflow-y: auto;
            padding-top: 1vh;
        }
        
        #console_output {
            height: 100%;
            width: 100%;
            background: #f8f9fa;
            border: 1px solid #ccc;
        }

      ")),
    )
  ),

  # First tab: Introduction
  tabPanel(
    title = "Introduction",
    div(class = "intro-page",
    fluidPage(
      h2("Welcome to CloudFlux (CF)"),
      tags$div(
        style = "text-align: justify;", # Ensures the text fills the container and is justified
        "CF is designed to visualize, process, and analyze point cloud data. It can ingest LAS or LAZ formats. CF will create Digital Terrain Models (DTMs) and normalized Digital Surface Models (nDSM) and align point clouds for change detection. User data uploaded to CF is not saved and does not persist in the application. CF is free for use and it was built on the efforts of the open-source and open-access communities.", tags$br(),
        tags$br(),
        tags$b("Disclaimer:"), "CF is an educational tool and is not intended to replace professional advice or certified data processing workflows.The outputs are provided 'as is,' with no guarantee of accuracy, completeness, or suitability for any specific purpose. The developers of CF are not liable for any errors, inaccuracies, or decisions made based on its use. Use at your own risk.", tags$br(),
        tags$br(),
        "Users can uploaded their own point clouds, or they can use the data for Sunnybrook Campus (SB_19.laz and SB_23.laz) which comes preloaded. Below are the steps to use the tool:", tags$br(),
        tags$br()
      ),
      tags$ul(
        tags$li("Step 1: Upload the source and target point clouds. Confirm Inputs"),
        tags$li("Step 2: Select your source and target point clouds. Confirm Point Clouds"),
        tags$li("Step 3: Create Mask for Point Clouds"),
        tags$li("Step 4: Denoise point clouds."),
        tags$li("Step 5: ICP Alignment (currently not working for web tool)."),
        tags$li("Step 6: Generate DTMs for source and target."),
        tags$li("Step 7: Generate nDSMs for source and target"),
        tags$li("Step 8: Select DTM or nDSM for change detection"),
        tags$li("Step 9: Align rasters."),
        tags$li("Step 10: Classify"),
        tags$li("Step 11: Visualize the outputs in maps, 2D statistics, and 3D plots.")
      ),
      div(class = "responsive-img", imageOutput("photo"))
      )
    )
  ),

  # Second tab: CloudFlux UI
  tabPanel(
    title = "CloudFlux",
    div(class = "sidebar-content",
      div(class = "input-container",
        h4("Data Input"),
        fileInput("upload_file", "Upload Point Cloud (.laz or .las)", accept = c(".laz", ".las"), width = "100%"),
        textInput("in_dir", "Input directory:", value = paste0(getwd(), "/data/"), width = "100%"),
        textInput("out_dir", "Output directory:", value = paste0(getwd(), "/saves/"), width = "100%"),
        numericInput("resolution", "Resolution:", value = 1, width = "100%"),
        numericInput("crs", "CRS:", value = 26917, width = "100%"),
  
        div(class = "button-confirm",
            actionButton("confirm", "Confirm Inputs")
        ),
        bsTooltip("confirm", "Ensure you are selecting the appropriate CRS and resolution."),
        ),
        h4("Data Processing"),
        div(class = "button-container",
          selectInput("selected_source", "Select Source", choices = NULL),
          selectInput("selected_target", "Select Target",  choices = NULL),
          selectInput("selected_buildings", "Select Footprints (.shp)", choices = NULL)
        ),
        div(class = "button-confirm",
          actionButton("PC_confirm", "Confirm Point Clouds")
        ),
        bsTooltip("PC_confirm", "Confirm selection of source and target point clouds."),
        h4("Pre-Processing"),
        div(class = "button-container",
          actionButton("run_icp", "ICP Alignment"),
          bsTooltip("run_icp", "Align your source point cloud to your target point cloud"),
          actionButton("run_mask", "Generate Mask"),
          bsTooltip("run_mask", "Create a mask to outline the boundary of the point cloud"),
          actionButton("run_denoise", "Remove Noise"),
          bsTooltip("run_denoise", "Remove outlier points and building footprints")
        ),
        h4("Raster Generation"),
        div(class = "button-container",
          actionButton("dtm1", "DTM (Source)"),
          actionButton("dtm2", "DTM (Target)"),
          actionButton("ndsm1", "nDSM (Source)"),
          actionButton("ndsm2", "nDSM (Target)")
        ),
        h4("Post Processing"),
        selectInput("selected_processing", "Raster Type to Align", choices = c("", "DTM", "nDSM"), width = "100%"),
        div(class = "button-container",
          actionButton("align_rasters", "Align Rasters", class = "btn"),
          bsTooltip("align_rasters", "Alignment is required to compare the rasters"),
          actionButton("classify_raster", "Classify Rasters", class = "btn"),
          bsTooltip("classify_raster", "Classify rasters to identify changes in the landscape"),
          tags$hr()
        ),
        h4("Plotting"),
        div(class = "button-container",
          actionButton("plot_source", "Plot Source Las", class = "btn"),
          actionButton("plot_target", "Plot Target Las", class = "btn"),
          actionButton("plot_leaf", "Plot to Leaflet", class = "btn"),
          actionButton("plot_DTM_results", "Plot DTM Results", class = "btn"),
          actionButton("plot_nDSM_results", "Plot nDSM Results", class = "btn")
        ),
        h4("Data Saving"),
        selectInput("io_obj", "Select PC to Save", choices = NULL, width = "100%"),
        div(class = "button-container",
          actionButton("screenshot_btn", "Take Screenshot", class = "btn"),
          actionButton("save_las", "Save LAS", class = "btn"),
          actionButton("save_dtm", "Save DTM", class = "btn"),
          actionButton("save_ndsm", "Save nDSM", class = "btn"),
          actionButton("save_mask", "Save Mask", class = "btn"),
          downloadButton("downloadData", "Save All", class = "btn")
        )
      ),
      div(class = "main-panel",
        div(class = "main-content",
          tabsetPanel(
            tabPanel("Leaflet Map",leafletOutput("leafletmap", width = "70%", height = "63vh")),
              tabPanel("3D Plot", rglwidgetOutput("plot3D", width = "70%", height = "63vh")),
              tabPanel("2D Plot",  plotOutput("plot2D", width = "70%", height = "60vh")),
              tabPanel("Directory Data", tableOutput("plotmeta"))
            )
        ),
        div(class = "console-container", 
            uiOutput("console_output")
        )
      )
    )
)