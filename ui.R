library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

ui <- navbarPage(
  title = "CloudFlux (CF)",
  
  # Move JavaScript and styling to the header
  header = tagList(
    useShinyjs(),  # Enable JavaScript features
    tags$head(
      tags$style(HTML("
        /* Improved Button Layout */
        .button-container { 
          display: grid; 
          grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); 
          gap: 10px; 
          align-items: center; /* Center buttons vertically */
        }
        .button-container .btn { width: auto; min-width: 150px; height: 40px; } /* Ensure fixed height */
      
        /* Ensure 'Confirm Point Cloud' button is on a new row */
        .confirm-container {
          display: flex;
          justify-content: center; /* Center horizontally */
          width: 100%;
          margin-top: 10px;
        }
        
          /* Improved Dark Mode Toggle */
        .dark-mode-toggle {
          position: absolute;
          top: 10px;
          right: 15px;
          transform: scale(0.8);  /* Reduce button size */
        }
        
          #console_output {
            max-height: 300px;  
            overflow-y: auto;   
            background: #f8f9fa;  
            padding: 10px;
            border: 1px solid #ccc;  
            white-space: pre-wrap; 
            color: black; /* Default text color */
          }
        
           /* Default Light Mode Styles */
          body {
            background-color: #ffffff;
            color: #000000;
          }
        
          /* Dark Mode Styles */
          body.dark-mode {
            background-color: #121212;  /* Dark background */
            color: #ffffff;  /* Light text */
          }
        
          /* Dark Mode - Sidebar */
          body.dark-mode .well {
            background-color: #1e1e1e;
            color: #ffffff;
            border-color: #444;
          }
        
          /* Dark Mode - Navbar */
          body.dark-mode .navbar-default {
            background-color: #222;
            border-color: #333;
          }
        
          /* Dark Mode - Main Panel */
          body.dark-mode .tab-content {
            background-color: #1e1e1e;
            color: #ffffff;
          }
        
          /* Dark Mode - Input Fields */
          body.dark-mode input, 
          body.dark-mode textarea, 
          body.dark-mode select {
            background-color: #333;
            color: white;
            border-color: #555;
          }
        
          /* Dark Mode - Buttons */
          body.dark-mode .btn {
            background-color: #444;
            color: white;
            border-color: #666;
          }
          
          body.dark-mode .btn:hover {
            background-color: #666;
            border-color: #888;
          }
        
          /* Dark Mode - Console Output */
          body.dark-mode #console_output {
            background: #222;
            color: white;
            border-color: #555;
          }
            
        /* Ensure proper button alignment */
        .button-container select, .button-container .btn {
          align-self: center;
        }
      ")),
      
      tags$script(HTML("
      $(document).ready(function() {
        $('#dark_mode').on('change', function() {
          if ($(this).prop('checked')) {
            $('body').addClass('dark-mode');
          } else {
            $('body').removeClass('dark-mode');
          }
        });
    "))
    )
  ),
  
  # First tab: Introduction
  tabPanel(
    title = "Introduction",
    fluidPage(
      h2("Welcome to CloudFlux (CF)"),
      tags$div(
        style = "text-align: justify;", # Ensures the text fills the container and is justified
        "CF is designed to visualize, process, and analyze point cloud data. It can ingest LAS or LAZ formats. CF will create Digital Terrain Models (DTMs) and normalized Digital Surface Models (nDSM) and align point clouds for change detection. User data uploaded to CF is not saved and does not persist in the application. CF is free for use and it was built on the efforts of the open-source and open-access communities.", tags$br(),
        tags$br(),
        tags$b("Disclaimer:"),"CF is an educational tool and is not intended to replace professional advice or certified data processing workflows.The outputs are provided 'as is,' with no guarantee of accuracy, completeness, or suitability for any specific purpose. The developers of CF are not liable for any errors, inaccuracies, or decisions made based on its use. Use at your own risk.", tags$br(),
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
  ),
  
  # Second tab: CloudFlux UI
  tabPanel(
    title = "CloudFlux",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # Dark Mode Toggle
          div(class = "dark-mode-toggle",
              switchInput("dark_mode", label = NULL, value = FALSE, size = "small")
          ),
          
          fileInput("upload_file", "Upload Point Cloud (.laz or .las)", accept = c(".laz", ".las")),
          textInput("in_dir", "Input directory:", value = paste0(getwd(), "/data/")),
          textInput("out_dir", "Output directory:", value = paste0(getwd(), "/saves/")),
          numericInput("resolution", "Resolution:", value = 1),
          numericInput("crs", "CRS:", value = 26917),
          actionButton("confirm", "Confirm Inputs"),
          bsTooltip("confirm", "Ensure you are selecting the appropriate CRS and resolution."),
          tags$hr(),
          
          h4("Data Processing"),
          div(class = "button-container",
              selectInput("selected_source", "Select Source", choices = NULL),
              selectInput("selected_target", "Select Target", choices = NULL),
              selectInput("selected_buildings", "Select Footprints (.shp)", choices = NULL)
          ),
          
          # Ensure the button is always on a new row
          div(class = "confirm-container",
              actionButton("PC_confirm", "Confirm Point Cloud", class = "btn")
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
          selectInput("selected_processing", "Raster Type to Align", choices = c("","DTM", "nDSM")),
          div(class = "button-container",
              actionButton("align_rasters", "Align Rasters"),
              bsTooltip("align_rasters", "Alignment is required to compare the rasters"),
              actionButton("classify_raster", "Classify Rasters"),
              bsTooltip("classify_raster", "Classify rasters to identify changes in the landscape"),
              tags$hr()
          ),
          
          h4("Data Saving"),
          selectInput("io_obj", "Select PC to Save", choices = NULL),
          div(class = "button-container",
              actionButton("save_las", "Save LAS"),
              actionButton("save_dtm", "Save DTM"),
              actionButton("save_ndsm", "Save nDSM"),
              actionButton("save_mask", "Save Mask"),
              actionButton("save_SC", "Save Spatial Containers"),
              downloadButton("downloadData", "Save All")
          ),
          
          h4("Plotting"),
          div(class = "button-container",
              actionButton("plot_source", "Plot Source Las"),
              actionButton("plot_target", "Plot Target Las"),
              actionButton("plot_leaf", "Plot to Leaflet"),
              actionButton("plot_results", "Plot Results")
          )
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Leaflet Map", leafletOutput("leafletmap", height = "500px")),
            tabPanel("Directory Data", tableOutput("plotmeta")),
            tabPanel("3D Plot", rglwidgetOutput("plot3D", width = "100%", height = "500px")),
            tabPanel("2D Plot", plotOutput("plot2D"))
          ),
          uiOutput("console_output")
        )
      )
    )
  )
)

