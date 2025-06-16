# Run this script to start CF
source(file.path(paste0(getwd(), "/global.R")), local = TRUE)

# Define the UI and server
ui <- source(file.path("./ui.R"), local = TRUE)$value
server <- source(file.path("./server.R"), local = TRUE)$value

# Run the app
shiny::shinyApp(ui = ui, server = server)