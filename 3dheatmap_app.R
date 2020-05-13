# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(shiny)
library(d3heatmap)
library(tidyverse)
library(lattice)

# Load data ---------------------------------------------------------------
joined_data_aug <- read_csv(file = "data/02_joined_data_PAM50_aug.csv")


# Wrangle data ---------------------------------------------------------------
heatmap_mtx <- joined_data_aug %>% 
  column_to_rownames(var = "patient_ID") %>% 
  select(starts_with("NP_")) %>% 
  data.matrix(.)


# Shiny App ---------------------------------------------------------------

# USER INTERFACE ------------
# we use two wrapper functions to implement d3heatmap in shiny ->
#     d3heatmapOutput - > create a UI element whenever the app runs
#     renderD3heatmap - > render the actual heatmap (used later in the server seccion)
ui <- fluidPage(
  
  # Add title
  titlePanel("Expression of PAM50 protein isoforms in breast cancer data."),
  
  # Give an interesting theme to your app
  # theme = shinytheme("united"),
  
  # Sidebar layout, elements: side panel + main panel
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: check if clustering is needed or not
      checkboxInput(inputId = "cluster", label = "Apply clustering")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: d3heatmap
      d3heatmapOutput(outputId = "heatmap", width = "100%", height = "700px")
    )
  )
)


# SERVER -------------------
server <- function(input, output) {
  output$heatmap <- renderD3heatmap({d3heatmap(heatmap_mtx,
                                              colors = "RdYlBu",
                                              dendrogram = if(input$cluster) "both" else "none")})
}

# CALL SHINY APP -----------
shinyApp(ui = ui, server = server)