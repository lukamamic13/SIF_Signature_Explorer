library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(paletteer)
library(readr)
library(DT)
library(leaflet)
library(ggthemes)
library(shinyjs)


# Load Data
sif_signature <- read_csv("data_final_signatures_20March.csv")
confidence_data <- read_csv("PointsForSignatures_March2025.csv")

# Compute Mean Confidence Percentage
mean_confidence <- confidence_data %>%
  group_by(crop, Climate) %>%
  summarise(mean_confidence_percentage = mean(confidence, na.rm = TRUE))

# Define color palette for climate
unique_climates <- unique(sif_signature$climate)
climate_palette <- paletteer::paletteer_c("ggthemes::Temperature Diverging", length(unique_climates))
climate_colors <- setNames(climate_palette, unique_climates)

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("SIF Signature Explorer"),
  
  tabsetPanel(
    tabPanel("SIF Plot & Map", 
             sidebarLayout(
               sidebarPanel(
                 pickerInput("crop", "Select Crop(s):", 
                             choices = unique(sif_signature$crop), 
                             selected = unique(sif_signature$crop),  
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE)),  
                 
                 uiOutput("climate_ui"),  
                 
                 radioButtons("smooth_choice", "Choose SIF Type:", 
                              choices = c("Raw Values" = "raw", "Smoothed Values" = "smooth"), 
                              selected = "raw"),
                 
                 radioButtons("variability_choice", "Choose Variability:", 
                              choices = c("Standard Error (SE)" = "se", "Standard Deviation (SD)" = "sd"), 
                              selected = "se"),
                 
                 downloadButton("download_data", "Download Data", class = "btn-primary")  
               ),
               
               mainPanel(
                 plotlyOutput("sif_plot"),
                 leafletOutput("map")
               )
             )
    ),
    
    tabPanel("Confidence Data", 
             DTOutput("confidence_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Show disclaimer modal on app start
  observe({
    showModal(modalDialog(
      title = "Disclaimer",
      HTML("This work is currently under review for the journal <i>Remote Sensing Applications: Society and Environment</i>.<br>
           For any use or citation, please contact the corresponding author: <b>luka.mamic@uniroma1.it</b>."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  output$climate_ui <- renderUI({
    req(input$crop)  
    available_climates <- sif_signature %>% 
      filter(crop %in% input$crop) %>%  
      pull(climate) %>% 
      unique()
    
    pickerInput(
      "climate", "Select Climate(s):",
      choices = available_climates,
      selected = available_climates,  
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  filtered_data <- reactive({
    req(input$crop, input$climate)  
    sif_signature %>% 
      filter(crop %in% input$crop, climate %in% input$climate)  
  })
  
  filtered_confidence_data <- reactive({
    req(input$crop, input$climate)  
    confidence_data %>% 
      filter(crop %in% input$crop, Climate %in% input$climate)  
  })
  
  output$sif_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)  
    
    sif_col <- ifelse(input$smooth_choice == "smooth", "smooth_sif", "mean_sif")
    variability_col <- ifelse(input$variability_choice == "se", 
                              ifelse(input$smooth_choice == "smooth", "smooth_se", "se_sif"),
                              ifelse(input$smooth_choice == "smooth", "smooth_sd", "sd_sif"))
    
    p <- ggplot(data, aes(x = week, y = !!sym(sif_col), color = climate, fill = climate)) +
      
      geom_ribbon(aes(ymin = !!sym(sif_col) - !!sym(variability_col), 
                      ymax = !!sym(sif_col) + !!sym(variability_col), fill = climate), alpha = 0.3) +
      
      geom_line(size = 1) +
      
      scale_x_continuous(
        breaks = seq(1, 52, length.out = 12),  
        labels = month.abb  
      ) +
      
      labs(title = "SIF Signature for Selected Crops and Climates",
           x = "Month", 
           y = "SIF 743nm (mW/m2/sr/nm)") +
      
      theme_minimal() +
      theme(
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      
      scale_color_manual(values = climate_colors) +  
      scale_fill_manual(values = climate_colors) +  
      facet_wrap(~crop)  
    
    ggplotly(p)
  })
  
  output$map <- renderLeaflet({
    data <- filtered_confidence_data()
    req(nrow(data) > 0)
    
    leaflet(data) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~climate_colors[Climate],
        fillOpacity = 0.7, radius = 5,
        popup = ~paste("Crop:", crop, "<br>Climate:", Climate, "<br>Confidence:", round(confidence, 2))
      )
  })
  
  output$confidence_table <- renderDT({
    datatable(mean_confidence, options = list(pageLength = 10))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("SIF_data_selected_crops_climates.csv", sep = "")
    },
    content = function(file) {
      req(filtered_data())  
      write_csv(filtered_data(), file)
    }
  )
}

shinyApp(ui, server)
