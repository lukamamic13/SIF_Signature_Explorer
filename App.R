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
  summarise(mean_confidence_percentage = mean(confidence, na.rm = TRUE), .groups = "drop")

# Define color palette for climate
unique_climates <- unique(sif_signature$climate)
all_climates <- sort(unique(c(as.character(sif_signature$climate),
                              as.character(confidence_data$Climate))))
climate_palette <- paletteer::paletteer_c(
  "ggthemes::Temperature Diverging",
  length(all_climates)
)
climate_colors <- setNames(as.character(climate_palette), all_climates)

# Leaflet palette
climate_pal <- colorFactor(
  palette = climate_colors,
  domain  = names(climate_colors)
)


# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("SIF Signature Explorer"),
  
  # --- Publication (APA-style, label removed) ---
  fluidRow(
    column(
      width = 12,
      tags$div(
        style = "margin-bottom: 10px;",
        tags$p(
          "Mamić, L., Riches, M., Farmer, D. K., & Pirotti, F. (2025). ",
          "Solar-induced fluorescence as a robust proxy for vegetation productivity across climate zones and vegetation types in the United States. ",
          tags$em("Remote Sensing Applications: Society and Environment, 40"),
          ", 101760. ",
          tags$a("https://doi.org/10.1016/j.rsase.2025.101760",
                 href = "https://doi.org/10.1016/j.rsase.2025.101760",
                 target = "_blank")
        )
      )
    )
  ),
  
  # --- Tutorial button ---
  fluidRow(
    column(
      width = 12,
      align = "right",
      actionLink("show_tutorial", label = "Show tutorial",
                 icon = icon("info-circle"))
    )
  ),
  
  tabsetPanel(
    tabPanel("SIF Plot & Map", 
             sidebarLayout(
               sidebarPanel(
                 pickerInput(
                   "crop", "Select vegetation type(s):", 
                   choices  = sort(unique(sif_signature$crop)), 
                   # default: only first 2 vegetation types
                   selected = head(sort(unique(sif_signature$crop)), 2),  
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE, 
                     `live-search` = TRUE
                   )
                 ),  
                 
                 uiOutput("climate_ui"),  
                 
                 radioButtons(
                   "smooth_choice", "Choose SIF type:", 
                   choices = c("Weekly averages" = "raw",
                               "Smoothed weekly averages" = "smooth"), 
                   selected = "smooth"
                 ),
                 
                 radioButtons(
                   "variability_choice", "Choose variability:", 
                   choices = c("Standard Error (SE)" = "se",
                               "Standard Deviation (SD)" = "sd"), 
                   selected = "se"
                 ),
                 
                 downloadButton("download_data", "Download data", class = "btn-primary")  
               ),
               
               mainPanel(
                 plotlyOutput("sif_plot"),
                 leafletOutput("map")
               )
             )
    ),
    
    tabPanel("Confidence data", 
             DTOutput("confidence_table")
    ),
    
    # --- About / Methods tab with GitHub link ---
    tabPanel(
      "About",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          tags$br(),
          tags$h3("Overview"),
          tags$p(
            "This application visualises seasonal Solar-induced fluorescence (SIF) signatures ",
            "for different vegetation types and climate zones in the contiguous United States. ",
            "It accompanies the publication by Mamić et al. (2025) in ",
            tags$em("Remote Sensing Applications: Society and Environment"),
            "."
          ),
          
          tags$h4("Data sources"),
          tags$ul(
            tags$li("Satellite-based SIF at 743 nm, aggregated to weekly time steps."),
            tags$li("Vegetation type information derived from land cover / land use products."),
            tags$li("Climate classification based on gridded climate data (e.g., temperature and precipitation).")
          ),
          
          tags$h4("Computation of SIF signatures"),
          tags$ul(
            tags$li("SIF values were aggregated to weekly averages for each vegetation type–climate combination."),
            tags$li("Smoothed weekly averages were obtained by applying a temporal smoothing function to emphasise the seasonal trajectory."),
            tags$li("For each week of the year, the mean SIF and a variability metric (SE or SD) were computed.")
          ),
          
          tags$h4("Relationships between SIF, GPP and NDVI"),
          tags$p(
            "As described in the paper, the relationships between SIF, gross primary productivity (GPP) and NDVI ",
            "were evaluated across all vegetation types and climate zones. ",
            "The repository linked below contains figures and scripts that quantify and visualise ",
            "these relationships (GPP–SIF, SIF–NDVI, and GPP–NDVI) for the combinations used in this study."
          ),
          
          tags$h4("Code and data availability"),
          tags$p(
            "The code and data used to generate this application, as well as materials illustrating the ",
            "relationships between SIF, GPP and NDVI across vegetation types and climates, are available on GitHub: ",
            tags$a(
              "GitHub repository",
              href = "https://github.com/lukamamic13/SIF_Signature_Explorer/",
              target = "_blank"
            ),
            "."
          ),
          
          tags$h4("Variability and confidence"),
          tags$p(
            "The shaded ribbons in the SIF plots represent uncertainty around the mean SIF signal. ",
            "You can choose to display either the standard error (SE) or the standard deviation (SD) of SIF."
          ),
          tags$p(
            "The confidence scores summarised in the ",
            tags$strong("Confidence data"),
            " tab reflect the robustness of the SIF–productivity relationship for each vegetation type and climate zone. ",
            "Higher values indicate greater confidence that SIF provides a reliable proxy for vegetation productivity under those conditions."
          ),
          
          tags$h4("Usage notes"),
          tags$ul(
            tags$li("Results are intended for research and exploratory analysis."),
            tags$li("Interpretation should consider the underlying spatial and temporal resolution of the original datasets."),
            tags$li("For full methodological details, including model setup and validation, please refer to the published article.")
          ),
          
          tags$h4("Contact"),
          tags$p(
            "For questions, feedback, or potential collaboration, please contact the corresponding author at ",
            tags$strong("luka.mamic@uniroma1.it"), "."
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Show information & disclaimer modal on app start
  observe({
    showModal(modalDialog(
      title = "Information & disclaimer",
      HTML(
        paste0(
          "<b>Publication</b><br>
          This application accompanies the article:<br>
          Mamić, L., Riches, M., Farmer, D. K., &amp; Pirotti, F. (2025). 
          Solar-induced fluorescence as a robust proxy for vegetation productivity across climate zones and vegetation types in the United States. 
          <i>Remote Sensing Applications: Society and Environment, 40</i>, 101760. 
          <a href='https://doi.org/10.1016/j.rsase.2025.101760' target='_blank'>
          https://doi.org/10.1016/j.rsase.2025.101760</a><br><br>",
          
          "<b>Code and data</b><br>
          The code and data used for this application, and additional analyses of SIF–GPP–NDVI relationships, are available on GitHub: ",
          "<a href='https://github.com/lukamamic13/SIF_Signature_Explorer/' target='_blank'>GitHub repository</a>.<br><br>",
          
          "<b>Corresponding author</b><br>
          For questions or use in other applications, please contact:<br>
          <b>luka.mamic@uniroma1.it</b>."
        )
      ),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  # Tutorial modal
  observeEvent(input$show_tutorial, {
    showModal(modalDialog(
      title = "How to use the SIF Signature Explorer",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML(
        "<b>1. Select vegetation type(s)</b><br>
         Use the <i>Select vegetation type(s)</i> dropdown to choose one or more vegetation types. 
         The climate options update automatically based on your selection.<br><br>
         
         <b>2. Select climate(s)</b><br>
         Use the <i>Select climate(s)</i> dropdown to filter to specific climate zones. 
         The SIF signatures and the map will reflect your choices.<br><br>
         
         <b>3. Choose SIF type</b><br>
         <i>Weekly averages</i> show the mean weekly SIF signal, 
         while <i>Smoothed weekly averages</i> show a smoothed seasonal trajectory.<br><br>
         
         <b>4. Variability band</b><br>
         Toggle between <i>Standard Error (SE)</i> and <i>Standard Deviation (SD)</i> 
         to visualise the uncertainty around the mean SIF signal.<br><br>
         
         <b>5. Map panel</b><br>
         The map shows locations of sites used to compute SIF signatures. 
         Click a point to see vegetation type, climate, and confidence information.<br><br>
         
         <b>6. Download data</b><br>
         Use the <i>Download data</i> button to export the filtered SIF data 
         (based on your vegetation type and climate selections) as a CSV file.<br><br>
         
         <b>7. Confidence data tab</b><br>
         The <i>Confidence data</i> tab summarises average confidence scores per vegetation type and climate."
      )
    ))
  })
  
  # Dynamic climate UI based on selected vegetation types
  output$climate_ui <- renderUI({
    req(input$crop)  
    available_climates <- sif_signature %>% 
      filter(crop %in% input$crop) %>%  
      pull(climate) %>% 
      unique() %>%
      sort()
    
    pickerInput(
      "climate", "Select climate(s):",
      choices  = available_climates,
      selected = available_climates,  
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE, 
        `live-search` = TRUE
      )
    )
  })
  
  # Filtered SIF data based on inputs
  filtered_data <- reactive({
    req(input$crop, input$climate)  
    sif_signature %>% 
      filter(crop %in% input$crop, climate %in% input$climate)  
  })
  
  # Filtered confidence data based on inputs
  filtered_confidence_data <- reactive({
    req(input$crop, input$climate)  
    confidence_data %>% 
      filter(crop %in% input$crop, Climate %in% input$climate)  
  })
  
  # SIF plot
  output$sif_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)  
    
    sif_col <- ifelse(input$smooth_choice == "smooth", "smooth_sif", "mean_sif")
    variability_col <- ifelse(
      input$variability_choice == "se", 
      ifelse(input$smooth_choice == "smooth", "smooth_se", "se_sif"),
      ifelse(input$smooth_choice == "smooth", "smooth_sd", "sd_sif")
    )
    
    p <- ggplot(
      data, 
      aes(x = week, y = !!sym(sif_col), color = climate, fill = climate)
    ) +
      geom_ribbon(
        aes(
          ymin = !!sym(sif_col) - !!sym(variability_col), 
          ymax = !!sym(sif_col) + !!sym(variability_col),
          fill = climate
        ), 
        alpha = 0.3,
        show.legend = FALSE
      ) +
      geom_line(size = 1) +
      scale_x_continuous(
        breaks = seq(1, 52, length.out = 12),  
        labels = month.abb  
      ) +
      labs(
        title = "SIF signature for selected vegetation types and climates",
        x = "Month", 
        y = "SIF 743nm (mW/m2/sr/nm)"
      ) +
      theme_minimal() +
      theme(
        panel.border    = element_rect(colour = "black", fill = NA, size = 1),
        axis.text       = element_text(colour = "black"),
        axis.title      = element_text(colour = "black"),
        legend.position = "right",
        axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      scale_color_manual(values = climate_colors) +  
      scale_fill_manual(values = climate_colors) +  
      facet_wrap(~crop)
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Map
  output$map <- renderLeaflet({
    data <- filtered_confidence_data()
    req(nrow(data) > 0)
    
    data <- data %>%
      mutate(Climate = as.character(Climate))  # match palette domain
    
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 4,
        stroke = TRUE,
        weight = 1,
        opacity = 0.9,
        fillOpacity = 0.8,
        color = ~climate_pal(Climate),      # outline colour by climate
        fillColor = ~climate_pal(Climate),  # fill colour by climate
        popup = ~paste0(
          "<b>Vegetation type:</b> ", crop, "<br>",
          "<b>Climate:</b> ", Climate, "<br>",
          "<b>Confidence:</b> ", round(confidence, 1), "%"
        )
      )
    # no legend here on purpose
  })
  
  # Confidence table
  output$confidence_table <- renderDT({
    datatable(
      mean_confidence,
      colnames = c("Vegetation type", "Climate", "Mean confidence percentage"),
      options = list(
        pageLength = 10,
        scrollX    = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("SIF_data_selected_vegetation_types_climates.csv", sep = "")
    },
    content = function(file) {
      req(filtered_data())  
      write_csv(filtered_data(), file)
    }
  )
}

shinyApp(ui, server)

