# Load required packages
library(shiny)
library(DT)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shinythemes)

# The database here needs pre-transform. See "data conversion" script
# The 'df1' here is an unsorted data
df <- readRDS("df.rds")
df1 <- read.csv("df1.csv", stringsAsFactors = FALSE)

# UI
ui <- navbarPage(title = "SWIMS marine biodiversity database", theme = shinytheme("readable"),
                 windowTitle = "SWIMS Database",
                 
                 tabPanel("Description",
                          h5("This marine biodiversity database is copyrighted by SWIMS at the University of Hong Kong.",
                             align = "center"),
                          br(), br(),
                          h4("The Hong Kong coastal area is enrich in biodiversity. Hong Kong only contribute ~0.07% of the 
                             South China Sea but it hosts ~26% of all marine fish known in the region. In 2014, the Swire Institute of Marine Science (SWIMS) at the Univeristy
                             of Hong Kong produced a regional species database, which is essential for marine resource monitoring and management.", align = "center"),
                          br(), br(),
                          h4("This website is aim to provide the advanced data management services for SWIMS internal use.
                             Combining with database management function and Geographic Information System (GIS), this website facilitates the database usage and visualises the database from several perspectives.", 
                             align = "center"),
                          br(), br(),
                          h4("In addition to a interactive platform, this website is created by R. The R scripts are reproducible for further utilisation.", align = "center"),
                          br(), br(),
                          h5("Produced by MSc ENVM students at HKU:", align = "right"),
                          tags$img(src = "logo.png", height = 108, width = 372, align = "right")),
                 
                 tabPanel("Database",
                          DT::dataTableOutput("table")),
                 
                 navbarMenu("Data Plotting",
                            tabPanel("Total Density",
                                     leafletOutput("density_map", width = 1200, height = 600)),
                            
                            tabPanel("Interactive Cluster Habitats",
                                     leafletOutput("habitat_map", width = 1200, height = 600)),
                            
                            tabPanel("Draw Habitats",
                                     leafletOutput("draw_map", width = 1200, height = 600)),
                            
                            tabPanel("Year slide map", leafletOutput("map", width = 1200, height = 600),
                                     absolutePanel(top = 10, right = 10, draggable = TRUE,
                                                   style="z-index:7000;",
                                                   tags$h3("Year slide map"),
                                                   sliderInput("Sampling_year", "Chronology:",
                                                               min(df1$Sampling_year),
                                                               max(df1$Sampling_year),
                                                               value = range(df1$Sampling_year),
                                                               step = 1, animate = TRUE)
                                     )
                            )
))


# Server
server <- function(input, output) {
  
  # Function for database
  output$table <- DT::renderDataTable(df, server = FALSE, rownames = FALSE,
                                      extensions = 'Buttons', 
                                      options = list(dom = 'Bfrtip',
                                                     pageLength = 20,
                                                     buttons = list('copy', 'print', list(
                                                       extend = 'collection',
                                                       buttons = c('csv', 'excel', 'pdf'),
                                                       text = 'Download'
                                                     )))
                                      )
  
  # The server function for leaflet map
  # Here is the density map
  output$density_map <- renderLeaflet({
    leaflet(df) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(lng = ~Lon, lat = ~Lat, 
                       label = ~paste0(Habitat, " (", Sampling_year, ")" ),
                       color = '#FF0000', radius = 0.2) %>%
      addHeatmap(lng = ~Lon, lat = ~Lat, radius = 10) %>%
      setView(lng = 114.154217, lat = 22.4045, zoom = 11) %>%
      addResetMapButton()
      
  })
  
  # Here is the habitat cluster map
  output$habitat_map <- renderLeaflet({
      leaflet(df) %>%
      
      # Base Groups and Multiple Map Tiles
      addTiles(group = "OSM") %>%
      addProviderTiles("CartoDB", group = "CartoDB") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri") %>%
      
      addMarkers(lng = 114.2896, lat = 22.51589, label = "Yan Chau Tong Marine Park", 
                 labelOptions = labelOptions(noHide = TRUE, textsize = "15px", direction = "bottom"), group = "Yan Chau Tong Marine Park") %>%
      
      addMarkers(lng = 114.4283, lat = 22.54295, label = "Tung Ping Chau Marine Park", 
                 labelOptions = labelOptions(noHide = TRUE, textsize = "15px", direction = "bottom"), group = "Tung Ping Chau Marine Park") %>%
      
      addMarkers(lng = 113.9753, lat = 22.33383, label = "The Brothers Marine Park", 
                 labelOptions = labelOptions(noHide = TRUE, textsize = "15px", direction = "top"), group = "The Brothers Marine Park") %>%
      
      addMarkers(lng = 113.8825, lat = 22.37902, label = "Sha Chau and Lung Kwu Chau Marine Park", 
                 labelOptions = labelOptions(noHide = TRUE, textsize = "15px", direction = "top"), group = "Sha Chau and Lung Kwu Chau Marine Park") %>%
      
      addMarkers(lng = 114.3556, lat = 22.47368, label = "Hoi Ha Wan Marine Park", 
                 labelOptions = labelOptions(noHide = TRUE, textsize = "15px", direction = "right"), group = "Hoi Ha Wan Marine Park") %>%
      
      addMarkers(lng = 114.2602, lat = 22.20818, label = "Cape D'Aguilar Marine Reserve", 
                 labelOptions = labelOptions(noHide = TRUE, textsize = "15px", direction = "bottom",
                                             style = list(
                                               "color" = "red",
                                               "font-family" = "serif",
                                               "font-style" = "italic",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = "18px",
                                               "border-color" = "rgba(0,0,0,0.5)")), group = "Cape D'Aguilar Marine Reserve") %>%
      
      addLayersControl(baseGroups = c("OSM", "CartoDB", "Esri"), 
                       overlayGroups = c("Yan Chau Tong Marine Park", "Tung Ping Chau Marine Park", "The Brothers Marine Park", "Sha Chau and Lung Kwu Chau Marine Park", "Hoi Ha Wan Marine Park", "Cape D'Aguilar Marine Reserve", "Cluster"),
                       position = "topright",
                       options = layersControlOptions(collapsed = TRUE)) %>%
      setView(lng = 114.109497, lat = 22.396428, zoom = 11) %>%
      addResetMapButton() %>%
      addMarkers(
        data = df, lng = ~Lon, lat = ~Lat, label = ~paste0(Name, "(", Sampling_year, ")"), popup = ~paste0(Name, " (", Sampling_year, ")" ), 
        icon = makeIcon(
          iconWidth = 1, iconHeight = 1
        ), group = "Cluster",
        clusterOptions = markerClusterOptions(riseOnHover = TRUE, opacity = 0.75,
                                              iconCreateFunction=JS("function (cluster) {    
                                                              var childCount = cluster.getChildCount(); 
                                                                                   var c = ' marker-cluster-';  
                                                                                   if (childCount < 134) {  
                                                                                   c += 'small';  
                                                                                   } else if (childCount < 1000) {  
                                                                                   c += 'medium';  
                                                                                   } else { 
                                                                                   c += 'large';  
                                                                                   }    
                                                                                   return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
                                                                                   }"))
        ) %>%
      addSearchFeatures(targetGroups = "Cluster", options = searchFeaturesOptions(
        zoom=23, openPopup = TRUE, firstTipSubmit = TRUE,
        autoCollapse = TRUE, hideMarkerOnCollapse = TRUE
      ))
  })
  
  # Add Draw Habitat map
  output$draw_map <- renderLeaflet({
    leaflet(df) %>% setView(lng = 114.109497, lat = 22.396428, zoom = 11) %>%
      addBootstrapDependency() %>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(lng = ~Lon, lat = ~Lat, 
                       label = ~paste0(Habitat, " (", Sampling_year, ")" ),
                       color = '#FF0000', radius = 0.2, group = "Cluster",
                       clusterOptions = markerClusterOptions(riseOnHover = TRUE, opacity = 0.75,
                                                             iconCreateFunction=JS("function (cluster) {    
                                                                                   var childCount = cluster.getChildCount(); 
                                                                                   var c = ' marker-cluster-';  
                                                                                   if (childCount < 134) {  
                                                                                   c += 'small';  
                                                                                   } else if (childCount < 1000) {  
                                                                                   c += 'medium';  
                                                                                   } else { 
                                                                                   c += 'large';  
                                                                                   }    
                                                                                   return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
  }"))) %>%
      addDrawToolbar(
        targetGroup = "Draw", editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      addMeasurePathToolbar(options = measurePathOptions(imperial = TRUE, minPixelDistance = 100, showDistances = TRUE)) %>%
      addLayersControl(overlayGroups = c("Draw"), options = 
                         layersControlOptions(collapsed = FALSE)) %>%
      addStyleEditor()
    
  })
  
  # Add Time Slider map
  reactive_data_chrono <- reactive({
    df1 %>%
      filter(Sampling_year >= input$Sampling_year[1] & Sampling_year <= input$Sampling_year[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(df1) %>%
      addProviderTiles("CartoDB") %>%
      fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
  })
  
  observe({
    leafletProxy("map", data = reactive_data_chrono()) %>%
      clearShapes() %>%
      addMarkers(lng = ~Lon, lat = ~Lat, layerId = ~id) %>%
      setView(lng = 114.154217, lat = 22.4045, zoom = 10)
  })
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
