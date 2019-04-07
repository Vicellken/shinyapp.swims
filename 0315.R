# Load required packages
library(DT)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinythemes)

# The database here needs pre-transform. See "data conversion" script
df <- readRDS("df_uniyear.rds")
# df1 <- read.csv("df1.csv", stringsAsFactors = FALSE)

# UI
ui <- navbarPage(title = "SWIMS marine biodiversity database", theme = shinytheme("readable"),
                 windowTitle = "SWIMS Database",
                 
                 tabPanel("Database",
                          DT::dataTableOutput("table")),
                 
                 tabPanel("Interactive Cluster Habitats",
                                     leafletOutput("habitat_map", width = 1200, height = 600)),
                 
                 tabPanel("Draw Habitats",
                                     leafletOutput("draw_map", width = 1200, height = 600)),
                 
                 tabPanel("Year slide map", leafletOutput("map", width = 1200, height = 600),
                                     absolutePanel(top = 10, right = 10, draggable = TRUE,
                                                   style="z-index:7000;",
                                                   tags$h3("Time slide map"),
                                                   sliderInput("Year", "Chronology:",
                                                               min(df$Year),
                                                               max(df$Year),
                                                               value = range(df$Year),
                                                               step = 1, animate = TRUE)
                                     )
                            )
)


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
                                                       text = 'Download')
                                                     )
                                      )
  )
  
  
  # The server function for leaflet map
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
        data = df, lng = ~longitude, lat = ~latitude, label = ~paste0(Name, "(", Year, ")"), popup = ~paste0(Name, " (", Year, ")" ), 
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
  
  # Add drawing function on Habitat map
  output$draw_map <- renderLeaflet({
    leaflet(df) %>% setView(lng = 114.109497, lat = 22.396428, zoom = 11) %>%
      addBootstrapDependency() %>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                       label = ~paste0(Habitat, " (", Year, ")" ),
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
    df %>%
      filter(Year >= input$Year[1] & Year <= input$Year[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(df) %>%
      addProviderTiles("CartoDB") %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  observe({
    leafletProxy("map", data = reactive_data_chrono()) %>%
      clearShapes() %>%
      addMarkers(lng = ~longitude, lat = ~latitude, layerId = ~id) %>%
      setView(lng = 114.154217, lat = 22.4045, zoom = 10)
  })
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
