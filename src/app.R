library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(sf)
library(leaflet.extras)
library(shinythemes)
library(shinycssloaders)
library(RColorBrewer)

# Provides an interactive browser-based tool for visualizing reactive dependencies and execution in your application.
# reactlog::reactlog_enable()

# Load the data
load("stations_df.RData")
load("bike_df.RData")

# Import the script plots.R to use the created plots in the Shiny dashboard
# source("../plots.R")
source("plots.R")
load("df.RData")

# Read the bike lane shape file
# nyc_bike_lanes <- read_sf("NYC_BICYCLE_NETWORK_19D_20210311.shp")
# # https://community.rstudio.com/t/projection-problems-with-leaflet/27747
# nyc_bike_lanes_transformed <- st_transform(nyc_bike_lanes, 4326)

# # Read the shape file with NYC's boroughs
# nyc_boroughs <- read_sf("nybb.shp")
# # https://community.rstudio.com/t/projection-problems-with-leaflet/27747
# nyc_boroughs_transformed <- st_transform(nyc_boroughs, 4326)


# Getting Time and grouping into daytime categories
df$onlytime <- as.POSIXct(substr(df$starttime, 12, 19), format="%H:%M:%S")
# Only Hours and Minutes
df$time <- strftime(df$onlytime,format="%H:%M")
# Only Hour
df$hour <- as.numeric(substr(df$time,1,2))
# Set Daytime
df$daytime  <- ifelse(df$onlytime <= as.POSIXct("06:00:00",format="%H:%M:%S"), 'night',
                                ifelse(df$onlytime > as.POSIXct("06:00:00",format="%H:%M:%S") & 
                                       df$onlytime <= as.POSIXct("12:00:00",format="%H:%M:%S"), 'morning',
                                ifelse(df$onlytime > as.POSIXct("12:00:00",format="%H:%M:%S") & 
                                       df$onlytime <= as.POSIXct("18:00:00",format="%H:%M:%S"), 'afternoon', 
                                ifelse(df$onlytime > as.POSIXct("18:00:00",format="%H:%M:%S") & 
                                       df$onlytime <= as.POSIXct("24:00:00",format="%H:%M:%S"), 'evening', 'night'))))
# Setting correct factor order
df$daytime <- factor(df$daytime,levels = c("morning","afternoon","evening","night"))

# Required for the interactive table
stations_df$id <- seq.int(nrow(stations_df))

# To make sure that the categories are in the intended order
stations_df$cat <- factor(stations_df$cat, levels = c("<5k", "5k-25k", "25k-100k","100k-250k", ">=250k"))

# There are two parts of the Shiny app: ui and server

# First the ui is created:

# navbarPage is the layout of the shiny app
# With tabPanel different tabs can be created
# In the ui part it is defined how the app looks like. Logic follows in the server part
ui <- navbarPage("Citi Bike NY", theme = shinytheme("paper"), id="main",
                 tabPanel(icon=icon("chart-line"),"Performance",
                          column(width=12,h5('Development of Citi Bike Rides'),
                                  plotOutput("timeseries") 
                          ),
                          column(width=4,h5('Hourly Distribution of Bike Rides by Week Day'),
                                 plotOutput("hourly_distribution")
                                 ),
                          column(width=4,h5('User Types'),
                                 plotOutput("usertypes")
                                 ),
                          column(width=4,h5('Distribution of Trip Duration Categories'),
                                 plotOutput("tripduration_hist")
                                 ),
                          column(width=4,h5('Effect of Precipitation on Trip Duration'),
                                 plotOutput('rain_tripduration_scatter')
                                 ),
                          column(width=4,h5('Customer Gender Ratio'),
                                 plotOutput('genderratio')
                                 ),
                          column(width=4,h5('Effect of daily Average Temperature on Trip Duration'),
                                 plotOutput('temp_scatter')),
                          ),  
                 tabPanel(icon=icon("charging-station"),"Stations",
                          column(width=8,
                                 leafletOutput("map01", height=600)),
                          column(width=4,
                                 DT::dataTableOutput("table01"))),                 
                 tabPanel(icon=icon("bicycle"),"Rides",
                          column(width=3,
                                 wellPanel(h5('Citi Bike Rides'),
                                             ("Use the filters to adjust the trips shown on the map. Please note that the data only shows a subset of the population.\n"),
                                   dateRangeInput(inputId = "daterange", label = "Date range:",
                                                  start = "2018-01-01",
                                                  end = "2019-12-31",
                                                  min = "2018-01-01",
                                                  max = "2019-12-31",
                                                  format = "dd.mm.yyyy",
                                                  separator = " - "),
                                   selectInput(inputId = "day.time",
                                               label = "Daytime",
                                               choices = c("All day", "morning", "afternoon", "evening", "night"),
                                               selected = c("All day")),
                                   selectInput(inputId = "user.type",
                                               label = "User Type",
                                               choices = c("All users", "Customer","Subscriber"),
                                               selected = "All users"),
                                   sliderInput(inputId = "temp",
                                               label = "Select a temperature range",
                                               min = -20, max = 40, value = c(-20,40), step = 1),
                                 ),
                                 wellPanel(
                                   h6("Number of observations shown on the map:"),
                                   textOutput("number")
                                 ),
                          ),
                          column(3,
                                 mainPanel(
                                   leafletOutput("citibikemap",
                                                 height=600, width=800)),
                          ),
                 ),
                 tabPanel(icon=icon("wrench"),"Maintenance", 
                          column(width=6, DT::dataTableOutput("bike_maintenance"))),
                 tabPanel(icon=icon("readme"),"Read Me",includeMarkdown("README-SHINY.md"))
             
)

# # Server logic
server <- function(input, output) {
  
  #################### Start of server part for tab "Performance"
  
  output$timeseries <- renderPlot({
    ts
  })
  
  output$hourly_distribution <- renderPlot({
    hourly_dist
  })
  
  output$usertypes <- renderPlot({
    usertype_plot
  })
  
  output$tripduration_hist <- renderPlot({
    tripdurations
  })

  output$rain_tripduration_scatter <- renderPlot({
    rain_scatter
  })

  output$genderratio <- renderPlot({
    genderratio_plot
  })

  output$temp_scatter <- renderPlot({
    temp_scatter
  })    
  
  ################### End of server part for tab "Performance"
  ###################
  
  ###################  
  ################### Start of server part for tab "Rides"
  
   ## Reactive part: Whenever an interactive element is used, the following code gets executed to filter the dataset.
   filtered_df <- reactive({
  
     res <- df %>%
       filter(startyear > input$daterange[1] & startyear < input$daterange[2])
  
     res <- res %>%
       filter(meantemp > input$temp[1] & meantemp < input$temp[2])
  
     if (input$day.time != "All day") {
       res <- res %>%
         filter(daytime == input$day.time)
     }
     if (input$user.type != "All users") {
       res <- res %>%
         filter(usertype == input$user.type)
     }
     res
   })
   

  # Interactive map: Depending on the filters used, it changes the number of observations displayed on the map.
  output$citibikemap <- renderLeaflet({
    leaflet(filtered_df()) %>%
      addTiles() %>%
      # addPolygons(data = nyc_bike_lanes_transformed, # Add bike lanes to map
      #             color = "#2c7fb8",
      #             fill = NA,
      #             weight = 1.5) %>%
      # addPolygons(data = nyc_boroughs_transformed, # Add polygons to highlight boroughs
      #             color = "#ff9900",
      #             fill = NA,
      #             weight = 2) %>%
      addCircleMarkers(lng = filtered_df()$start_station_longitude, lat = filtered_df()$start_station_latitude,
                       radius = 3,
                       popup = ~as.character(filtered_df()$content),
                       stroke = FALSE,
                       fillOpacity = 0.6,
                       clusterOptions = markerClusterOptions()) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addHeatmap(lng = filtered_df()$start_station_longitude,
                 lat = filtered_df()$start_station_latitude,
                 radius=8) %>%
      clearBounds()  # clears the bound, so that the view will be automatically determined by the
                     # range of latitude/longitude data in the map layers if provided
  })

  # To show the number of elements filtered respectively shown on the map
  output$number <- renderText({nrow(filtered_df())})
  
  ################### End of server part for tab "Rides"
  ###################
  
  ###################
  ################### Start of server part for tab "Stations"

  # Map which shows the stations and the number of bikes rented and returned
  output$map01 <- renderLeaflet({
    leaflet(data=stations_df) %>%
      addProviderTiles("CartoDB.Voyager") %>% 
      # addPolygons(data = nyc_bike_lanes_transformed, # Add bike lanes to map
      #             color = "#2c7fb8",
      #             fill = NA,
      #             weight = 1.5) %>%
      addCircleMarkers(layerId = as.character(stations_df$id),
                       lng = stations_df$Longitude, lat = stations_df$Latitude,
                       radius = 3,
                       stroke = FALSE,
                       color = ~pal(cat),
                       fillOpacity = 0.60) %>% 
      leaflet::addLegend(position = "bottomleft",
                pal = pal,
                values = stations_df$cat,
                title = "Bikes rented and returned") %>%
      clearBounds() # clears the bound, so that the view will be automatically determined by the
                    # range of latitude/longitude data in the map layers if provided
  })  
  
  # Data panel output in navbar tab "data"
  output$table01 <- renderDataTable({
    DT::datatable(
    stations_df[,c("Station_Name", "Bikes_rented", "Bikes_returned", "Total")],
    #filter = 'top',
    colnames = c("Station Name", "Bikes rented", "Bikes returned", "Total"),
    selection = "single",
    options = list(stateSave = TRUE))
  })
  

  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  # new icon style
  highlight_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'green', iconColor = 'white')
  
  observeEvent(input$table01_rows_selected, {
    row_selected = stations_df[input$table01_rows_selected, ]
    proxy <- leafletProxy('map01')
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$id),
                        lng = row_selected$Longitude,
                        lat = row_selected$Latitude,
                        icon = highlight_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row())){
      proxy %>%
        addCircleMarkers(layerId = as.character(prev_row()$id),
                         lng = prev_row()$Longitude, 
                         lat = prev_row()$Latitude,
                         radius = 3,
                         stroke = FALSE,
                         #color = ~pal(cat),
                         fillOpacity = 0.60)
    }
    # set new value to reactiveVal 
    prev_row(row_selected)
  })
  
  # Define color palette for legend and markers
  pal <- colorFactor(
    palette = 'RdYlGn',
    domain = stations_df$cat
  )
  
  observeEvent(input$map01_marker_click, {
    clickId <- input$map01_marker_click$id
    dataTableProxy("table01") %>%
      selectRows(which(stations_df$id == clickId)) %>%
      selectPage(which(input$table01_rows_all == clickId) %/% input$table01_state$length + 1)
  })
  
  ################### End of server part for tab "Stations"  
  ###################
  
  ###################
  ################### Start of server part for tab "Maintenance"  
  
  # Data panel output in navbar tab "data"
  output$bike_maintenance <- renderDataTable({
    DT::datatable(bike_df[,c("BikeId", "Usages", "TripDuration")],
    colnames = c("Bike Id", "Number of Usages", "Usage in Seconds"))})
  
  ################### End of server part for tab "Maintenance"  
  ###################  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
