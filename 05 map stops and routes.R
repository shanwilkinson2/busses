# map stops & routes

# load packages
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(leaflet)
  library(RColorBrewer)
  library(glue)
  library(shinydashboard)
  library(htmlwidgets)
  library(shiny)

  
# read in files
  stops <- st_read("stops_extradata.geojson")
  stops_routes <- st_read("joined_stops_routes.geojson") 

# In/ out/ different operators have different numbers of stops
  stops_routes %>%
    st_drop_geometry() %>%
    filter(route_short_name ==575) %>%
    group_by(route_id) %>%
    summarise(n())

# join stops with stops with life expectancy added
  stops_routes_joined <- 
    left_join(stops_routes, stops_life_exp, by = "stop_id") %>%
    mutate(stop_sequence = as.numeric(stop_sequence))
  
# make one life expectancy column instead of two so can filter out unwanted one
  # was struggling with switching between columns for colouring & label
  stops_routes_joined2 <- stops_routes_joined %>%
    gather(key = "life_exp_gender", value = "life_exp_val", 
           c(male_life_exp:female_life_exp)) %>%
    mutate(life_exp_gender = recode(life_exp_gender, "male_life_exp" = "male", 
                                    "female_life_exp" = "female")) %>%
    select(route_id, agency_id:route_long_name, stop_id, stop_sequence, stop_name:life_exp_val)
  
  st_write(stops_routes_joined2, "shiny_life_exp_stop.geojson")
  
############# shiny dashboard ############

  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      radioButtons("select_gender", 
                   "Select gender", 
                   c("male", "female"), 
                   selected = "female"),
      selectInput("select_route_id", 
                  "Route ID", 
                  unique(stops_routes$route_id))
    ),
    dashboardBody(
      h1("life expectancy along a bus route"),
      leafletOutput("bus_map"),
      tableOutput("table")
    )
  )
  
  server <- function(input, output) {
  
  # selected gender
  # breaks the rest!!!
    output$which_gender <- reactive({
      print(input$select_gender)
    })
      
  # reactive dataset for map & table  
    selected_data <- reactive({
      stops_routes_joined2 %>%
        filter(route_id == input$select_route_id & life_exp_gender == input$select_gender) %>%
        select(route_short_name, agency_id, route_long_name, stop_name, 
               stop_sequence, life_exp_gender, life_exp_val)
    })
    
    # interactive map
       # domain = range of values  
    life_exp_pal <- colorNumeric(palette = "BuPu", 
                                 domain = c(min(stops_routes_joined2$life_exp_val, na.rm = TRUE),
                                            max(stops_routes_joined2$life_exp_val, na.rm = TRUE)), 
                                 reverse = TRUE)
      
      
      output$bus_map <- renderLeaflet(
        selected_data() %>%
        leaflet() %>%  
        addProviderTiles("Stamen.TonerLite") %>%
        addCircleMarkers(radius = 8, 
                         fillColor = ~life_exp_pal(life_exp_val),
                         popup = ~glue("route number: {route_short_name} operator: {agency_id}<br>
                    {stop_name}<br>
                    {life_exp_gender} life expectancy {life_exp_val}"), 
                    weight = 2, fillOpacity = 0.8, color = "black") %>%
          addLegend("bottomleft", pal = life_exp_pal, values = ~life_exp_val,
                    labFormat = labelFormat(digits = 0), title = "Life expectancy",
                    opacity = 1) %>%
          addControl("click on a bus stop to find out more", 
                     position = "topright")
      )
     
    # table
      output$table <- renderTable({
        selected_data() %>%
        st_drop_geometry() %>%
        select(route_short_name, route_long_name, 
               stop_name, life_exp_gender, life_exp_val) %>%
        arrange(life_exp_val)
      })
  }
  
  shinyApp(ui, server)
  
