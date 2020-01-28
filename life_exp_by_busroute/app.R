#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(RColorBrewer)
library(glue)
library(shinydashboard)
library(shiny)

stops_routes_joined2 <- st_read("shiny_life_exp_stop.geojson")

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        radioButtons("select_gender", 
                     "Select gender", 
                     c("male", "female"), 
                     selected = "female"),
        selectInput("select_route_id", 
                    "Route ID", 
                    unique(stops_routes_joined2$route_id))
    ),
    dashboardBody(
        h1("life expectancy along a bus route"),
        leafletOutput("bus_map"),
        tableOutput("table")
    )
)

server <- function(input, output) {

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

