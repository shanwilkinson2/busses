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
    dashboardHeader(title = "Life expectancy along bus routes",
                    titleWidth = 330),
    dashboardSidebar(
      sidebarMenu(id="tabs",
                  sidebarMenuOutput("menu")
      ),
      radioButtons("select_gender", 
                   "Select gender", 
                   c("male", "female"), 
                   selected = "female"),
      selectInput("select_route_id", 
                  "Select route:", 
                  unique(stops_routes_joined2$route_short_long_name))
    ),
    dashboardBody(
        tabItems(
          # map tab
          tabItem(tabName = "map",
            leafletOutput("bus_map"),
            tableOutput("table")
          ),
          # details & sources tab
          tabItem(tabName = "details",
            h2("Further information"),
            p("Code for this app is on my github"),
            a("Github", href="https://github.com/shanwilkinson2/busses", target = "_blank"),
            br(),
            p("Bus data from Open Data Manchester"),
            a("Open Data Manchester Bus Fare Dropbox", href = "https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0", 
               target = "_blank"),
            br(),
            p("Life expectancy data from Public Health England Fingertips. Life expectancy at birth 
              used, at Middle Super Output Area (MSOA) for 2019."),
            a("PHE Fingertips", href = "https://fingertips.phe.org.uk/search/life%20expectancy#page/0/gid/1/pat/101/par/E08000001/ati/3/are/E02000984", 
              target = "_blank")
          )
        )
  )
)

server <- function(input, output, session) {

  # render sidebar menu - needed if reactive stuff on page
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map-signs")),
      menuItem("Details", tabName = "details", icon = icon("comment-dots"))
    )
  })
  isolate({updateTabItems(session, "tabs", "m1")})
  
    # reactive dataset for map & table  
    selected_data <- reactive({
        stops_routes_joined2 %>%
            filter(route_short_long_name == input$select_route_id & life_exp_gender == input$select_gender) %>%
            select(route_short_name, agency_id, route_long_name, route_short_long_name, stop_name, 
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
                             popup = ~glue("<b>Route:</b> {route_short_name} {route_long_name}<br>
                                            <b>Stop:</b> {stop_name}<br>
                                <b>{str_to_sentence(life_exp_gender)} life expectancy:</b> {life_exp_val}"), 
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
            ungroup() %>%
            select(stop_name, life_exp_gender, life_exp_val) %>%
            filter(life_exp_val == max(life_exp_val, na.rm = TRUE) | 
                       life_exp_val == min(life_exp_val, na.rm = TRUE)) %>%
            arrange(life_exp_val)
    })
}

shinyApp(ui, server)

