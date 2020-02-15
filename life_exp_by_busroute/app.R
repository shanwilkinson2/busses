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
library(shinyjs)

# dataset
  stops_routes_joined2 <- st_read("shiny_life_exp_stop.geojson")

# CSS for styling of download button
my_css <- "
  #bttn_data {
  background: grey;
  color: white;
  font-size: 16px;
  }
  "

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
      # selectize = TRUE makes it so you can type in
      selectInput(inputId = "select_route_id", 
                  label = "Select route:", 
                  choices = sort(unique(stops_routes_joined2$route_short_long_name)),
                  selectize = TRUE
                  ),
      em("(delete selected route to type & search)")
    ),
    dashboardBody(
      tags$style(my_css),
        tabItems(
          # map tab
          tabItem(tabName = "map",
            # box(title = "Map",
            #     status = "primary",
            #     width = 12,
            #     solidHeader = TRUE,
            leafletOutput("bus_map"),
            # ),
            box(title = "Life expectancy on this route",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                infoBoxOutput("max_le"),
                infoBoxOutput("min_le"),
                infoBoxOutput("diff_le")
                ),
            downloadButton("bttn_data", "Get the data (csv)")
            ),
          # tab with table of all data
          tabItem(tabName = "data",
            DT::DTOutput("table")
          ),
          # details & sources tab
          tabItem(tabName = "details",
            h2("Further information"),
            p("Code for this app is on my github"),
            a("Github", href="https://github.com/shanwilkinson2/busses", target = "_blank"),
            br(),
            br(),
            p("Bus data from Open Data Manchester"),
            a("Open Data Manchester Bus Fare Dropbox", href = "https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0", 
               target = "_blank"),
            br(),
            br(),
            p("Life expectancy data from Public Health England Fingertips, for 2019."),
            p("Life expectancy at birth used, for the Middle Super Output Area (MSOA) in which the bus stop is located."),
            p("Life expectancy can be used as a general measure of overall population health."),
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
      menuItem("Explore all data", tabName = "data", icon = icon("table")),
      menuItem("Details", tabName = "details", icon = icon("comment-dots"))
    )
  })
  # makes the menu not update itself when other reactive stuff changes
  isolate({updateTabItems(session, "menu", "map")})

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
    
    # map
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
    
    # max life expectancy info box
    output$max_le <- renderInfoBox({
      infoBox(
        "HIGHEST", 
        paste(round(
          max(selected_data()$life_exp_val, na.rm = TRUE)), 
              "years"), 
        icon = icon("arrow-circle-up"),
        color = "light-blue"
      )
    })
    
    # min life expectancy info box
    output$min_le <- renderInfoBox({
      infoBox(
        "LOWEST", 
        paste(round(
          min(selected_data()$life_exp_val, na.rm = TRUE)),
          "years"), 
        icon = icon("arrow-circle-down"),
        color = "light-blue"
      )
    })
    
    # difference in life expectancy info box
    output$diff_le <- renderInfoBox({
      infoBox(
        "DIFFERENCE", 
        paste(round(
          max(selected_data()$life_exp_val, na.rm = TRUE) - min(selected_data()$life_exp_val, na.rm = TRUE) 
          ), 
              "years"), 
        icon = icon("exchange-alt"),
        color = "yellow"
      )
    })
    
    # generate data for download button
    output$bttn_data <- downloadHandler(filename = "bus_life_exp.csv",
        # create file for downloading
        content = function(file){
          write.csv(selected_data() %>%
                      st_drop_geometry()
                    , file)
        })
    
    # table
    output$table <- DT::renderDT({
      data = stops_routes_joined2 %>%
        st_drop_geometry() %>%
        select("Route number" = route_short_name, "Stop name" = stop_name, 
               "Gender" = life_exp_gender, "Life expectancy" = life_exp_val)
    }, filter = "top")
}

shinyApp(ui, server)

