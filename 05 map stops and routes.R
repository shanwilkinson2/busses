# map stops & routes

# load packages
  library(sf)
  library(dplyr)
  library(leaflet)
  library(RColorBrewer)
  library(glue)
  library(shinydashboard)
  library(htmlwidgets)
  library(shiny)

  
# read in files
  stops <- st_read("stops_extradata.geojson")
  stops_routes <- st_read("joined_stops_routes.geojson") 

 # was going to filter out different operatorts/ in/ out route
    # but different numbers of stops.
  stops_routes %>%
    st_drop_geometry() %>%
    filter(route_short_name ==575) %>%
    group_by(route_id) %>%
    summarise(n())

# join
  stops_life_exp <- stops %>%
    st_drop_geometry() %>%
    select(stop_id, male_life_exp, female_life_exp)

  # test one route only
  stops_routes_short <- stops_routes %>%
    filter(route_id == "NOR: 575:O:") # 42 stops
  stops_routes_short <- stops_routes %>%
    filter(route_id == "GTB: 575:O:") # 27 stops
  stops_routes_short <- stops_routes %>%
    filter(route_id == "GMN: 501:O:")
  
  
  # join
  stops_routes_short <- 
    left_join(stops_routes_short, stops_life_exp, by = "stop_id")
  
  stops_routes_joined <- 
    left_join(stops_routes, stops_life_exp, by = "stop_id")

# map life expectancy along route selected above
  # palette reversed so dark = low to emphasise low rather than high 
  
  pal <- colorNumeric(palette = "YlOrRd", domain = stops_routes_short$female_life_exp, reverse = TRUE)
  mylabel <- glue("route number: {stops_routes_short$route_short_name} operator: {stops_routes_short$agency_id}<br>
                  {stops_routes_short$stop_name}<br>
                  Female life expectancy: {stops_routes_short$female_life_exp}")
  
life_exp_map <-  leaflet(stops_routes_short) %>%  
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(radius = 5, fillColor = ~pal(female_life_exp), 
    popup = ~mylabel, weight = 2, fillOpacity = 0.8, color = "black") 

saveWidget(life_exp_map, "life_exp_map.html")
  
############# shiny dashboard ############

  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      varSelectInput("select_gender", "Gender:", 
                     data = st_drop_geometry(stops_routes_short) %>%
                       select(male_life_exp, female_life_exp),
                  c("Male" = "male_life_exp", "Female" = "female_life_exp")),
      selectInput("select_route_id", "Route ID", unique(stops_routes$route_id))
    ),
    dashboardBody(
      h1("Life expectancy along a bus route"),
      # leafletOutput("mymap"),
      leafletOutput("bus_map"),
      tableOutput("table")
    )
  )
  
  server <- function(input, output) {
    # # map
    #   pal <- colorNumeric(palette = "BuPu", domain = stops_routes_short$male_life_exp, reverse = TRUE)
    #   mylabel <- glue("route number: {stops_routes_short$route_short_name} operator: {stops_routes_short$agency_id}<br>
    #                 {stops_routes_short$stop_name}<br>
    #                 Male life expectancy: {stops_routes_short$male_life_exp}")
    #   
    #   mymap <- leaflet(stops_routes_short) %>%  
    #     addProviderTiles("Stamen.TonerLite") %>%
    #     addCircleMarkers(radius = 8, fillColor = ~pal(stops_routes_short$male_life_exp), 
    #                      popup = ~mylabel, weight = 2, fillOpacity = 0.8, color = "black")
    #   output$mymap <- renderLeaflet(mymap)
 
    # interactive map
    # can't work out how to use selected gender in popup/ label or colour????
      map_pal <- reactive(
        colorNumeric(palette = "BuPu", domain = !!!input$select_gender, reverse = TRUE)
      )
      
      output$bus_map <- renderLeaflet(
        stops_routes_joined %>%
          filter(route_id == input$select_route_id) %>%
          select(route_short_name, route_long_name, stop_name, male_life_exp, female_life_exp, !!!input$select_gender, agency_id) %>%
        leaflet() %>%  
        addProviderTiles("Stamen.TonerLite") %>%
        addCircleMarkers(radius = 8, 
                    #     fillColor = ~map_pal(),
                         popup = ~glue("route number: {route_short_name} operator: {agency_id}<br>
                    {stop_name}<br>
                    {male_life_exp} {female_life_exp}"), #<br>
                    # #life expectancy {!!!input$select_gender}")
                    #label = ~!!!input$select_gender,
                    weight = 2, fillOpacity = 0.8, color = "black")
      )
      
      # # potential solution:
      # observe({
      #   leafletProxy("map", data = dat) %>%
      #     clearMarkers() %>%
      #     addCircleMarkers(data = dat,
      #                      color = ~pal(eval(as.symbol(input$option))))
      # })
           
    # table
      output$table <- renderTable(
        stops_routes_joined %>%
        st_drop_geometry() %>%
        filter(route_id == input$select_route_id) %>%
          select(route_short_name, route_long_name, stop_name, !!!input$select_gender) %>%
          arrange(!!!input$select_gender)
      )
  }
  
  shinyApp(ui, server)
