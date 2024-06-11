# load packages

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
  stops_routes_joined2 <- readRDS("shiny_life_exp_stop.rds")

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
      radioButtons("select_stat", 
                   "Select measure", 
                   c("life expectancy",
                     "healthy life expectancy",
                     "years not in good health"), 
                   selected = "life expectancy"),
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
            # adjusts height of map to be full window minus some for header + box at bottom
            tags$style(type = "text/css", "#bus_map {height: calc(100vh - 250px) !important;}"),
            box(title = textOutput("selected_stat"), #"on this route",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                infoBoxOutput("max_le"),
                infoBoxOutput("min_le"),
                infoBoxOutput("diff_le")
            ),
            leafletOutput("bus_map"),
            # ),

            downloadButton("bttn_data", "Get the data (csv)")
            ),
          # tab with table of all data
          tabItem(tabName = "data",
            DT::DTOutput("table")
          ),
          # details & sources tab
          tabItem(tabName = "details",
            h2("Further information"),
            br(),
            h3("What does this app show?"),
            p("This app is intended to visualise the differences in life expectancy and healthy life expectancy that can be seen over relatively short distances."),
            p("Life expectancy is at birth is the average number of years a baby born today in a particular area would be expected to live, if the death rates at each age group stayed as they are today for the rest of their life."),
            p("Life expectancy is an important indicator of overall population health, and inequalities in health."),
            p("We can't live for ever, but is it fair that people from some areas can expect so many years more life than people from other areas? We would want to narrow the gap from what it is today."),
            p("Healthy life expectancy is the number of years a baby born today in a particular area can expect to live in good health, again if today's situation remained the same for the rest of their life. This refers to self rated health, but self rated health is a good predictor of service usage."),
            p("Years not in good health is the difference between the two."),
            p("We would want to increase the proportion of people's lives that is spent in good health so they have the health to do what they want to do as long as possible, and also to reduce the cost of intensive health and social care that is often necessary where people experience a prolonged period of very poor health right at the end of their lives."),
            p("To find out more about the reasons behind the inequalities in life expectancy, take a look at the Office for Health Improvement & Disparities's Segment Tool."),
            a("OHID Segment Tool", href = "https://analytics.phe.gov.uk/apps/segment-tool/", 
              target = "_blank"),
            br(),
            br(),
            h2("Data sources"),
            p("Bus data from Open Data Manchester (no longer available online)."),
            # a("Open Data Manchester Bus Fare Dropbox", href = "https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0", 
            #    target = "_blank"),
            p("Bus routes are no longer completely up to date, but are still useful to show how life expectancy changes over relatively small areas."),
            br(),
            p("Life expectancy data is from the Office for Health Improvement & Disparities' Fingertips, 2022 issue which is based on data relating to the period 2016-2020."),
            p("Healthy Life expectancy data is from the Office for Health Improvement & Disparities' Fingertips, 2020 issue which is based on data relating to the period 2013-2017, however this is no longer available from this source."),
            p("Life expectancy at birth & healthy life expectancy used (upper age band 85+), for the Middle Super Output Area (MSOA) in which the bus stop is located."),
            p("Years not in good health is calculated as the difference between the two. Since these now relate to different time periods, caution must be used when interpreting this information."),
            a("OHID Fingertips", href = "https://fingertips.phe.org.uk/search/life%20expectancy#page/0/gid/1/pat/101/par/E08000001/ati/3/are/E02000984", 
              target = "_blank"),
            br(),
            br(),
            p("Code for this app is on my github"),
            a("Github", href="https://github.com/shanwilkinson2/busses", target = "_blank")
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
            filter(route_short_long_name == input$select_route_id & 
                     life_exp_gender == input$select_gender &
                     life_exp_stat == input$select_stat) %>%
            select(route_short_name, agency_id, route_long_name, route_short_long_name, stop_name, 
                   stop_sequence, life_exp_gender, life_exp_val)
    })
    
  # name of selected stat
    output$selected_stat <- renderText({input$select_stat})
    
  # interactive map
    # domain = range of values  
    life_exp_pal <- reactive({
                   colorNumeric(palette = "BuPu", 
                          domain = c(min(selected_data()$life_exp_val, na.rm = TRUE),
                                     max(selected_data()$life_exp_val, na.rm = TRUE)), 
                          reverse = ifelse(input$select_stat == "years not in good health", FALSE, TRUE)
    )
            })
    
  
    
    # map
    output$bus_map <- renderLeaflet({
        myLabels = as.list(glue("<b>Route:</b> {selected_data()$route_short_name} {selected_data()$route_long_name}<br>
                                            <b>Stop:</b> {selected_data()$stop_name}<br>
                                <b>{str_to_sentence(selected_data()$life_exp_gender)} {input$select_stat}:</b> {round(selected_data()$life_exp_val, 1)}"))
        selected_data() %>%
            leaflet() %>%  
            addProviderTiles("OpenStreetMap.Mapnik") %>%
            addCircleMarkers(radius = 8, 
                             fillColor = ~life_exp_pal()(life_exp_val),
                             # popup = ~glue("<b>Route:</b> {route_short_name} {route_long_name}<br>
                             #                <b>Stop:</b> {stop_name}<br>
                             #    <b>{str_to_sentence(life_exp_gender)} life expectancy:</b> {life_exp_val}"), 
                             label = lapply(myLabels, HTML),
                             weight = 2, fillOpacity = 0.8, color = "black") %>%
            addLegend("bottomleft", pal = life_exp_pal(), values = ~life_exp_val,
                      labFormat = labelFormat(digits = 0), title = input$select_stat,
                      opacity = 1) %>%
            addControl("click on a bus stop to find out more", 
                       position = "topright")
    })
    
    # info box caption 
    output$infobox_caption <- reactive({
      paste(input$select_stat, "on this route")
    })
    
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
        select("Route number" = route_short_name, "Stop name" = stop_name, "Statistic" = life_exp_stat,
               "Gender" = life_exp_gender, "Value" = life_exp_val)
    }, filter = "top", rownames = FALSE)
}

shinyApp(ui, server)

