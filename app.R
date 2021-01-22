#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
# Define UI for application that draws a histogram
if (interactive()){
  library(shiny)
  library(shiny.semantic)
  shipdata<-read.csv("/Users/mehnazmaharin/Downloads/ships.csv", header=TRUE)
  vessel_type <- shipdata %>% pull(ship_type) %>% unique
  vessel_name <- shipdata %>% pull(SHIPNAME) %>% unique
  
  ui <- shinyUI(semanticPage(
    
   fluidRow(
    dropdown_input("select_type", vessel_type, default_text = "Select type of vessel",type = "selection fluid"),
    dropdown_input("select_name", vessel_name, default_text = "Select name of vessel",type = "selection fluid"),
    div(class = "ui raised segment",
        segment(
          h4("Distance between points (m)"),  
          textOutput("distance")
        )
    ))
    ,
    semantic_DTOutput("table"),
    
    #MAP --------
    div(class = "ui raised segment",
        mainPanel(width = "100%",
                  leafletOutput("map")
        )
    ),
    #Map 2--------
    div(class = "ui raised segment",
        mainPanel(width = "100%",
                  leafletOutput("map2")
        )
    )
  ))
  server <- function(input, output, session) {
    
   # output$table <- DT::renderDataTable(
      
    #  semantic_DT(shipdata)
    #)
    
    get_vessel <- reactive({
      shipdata %>% filter(ship_type %in% input$select_type)
    })
    vessel_name <- reactive({
      get_vessel() %>% pull(SHIPNAME) %>% unique
    })
    
    observeEvent(input$select_type, {
      update_dropdown_input(session, "select_name", 
                            choices = vessel_name(),
                            choices_value = vessel_name())
    })
    
    #Distance
    
    map_event <- reactive({
      shipdata %>% filter(ship_type %in% input$select_type & SHIPNAME %in% input$select_name)
    })
    map_distance <- reactive({
     
      map_event()%>%
        mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
        mutate(Distance=distHaversine(cbind(lag_LAT, lag_LON),cbind(LON,LAT)))%>%
        select(SHIPNAME,DATETIME,LAT,LON,lag_LAT,lag_LON,Distance)%>%group_by(SHIPNAME) %>%
        slice(which.max(Distance))%>%slice_max(DATETIME)
      
      #/---------------Forget this------
      
      # map_event()%>%arrange(DATETIME)%>%
       #   mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
        #  mutate(Distance=distHaversine(cbind(lag_LAT, lag_LON),cbind(LON,LAT)))%>%
        #slice_max(Distance)%>%slice_max(DATETIME)%>%
        #max(DATETIME)%>%
        
        #top_n(1, Distance) %>% arrange(desc(DATETIME)) %>% slice(1) %>% pull(Distance)
      #ungroup()
    })
   
    #ouput
   output$table <- DT::renderDataTable({
      semantic_DT(map_distance())
      #%>% 
       # paste0(top_n(1, Distance) %>% arrange(desc(DATETIME)) %>% slice(1) %>% pull(Distance)%>%abs)
  })
    output$distance <- renderText({
      paste0((map_distance() %>% pull(Distance)) %>% abs)
    })
    
    
    #MAP------
    map_table_cal_origin<-reactive({
      map_event()%>%
        mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
        mutate(Distance=distHaversine(cbind(lag_LAT, lag_LON),cbind(LON,LAT)))%>%
        select(SHIPNAME,DATETIME,LAT,LON,lag_LAT,lag_LON,Distance)%>%group_by(SHIPNAME)%>%
        slice_min(DATETIME)
    })
    map_table_cal_destination<-reactive({
      map_event()%>%
        mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
        mutate(Distance=distHaversine(cbind(lag_LAT, lag_LON),cbind(LON,LAT)))%>%
        select(SHIPNAME,DATETIME,LAT,LON,lag_LAT,lag_LON,Distance)%>%group_by(SHIPNAME)%>%
        slice_max(DATETIME)
    })
    #layer_options <- layersControlOptions()
    custom_tile <- "Esri.WorldGrayCanvas"
    output$map <- renderLeaflet({
      leaflet() %>%  
        addTiles(options = providerTileOptions())%>%
        addCircles(data = map_distance(), 
                   lng =map_distance()$LON, lat =map_distance()$LAT,
                   radius=30)%>%
        addCircles(data = map_distance(), 
                   lng=map_distance()$lag_LON, lat=map_distance()$lag_LAT,
                   radius = 30)%>%
        addCircles(data = map_table_cal_origin(), 
                   lng =map_table_cal_origin()$LON, lat =map_table_cal_origin()$LAT,
                   radius=10,
                   color="#DC8E2A",
                   label="Initial Position")%>%
        addCircles(data = map_table_cal_destination(), 
                   lng =map_table_cal_destination()$LON, lat =map_table_cal_destination()$LAT,
                   radius = 10,
                   color="#2F0C58",
                   label="Final Position")
  
    })
    output$map2 <- renderLeaflet({
      leaflet() %>%  
        addTiles(options = providerTileOptions())%>%
        addCircles(data = map_table_cal_origin(), 
                   lng =map_table_cal_origin()$LON, lat =map_table_cal_origin()$LAT,
                   radius=30)%>%
        addCircles(data = map_table_cal_destination(), 
                   lng =map_table_cal_destination()$LON, lat =map_table_cal_destination()$LAT,
                   radius = 30)
      
    })
  }
  shinyApp(ui, server)
}