
# Creator: Mehnaz Maharin (Appsilon Project 2021)
library(shiny)
library(dplyr)
library(leaflet)
library(data.table)
library(geosphere)
library(RCurl)
library(rsconnect)
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
  
#NOTE from Mehnaz: ensure that you unzip the zipped data and change following path.
  shipdata<-fread("ships.csv",
                  header=TRUE)
  #shipdata<-fread("/Users/mehnazmaharin/Documents/R-Semantic-Dahsboard-Appsilon/ships.csv",
   #                  header=TRUE)
  vessel_type <- shipdata %>% pull(ship_type) %>% unique
  vessel_name <- shipdata %>% pull(SHIPNAME) %>% unique
  
  ui <- semanticPage(
    #----Header--------
    style="background: #ADD8E6",
    div(class = "ui raised segment", 
    h1(class = "ui header", "Appsilon Marine Data Project",style="text-align:center")
    ),
    #----Dropdown Options --------
    cards(class = "three",(
              card(div(class = "content", div(class = "header", 
            dropdown_input("select_type", vessel_type, default_text = "Select type of vessel",type = "selection fluid")
                      )
                      )
            )
            
            ),
          (
            card(div(class = "content", div(class = "header", 
              dropdown_input("select_name", vessel_name, default_text = "Select name of vessel",type = "selection fluid")            )
            )
            )
            
          ) ,
          card(segment(div(class = "content", 
                           "Distance(m)"),  
                       textOutput("distance")
          ))
          )
    ,
  #-----If you want to see the data table then infreeze the following line-----
    #semantic_DTOutput("table"),
    
   #--------MAP --------
          
   
        segment(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map") 
  
        )
  )
  
  server <- function(input, output, session) {
    #-----If you want to see the data table then infreeze the following line-----
    #output$table <- DT::renderDataTable(
    # semantic_DT(map_distance())
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
    
    #-------Filter the Data table according to user input------
    map_event <- reactive({
      req(input$select_type)
      shipdata %>% filter(ship_type %in% input$select_type & SHIPNAME %in% input$select_name)
    })
    ##-----Calculate Distance--------
      map_distance <- eventReactive(input$select_name,{
        
        map_event()%>%
          group_by(SHIPNAME)%>%
          mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
          mutate(Distance=distHaversine(cbind(lag_LAT, lag_LON),cbind(LON,LAT)))%>%
          select(SHIPNAME,DATETIME,LAT,LON,lag_LAT,lag_LON,Distance)%>%
          slice(which.max(Distance))%>%slice_max(DATETIME)
      })
    
      #----Code for viewing Distance----------
    output$distance <- renderText({
      validate(
        need(input$select_type!="", 'Ship_Type Required')
      )
      paste0((map_distance() %>% pull(Distance)))
    })
    
    
    #----Data table to fetch initial location------
    map_table_cal_origin<-eventReactive(input$select_name,{
      map_event()%>%
        select(SHIPNAME,DATETIME,LAT,LON)%>%
        group_by(SHIPNAME)%>%
        slice_min(DATETIME)
    })
    #----Data table to fetch Final location------
    map_table_cal_destination<-eventReactive(input$select_name,{
      map_event()%>%
        select(SHIPNAME,DATETIME,LAT,LON)%>%
        group_by(SHIPNAME)%>%
        slice_max(DATETIME)
    })
    
    
   ##--------MAP output---------------------
      output$map <- renderLeaflet({
      #req(input$select_type)
      leaflet()%>%  
        addTiles(options = providerTileOptions())%>%
        #setView(lat = 0, lng = 0, zoom = 2)%>%
        fitBounds(min(map_distance()$LON), min(map_distance()$LAT), max(map_distance()$lag_LON), max(map_distance()$lag_LAT))%>% 
        addCircles(data = map_distance(), 
                   lng =map_distance()$LON, lat =map_distance()$LAT,
                   radius=12,
                   color="brown",
                   popup=paste("Origin Point","<br>",
                               "Lattitude: ", map_distance()$LAT, "<br>",
                               "Longtitude: ", map_distance()$LON,"<br>",
                               "DateTime", map_distance()$DATETIME
                   ))%>%
        addCircles(data = map_distance(), 
                   lng=map_distance()$lag_LON, lat=map_distance()$lag_LAT,
                   radius = 12,
                   color="green",
                   popup=paste("Origin Point","<br>",
                               "Lattitude: ", map_distance()$lag_LAT, "<br>",
                               "Longtitude: ", map_distance()$lag_LON,"<br>",
                               "DateTime: ", map_distance()$DATETIME
                               
                               ))%>%
        addMarkers(data = map_table_cal_origin(), 
                   lng =map_table_cal_origin()$LON, lat =map_table_cal_origin()$LAT,
                   label="Initial Position")%>%
        addMarkers(data = map_table_cal_destination(), 
                   lng =map_table_cal_destination()$LON, lat =map_table_cal_destination()$LAT,
                   label="Final Position")%>%
        addPolylines(data = rbind(map_table_cal_origin(),map_table_cal_destination()),
                     lng = ~c(LON),
                     lat = ~c(LAT),
                     group = 'markers',
                     color = '#FF1493')%>%
        addLegend(position = "bottomright",pal = colorFactor(palette = c("green","brown",'#FF1493'),domain = c("Origin Point","Destination Point","Journey Path"),ordered = T)
                , values = c("Origin Point","Destination Point","Journey Path"),title = "Points of Observation")
  
    })
  
}
  
  shinyApp(ui, server)
