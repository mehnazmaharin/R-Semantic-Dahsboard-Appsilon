# R-Semantic-Dahsboard-Appsilon

Dashboard Link : [Shiny Semantic Dashboard](https://mehnazmaharin.shinyapps.io/R-Semantic-Dashboard-Appsilon/)

## Data Manupulation
Finding highest distance between two consequtive points:
At fisrt I filtered the data table using ship type and ship name through user input using the following code:
```R
 map_event <- 
      shipdata %>% filter(ship_type %in% input$select_type & SHIPNAME %in% input$select_name)
```
I used lag() function to find out the two points of lattitude,longitude and their distance .The data manupulation operation is described bellow:
```javascript
 map_distance<-map_event()%>%
          group_by(SHIPNAME)%>%
          mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
          mutate(Distance=distHaversine(cbind(lag_LAT, lag_LON),cbind(LON,LAT)))%>%
          select(SHIPNAME,DATETIME,LAT,LON,lag_LAT,lag_LON,Distance)%>%
          slice(which.max(Distance))%>%slice_max(DATETIME)
```
I mentioned the distance and added polyline for the inital and final position of the ship.

![Dashboard View](https://github.com/mehnazmaharin/R-Semantic-Dahsboard-Appsilon/blob/main/Screen%20Shot%202021-01-24%20at%2011.59.23%20PM.png)
