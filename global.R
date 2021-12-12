stations_trips_2020 <- read.csv("stations_trips_2020.csv")
stations_trips_2019 <- read.csv("stations_trips_2019.csv")

# Color coding for 2020 maps, by percent change in activity or connections
getColor <- function(percent.change.vector) {
  sapply(percent.change.vector, function(percent.change) {
    if(percent.change < -20) {
      "#FF0000"
    } else if(percent.change < 0) {
      "#FFA07A"
    } else if(percent.change < 20) {
      "#6A5ACD"
    } else {
      "#0000CD"
    } })
}

# Add colors to stations_trips_2020
stations_trips_2020 <- stations_trips_2020 %>%
  add_column(percent.change = ((stations_trips_2020$n.trips.ss.es - stations_trips_2019$n.trips.ss.es)
                             / stations_trips_2019$n.trips.ss.es)*100)

stations_trips_2020 <- stations_trips_2020 %>%
  add_column(color = getColor(stations_trips_2020$percent.change))

# Make a 50 row table of summary data for each station
station_summary <- stations_trips_2019 %>% 
  select(start.station.name, lat.ss, long.ss, n.trips) %>%
  rename(station.name=start.station.name, lat=lat.ss, long=long.ss, n.trips.2019=n.trips) %>%
  add_column(n.trips.2020=stations_trips_2020$n.trips) %>%
  arrange(station.name)
station_summary <- station_summary[seq(1,2500,50),] %>%
  mutate(percent.change= ((n.trips.2020 - n.trips.2019)/n.trips.2019)*100) %>%
  mutate(percent.change = round(percent.change, 0))
station_summary <- station_summary %>%
  mutate(color = getColor(percent.change)) # for color coding the initial 2020 map

# Custom Legend
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, 
                   opacity = opacity, title = title, position = 'bottomright' ))
}

# Initial Map / Map after selected station is cleared
base_map <- function(year=2019){
  if (year==2019){
    map <- leaflet(options = leafletOptions(zoomSnap = .25, zoomDelta = .75)) %>%
      setView(lat = 40.74287, lng=	-73.98192, zoom = 12.25) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%  
      addCircleMarkers(data = station_summary, lat = ~ lat, lng = ~ long, 
                       layerId = ~ station.name, 
                       radius = ~((n.trips.2019-8000)/4000), color = ~'#008000') %>%
      addLegendCustom(colors = c('#008000'), 
                      labels = c("0-10,000", "10,000-20,000", "20,000-30,000","30,000-40,000","40,000-50,000"), 
                      sizes = c(6, 10, 14, 18, 22),
                      title = 'Total trips in 2019')
    
  }
  else {
    map <- leaflet(options = leafletOptions(zoomSnap = .25, zoomDelta = .75)) %>%
      setView(lat = 40.74287, lng=	-73.98192, zoom = 12.25) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%  
      addCircleMarkers(data = station_summary, lat = ~ lat, lng = ~ long, 
                  layerId = ~ station.name,  
                  radius = ~((n.trips.2020-8000)/4000), 
                  color = ~ color,
                  fillOpacity = 1) %>%
      addLegendCustom(colors = c("#FF0000", "#FFA07A", "#6A5ACD", "#0000CD"), 
                    labels = c("decreased 20% or more", "decreased 0 to 20%", 
                               "increased 0 to 20% ","increase more than 20%"), 
                    sizes = 12,
                    title = 'Change in Activity')
  }
  return(map)
}

# Make a map of connections to a selected station
conn_map <- function(year=2019, selected_station, connection_station=NULL){
  # @param connection_station is a vector of stations because users can select
  # multiple rows of the connections table
  if (year==2019){
    data_selected <- stations_trips_2020 %>%   # for color and size of the markers
      filter(start.station.name==selected_station) %>%
      arrange(end.station.name)
    
    # Add circle makers and legend
    map <- leaflet(options = leafletOptions(zoomSnap = .25, zoomDelta = .5)) %>%
      setView(lat = 40.74287, lng=	-73.98192, zoom = 12.25) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  
      addCircleMarkers(data = station_summary, lat = ~ lat, lng = ~ long, 
                       layerId = ~station.name, 
                       radius = ~((data_selected$n.trips.ss.es - 400)/400), 
                       color = ~ "#008000") %>%
      addLegendCustom(colors = c('#008000'), 
                      labels = c("0-2,000", "2,000-4,000", "4,000-6,000","6,000-8,000","8,000-10,000"), 
                      sizes = c(6, 13, 20, 27, 34),
                      title = 'Connections in 2019') %>%
      addMarkers(lat = data_selected$lat.ss[1], lng = data_selected$long.ss[1])
    
    # Add lines between selected_station and connection stations
    if (!is.null(connection_station)){  
      data_connected <- data_selected %>%
        filter(end.station.name %in% connection_station)
      for (i in 1:nrow(data_connected)){
        map <- map %>%
          addPolylines(data = data_connected[i,],
                       lat= ~ c(lat.ss, lat.es),
                       lng= ~ c(long.ss, long.es),
                       color = 'black',
                       weight = ~ 2)
      }
    }
  }
  else {   # ie year==2020
    data_selected <- stations_trips_2020 %>%   # for color and size of the markers
      filter(start.station.name==selected_station) %>%
      arrange(end.station.name)
      
    # Add circle makers and legend
    map <- leaflet(options = leafletOptions(zoomSnap = .25, zoomDelta = .5)) %>%
      setView(lat = 40.74287, lng=	-73.98192, zoom = 12.25) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%  
      addCircleMarkers(data = station_summary, lat = ~ lat, lng = ~ long, 
                       layerId = ~station.name, 
                       radius = ~((data_selected$n.trips.ss.es - 400)/400), 
                       color = ~ data_selected$color) %>%
      addLegendCustom(colors = c("#FF0000", "#FFA07A", "#6A5ACD", "#0000CD"), 
                      labels = c("decreased 20% or more", "decreased 0 to 20%", 
                                 "increased 0 to 20% ","increase more than 20%"), 
                      sizes = 12,
                      title = 'Change in Connections') %>%
      addMarkers(lat = data_selected$lat.ss[1], lng = data_selected$long.ss[1])
    
    # Add lines between selected_station and connection stations
    if (!is.null(connection_station)){  
      data_connected <- data_selected %>%
        filter(end.station.name %in% connection_station)
      for (i in 1:nrow(data_connected)){
        map <- map %>%
          addPolylines(data = data_connected[i,],
                       lat= ~ c(lat.ss, lat.es),
                       lng= ~ c(long.ss, long.es),
                       color = 'black',
                       weight = ~ 2)
      }
    }
  }
  return(map)
}

initial_table <- function(selected_station=NULL){
  if (is.null(selected_station)){
    trips_2019 <- station_summary$n.trips.2019
    trips_2020 <- station_summary$n.trips.2020
    
    tab <- data.frame("Stations" = station_summary$station.name,
                      "Total Trips 2019" = trips_2019, 
                      "Total Trips 2020" = trips_2020,
                      "Percentage Change" = round((trips_2020 - trips_2019) / trips_2019, 2),
                      check.names = FALSE)
    
    tab <- datatable(tab, selection = 'single') %>%
      formatPercentage('Percentage Change')
  }
  else {
    selected_row <- station_summary %>%
      filter(station.name==selected_station)
    trips_2019 <- selected_row$n.trips.2019
    trips_2020 <- selected_row$n.trips.2020
    
    tab <- data.frame("Station" = selected_station,
                      "Total Trips 2019" = trips_2019, 
                      "Total Trips 2020" = trips_2020,
                      "Percentage Change" = round((trips_2020 - trips_2019) / trips_2019, 2),
                      check.names = FALSE)
    
    tab <- datatable(tab, options = list(dom = 't'), selection = 'single') %>%
      formatPercentage('Percentage Change')
  }

return(tab)
}

# Make a table of connections to the selected station
connections_table <- function(selected_station=selected_station){
  
  connections_2019 <- stations_trips_2019 %>%
    filter(start.station.name == selected_station) %>%
    arrange(end.station.name)
    
  connections_2020 <- stations_trips_2020 %>%
    filter(start.station.name == selected_station) %>%
    arrange(end.station.name)
  
  conn_tab <- data.frame("Stations"=connections_2019$end.station.name, 
                    "Connections in 2019"=connections_2019$n.trips.ss.es, 
                    "Connections in 2020"=connections_2020$n.trips.ss.es,
                    "Percentage Change" = 
                      round((connections_2020$n.trips.ss.es - 
                               connections_2019$n.trips.ss.es) / connections_2019$n.trips.ss.es, 2), 
                    check.names = FALSE)
  row.names(conn_tab) <- c()
  
  conn_tab <- datatable(conn_tab) %>%
    formatPercentage('Percentage Change')
  return(conn_tab)
}