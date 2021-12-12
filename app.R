library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet)
library(maptools)
library(sp)
library(htmltools)
library(htmlwidgets)
library(DT)


source('global.R')

ui <- fluidPage(
    
    h1("Where did Citi Bike users go during the pandemic?"),
    
    h2("Along a river and back."),
    
    p('The on-going covid-19 pandemic has brought many changes to daily life in 
    New York City.  Among those changes, Citi Bikes are increasingly borrowed 
    from stations near the water, and more frequently returned back to the same station.'),
    
    p('The interactive maps below show that 9 out of the 50 busiest Citi Bike 
    stations saw an increase in activity during the first 7 months of the pandemic, 
    compared to the same 7 months in 2019.  Almost all of these stations 
    (shown in blue) are located along the Hudson River or East River, and 
    82% of the increase in activity at these stations was from trips that started 
      and ended at the same place.'),
    
    p('Click on any station to view the changes in trips between that station and the others.'),
    
    # Maps
    splitLayout( h3('Citi Bike Activity 2019'), 
                 h3('Citi Bike Activity 2020')
    ),

    splitLayout( leafletOutput(outputId = "map_2019"), 
                 leafletOutput(outputId = "map_2020")
    ),
    
    fluidRow( p('The maps show the 50 Citi Bike stations with the most activity from 
       March to September of 2019 and March to September of 2020 combined.'),
              h4(textOutput('selected')),
              actionButton('clear', 'Clear Selected Station'),
              
    hr(),
    
    # Initial table and title
    h4(textOutput('initial_table_title')),
    DT::dataTableOutput('initial_table'),
    
    hr(),

    # Connections table and title
    h4(textOutput('connections_table_title')),
    DT::dataTableOutput('connections_table')
    ),
    
    p('The data for this site was downloaded from Citi Bike: 
      https://www.citibikenyc.com/system-data .'),
    p('This site was created by Gaelen Adam, Whitney Su, and Tobias DeKara. 12/2020')
)

server <- function(input, output){
  
  # Make initial maps
  output$map_2019 <- renderLeaflet({
      base_map(2019)
  })
  
  output$map_2020 <- renderLeaflet({
      base_map(2020)
  })
  
  # Initial text
  output$selected <- renderText('Select a station by clicking a marker on the maps or by 
                                clicking on a row in the table below.')
  
  # Display initial table and title
  output$initial_table_title <- renderText('Table: Total Number of Trips Beginning or 
                                   Ending at Each Station')
  output$initial_table <- DT::renderDataTable({
    initial_table()
  })
  
  # Observe Clicks on Maps  
  # Update Maps, Update output$selected, Update Table and table title
  observeEvent(input$map_2019_marker_click, {
      selected_station <<- input$map_2019_marker_click$id
      connection_station <<- NULL
      
      output$selected <- renderText(paste0('You have selected ', selected_station, '.'))
      
      # Reduce initial table to show just the seleced station
      output$initial_table_title <- renderText('Table 1: Total Trips')
      output$initial_table <- DT::renderDataTable({
        initial_table(selected_station = selected_station)
      })

      # Add connections table and title
      output$connections_table_title <- renderText(paste0('Table 2: Number of trips between ',
                                                          selected_station, ' and each station.'))
      
      output$connections_table <- DT::renderDataTable({
        connections_table(selected_station = selected_station)
      })
      
      output$map_2019 <- renderLeaflet({
        conn_map(year=2019, selected_station=selected_station)
      })
      output$map_2020 <- renderLeaflet({
        conn_map(year=2020, selected_station=selected_station)
      })
      
  })
  
  observeEvent(input$map_2020_marker_click, {
      selected_station <<- input$map_2020_marker_click$id
      connection_station <<- NULL
      
      output$selected <- renderText(paste0('You have selected ', selected_station, '.'))
      
      # Reduce initial table
      output$initial_table_title <- renderText('Table 1: Total Trips')
      output$initial_table <- DT::renderDataTable({
        initial_table(selected_station = selected_station)
      })
      
      # Add connections table and title
      output$connections_table_title <- renderText(paste0('Table 2: Number of trips between ',
                                                          selected_station, ' and each station.'))
      output$connections_table <- DT::renderDataTable({
        connections_table(selected_station = selected_station)
      })
      
      output$map_2019 <- renderLeaflet({
        conn_map(year=2019, selected_station=selected_station)
      })
      output$map_2020 <- renderLeaflet({
        conn_map(year=2020, selected_station=selected_station)
      })
      
  })
  
  
  # Observe click on a row of the initial table
  observeEvent(input$initial_table_rows_selected,{
    selected_station <<- station_summary$station.name[input$initial_table_rows_selected]
    connection_station <<- NULL
    
    output$selected <- renderText(paste0('You have selected ', selected_station, '.'))

    # Reduce initial table
    output$initial_table_title <- renderText('Table 1: Total Trips')
    output$initial_table <- DT::renderDataTable({
      initial_table(selected_station = selected_station)
    })
    
    # Add connections table and title
    output$connections_table_title <- renderText(paste0('Table 2: Number of trips between ',
                                                        selected_station, ' and each station.'))
    output$connections_table <- DT::renderDataTable({
      connections_table(selected_station = selected_station)
    })
    
    output$map_2019 <- renderLeaflet({
      conn_map(year=2019, selected_station=selected_station)
    })
    output$map_2020 <- renderLeaflet({
      conn_map(year=2020, selected_station=selected_station)
    })
  })

  # Observe click on a row of a connections table
  observeEvent(input$connections_table_rows_selected,{

    connection_station <<- station_summary$station.name[input$connections_table_rows_selected]
    
    output$map_2019 <- renderLeaflet({
      conn_map(year=2019, selected_station=selected_station, connection_station = connection_station)
    })
    output$map_2020 <- renderLeaflet({
      conn_map(year=2020, selected_station=selected_station, connection_station = connection_station)
    })
    
  })
  
  # Observe clicking the clear button
  observeEvent(input$clear,{
    selected_station <<- NULL
    connection_station <<- NULL
    
    output$map_2019 <- renderLeaflet({
      base_map(2019)
    })
    
    output$map_2020 <- renderLeaflet({
      base_map(2020)
    })
    
    output$selected <- renderText('Select a station by clicking on either map or 
                                  by clicking on a row in the table below.')
    
    # Display initial table and title
    output$initial_table_title <- renderText('Table: Total Number of Trips To 
                                      and From Each Station') 
    
    output$initial_table <- DT::renderDataTable({
      initial_table()
    })
    
    # Remove connections table and title
    output$connections_table <- NULL
    output$connections_table_title <- NULL
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
