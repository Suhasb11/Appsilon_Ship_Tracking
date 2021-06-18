# Ship tracking app
# Developed by Suhas Baladkar
# Date: 16-06-2021



#libraries ----
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(data.table)
library(dplyr)
library(leaflet)
library(geosphere)
library(plotly)
library(testthat)
#setwd("F:/marin")
#import data ----
ships_df = fread(
  "data/ships.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = F
)
testthat::expect_is(ships_df, "data.frame")
#ships_df <- read.csv("data/ships.csv")
#ships_df<- as.data.frame(ships_df)
ship_type_df <-
  ships_df %>% select(ship_type) %>% distinct() %>% arrange()
ship_type_df <- as.data.frame(ship_type_df)

ship_df <-
  ships_df %>% select(ship_type) %>% distinct() %>% arrange()
ship_type_df <- as.data.frame(ship_type_df)
ship_name_df <-
  ships_df %>% select(ship_type, SHIPNAME) %>% distinct() %>% arrange(ship_type, SHIPNAME)





dropDownUI <- function(id, div_width = "col-xs-12 col-md-4") {
  ns <- NS(id)
  
  div(
    br(),
    br(),
    uiOutput(ns("class_level")),
    
    
    br(),
    br(),
    uiOutput(ns("ship_name_ui"),
    ))
}

dropDown <- function(id) {
  moduleServer(id, function(input, output, session) {
    print("dropDown start")
    #Sys.sleep(10)
    ns <- session$ns
    
    output$class_level <- renderUI({
      selectInput(
        ns("ship_type"),
        label = h4("Ship Type"),
        choices = ship_type_df$ship_type,
        selected = "Cargo"
      )
    })
    
    
    
    output$ship_name_ui <- renderUI({
      req(input$ship_type)
      #Sys.sleep(10)
      ns <- session$ns
      
      my_choices <-
        ship_name_df %>% filter(ship_type == input$ship_type)
      
      selectInput(
        inputId = ns("ship_name"),
        label = h4("Ship Name"),
        choices = my_choices$SHIPNAME
      )
      
    })
    
  })
}

mapUI <- function(id) {
  #Sys.sleep(10)
  leafletOutput(NS(id, "map"), height=300)
}

speedlineUI <- function(id) {
  #Sys.sleep(10)
  plotlyOutput(NS(id, "linegrap"), height = "100px")
}
#points_reactive----
points_reactive <- function(id) {
  #Sys.sleep(3)
  
  moduleServer(id, function(input, output, session) {
    points_reactive <- reactive({
      print("points_reactive start")
      #Sys.sleep(10)
      req(input$ship_type)
      req(input$ship_name)
      
      
      
      data <-
        ships_df %>% filter(ship_type == input$ship_type &
                              SHIPNAME == input$ship_name)
      
      testthat::expect_is(data$date, "character")
      data$date <- as.Date(data$date, "%m/%d/%Y")
      testthat::expect_is(data$date, "Date")
      class(data$date)
      #if (nrow(data==0)) return()
      max_date <- max(data$date)
      data <- data %>% filter(date == max_date)
      print(nrow(data))
      print("points_reactive end")
      data
    })
  })
}
# This function is used to get the start and end point of the ship
# for recent date.
points_fn <- function(id, p_points_data) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      print("points_fn start")
      print("11")
      # print(p_points_data)
      #Sys.sleep(10)
      req(input$ship_type)
      req(input$ship_name)
      data <- as.data.frame(p_points_data())
      
      
      # #print(data)
      # rad = pi / 180
      # x = (data$LON - mean(data$LON)) * cos(data$LAT * rad)
      # y = (data$LAT - mean(data$LAT))
      # i.n_1 = 1 # n_1: n-1
      # x.n_1 = mean(x)
      # y.n_1 = 0 # = mean(y)
      # s.n_1 = 0 # s: square of distance
      # repeat {
      #   s = (x - x.n_1)^2 + (y - y.n_1)^2
      #   i.n = which.max(s)
      #   x.n = x[i.n]
      #   y.n = y[i.n]
      #   s.n = s[i.n]
      #   if (s.n <= s.n_1) break
      #   i.n_1 = i.n
      #   x.n_1 = x.n
      #   y.n_1 = y.n
      #   s.n_1 = s.n
      # }
      # i.m_1 = 1
      # x.m_1 = (x.n + x.n_1) / 2
      # y.m_1 = (y.n + y.n_1) / 2
      # s.m_1 = 0
      # m_ok  = TRUE
      # repeat {
      #   s = (x - x.m_1)^2 + (y - y.m_1)^2
      #   i.m = which.max(s)
      #   if (i.m == i.n || i.m == i.n_1) { m_ok = FALSE; break }
      #   x.m = x[i.m]
      #   y.m = y[i.m]
      #   s.m = s[i.m]
      #   if (s.m <= s.m_1) break
      #   i.m_1 = i.m
      #   x.m_1 = x.m
      #   y.m_1 = y.m
      #   s.m_1 = s.m
      # }
      # if (m_ok && s.m > s.n) {
      #   i = i.m
      #   j = i.m_1
      # } else {
      #   i = i.n
      #   j = i.n_1
      # }
      # # output: i, j
      # data1 <- data[i, ]
      # data2 <- data[j, ]
      # print(i)
      testthat::expect_is(data$DATETIME, "POSIXct")
      data$Time <- format(data$DATETIME, "%H:%M:%S")
      data <- data %>% arrange (Time)
      print("2")
      i <- 1
      j <- nrow(data)
      data1 <- data[i,]
      data2 <- data[j,]
      data1 <-
        data1 %>% select(LAT, LON) %>% rename(LAT1 = LAT, LON1 = LON)
      data2 <-
        data2 %>% select(LAT, LON) %>% rename(LAT2 = LAT, LON2 = LON)
      start_end_points <- bind_cols(data1, data2)
      
      # Following function is used to calculate total distance if the destinations are different on same day
      start <- 1
      distmtr <- 0
      print("3")
      
      print("data")
      destination <- data %>% select(DESTINATION) %>% distinct()
      # Sys.sleep(10)
      destination<-  as.data.frame(destination)
      
      
      
      print(nrow(destination))
      norows <- as.numeric(NROW(destination))
      print(norows)
      print(class(norows))
      #for loop ----
      
      for (i in 1:norows) {
        print(norows)
        if (i>norows) {
          break
        }
        print("in loop")
        
        dest_df <-
          data %>% filter(DESTINATION == destination[i, "DESTINATION"])
        dest_df1 <- dest_df[start,]
        end <- nrow(dest_df)
        dest_df2 <- dest_df[end,]
        
        print("3")
        dest_df1<- as.data.frame(dest_df1)
        print(dest_df1)
        print(dim(dest_df1)[2])
        print(nrow(dest_df1))
        print(dest_df1$LON)
        distmtr <-
          distmtr + distm (c(dest_df1$LON, dest_df1$LAT),
                           c(dest_df2$LON, dest_df2$LAT),
                           fun = distHaversine)
        print(distmtr)
        
      }
      print("for loop complete")
      print(is.null(start_end_points))
      print(start_end_points)
      print(dim(start_end_points)[1])
      print(dim(start_end_points))
      if (dim(start_end_points)[1]!=0) {
        start_end_points$distm <- distmtr
      }
      print("1111")
      #data_not_parked <- data %>% filter(is_parked == 0)
      #start_end_points$avg_speed <- mean(data_not_parked$SPEED)
      #start_end_points$max_speed <- max(data$SPEED)
      print("points_fn end")
      start_end_points
    })
  })
}

# linegraphserver <- function(id,points_reactive) {
#   moduleServer(id, function(input, output, session) {
#     output$linegrap <- renderPlotly({
#
#       #Sys.sleep(10)
#       #print(points_reactive())
#       print(class(points_reactive()))
#       data<- as.data.frame(points_reactive())
#       data$Time <- format(data$DATETIME,"%H:%M:%S")
#       data<- data %>% arrange (Time)
#       speed_line <- plot_ly(data, x = ~Time, y = ~SPEED, type = 'scatter', mode = 'lines')
#       speed_line
#     })
#   })}


mapserver <- function(id, points_reactive, start_end_points) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      print("mapserver start")
      #Sys.sleep(1)
      
      r <- reactive({ req(input$ship_type, input$ship_name)})
      req(input$ship_type)
      req(input$ship_name)
      #print(points_reactive())
      print(class(points_reactive()))
      
      data <- as.data.frame(points_reactive())
      validate(
        need(nrow(data) > 0, message = FALSE)
      )
      start_end_points <- start_end_points()
      #print(data)
      data1 <- data[1,]
      #data <- ships_df %>% filter(ship_type=="Cargo" & SHIPNAME=="KAROLI")
      
      details <-
        paste(
          sep = "<br/>",
          paste("Spead: ", data$SPEED, sep = ""),
          paste("Time: ", data$DATETIME, sep = "")
        )
      leaflet(data = data1)  %>%
        setView(data1$LON, data1$LAT, zoom = 9) %>%
        addProviderTiles("Esri.NatGeoWorldMap", options = providerTileOptions(opacity = 0.9)) %>%
        #addProviderTiles("OpenStreetMap.DE")   %>%
        addCircleMarkers(
          ~ data$LON,
          ~ data$LAT,
          popup = ~ details,
          radius = 3,
          color = "yellow",
          fill = TRUE,
          fillColor = "yellow",
          weight = 1,
          opacity = 3
        ) %>%
        addCircleMarkers(
          ~ start_end_points$LON1,
          ~ start_end_points$LAT1,
          radius = 6,
          color = "red",
          fill = TRUE,
          fillColor = "red",
          weight = 1,
          opacity = 1
        ) %>%
        addCircleMarkers(
          ~ start_end_points$LON2,
          ~ start_end_points$LAT2,
          radius = 6,
          color = "red",
          fill = TRUE,
          fillColor = "red",
          weight = 1,
          opacity = 1
        )
      
    })
    
    output$linegrap <- renderPlotly({
      req(input$ship_type)
      req(input$ship_name)
      #Sys.sleep(10)
      #print(points_reactive())
      print(class(points_reactive()))
      speed_line_df <- as.data.frame(points_reactive())
      validate(
        need(nrow(speed_line_df) > 0, message = FALSE)
      )
      speed_line_df$Time <-
        format(speed_line_df$DATETIME, "%H")
      speed_line_df <- speed_line_df %>% arrange (Time)
      speed_line <-
        plot_ly(
          speed_line_df,
          x = ~ Time,
          y = ~ SPEED,
          type = 'scatter',
          mode = 'lines'
        )
      print("speed_line end")
      speed_line
    })
  })
  
  
  
}
shiptrackApp <- function() {
  sidebar <-
    dashboardSidebar(
      side = "left",
      size = "thin",
      color = "teal",
      dropDownUI(id = "ship1")
    )
  body <-   ## Body content----
  
  dashboardBody(
    #fluidRow(box( color="blue", ribbon = FALSE, width = 5, textOutput("distance"))),
    # fluidRow(chartTableBoxUI(id = "ATC_Topline"))) # render the tabBox inside a fluidRow
    fluidRow(box(
      title = "Ship Tracking",
      color = "blue",
      width = 12,
      mapUI("ship1")),
      #box( title="Note", title_side = "top left",collapsible = FALSE, color="blue", ribbon = FALSE, width = 3, textOutput("distance"))
      #textOutput("distance", placeholder = TRUE)
      box(
        title = "Note", width = 4, background = "grey",ribbon =FALSE,
        collapsible = FALSE,title_side = "top left",
        textOutput("distance")
      )
    ),
    fluidRow(
      box(
        title = "Ship Speed",
        color = "blue",
        width = 16,
        speedlineUI("ship1")
      )
    ))
  
  # Put them together into a dashboardPage
  ui <-   dashboardPage(
    
    dashboardHeader(
      color = "blue",
      inverted = TRUE,
      title = "Appsilon Ship tracking"
    ),
    sidebar,
    body
  )
  # server ----
  server = {
    shinyServer(function(input, output, session) {
      suppressWarnings(points_fn("ship1"))
      suppressWarnings(points_reactive("ship1"))
      dropDown("ship1")
      #Sys.sleep(10)
      Sys.sleep(.2)
      
      points_reactive <- points_reactive("ship1")
      print("1")
      
      #print(points_reactive)
      
      start_end_points <- points_fn("ship1", points_reactive)
      #print(start_end_points)
      mapserver("ship1", points_reactive, start_end_points)
      #linegraphserver("ship1", points_reactive)
      output$distance <- renderText({
        distance <- start_end_points()
        if(dim(distance)[1]!=0) {
          distance$distm <- round(distance$distm, 2)
        }
        #print(distance)
        
        distinmtr <- paste( "Distance in meters: ",distance$distm,sep = "")
        distinmtr
        
      })
      
    })
    
  }
  
  shinyApp(ui = ui, server = server)
}
shiptrackApp()

