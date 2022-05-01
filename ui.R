library(shiny)
library(leaflet)
library(leafem)
library(rdepthmap)
library(rgeos)
library(rgdal)
library(dplyr)
library(RColorBrewer)


#UI-------------------------------------------------------

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10,
                h4("Popular Times of Hawker Centres"),              
                selectInput("day","Day of the Week",
                            c("Monday" = "mon",
                              "Tuesday" = "tue",
                              "Wednesday" = "wed",
                              "Thursday" = "thu",
                              "Friday" = "fri",
                              "Saturday" = "sat",
                              "Sunday" = "sun")),
                sliderInput("time", "0000 - 2400", min = 0, max = 23, value = 12),
                #checkboxInput("voro", "Show voronoi map", TRUE),
                checkboxInput("legend", "Show legend", TRUE)
  )
)