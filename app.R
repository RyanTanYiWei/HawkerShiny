library(shiny)
library(leaflet)
library(leafem)
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

#Server-------------------------------------------------------

server <- function(input, output, session) {
  
  #read data
  t <- readOGR("Data_May1_2022/hawkers_v_times.mif")
  p <- readOGR("Data_May1_2022/hawkers_existing.mif")
  bins <- c(-1, 0, 20, 40, 60, 80, 100) #10 bins
  bin2 <- c(-1, 0, 6, 12, 18, 24)
  tt <-t
  
  #get column name
  dt <- reactive({
    paste0(input$day, input$time)
  })
  
  #output$text <- renderText({ dt() })

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite (Default)") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
      addPolygons(data = tt, fillOpacity = 0) 
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = tt)
    eval(parse(text = paste0( "pal <- colorBin(palette = c('Black', brewer.pal(n=6,name='Reds')), domain = tt$",dt(),", bins = bins)")))
    pal2 <- colorBin(palette = c('deeppink2', "gray80", "khaki1", "darkgoldenrod1","gray35"), domain = tt$peak, bins = bin2)

    leafletProxy("mymap", data = tt) %>%
      clearShapes() %>% addPolygons(
        fillColor = ~pal(eval(parse(text = dt()))),
        #fillColor = ~pal(eval(parse(text = "fri6"))),
        weight = 1,
        opacity = 1,
        color = "white",
        group = "Service Area (Voronoi) / Popular Times",
        dashArray = "3",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          #fillOpacity = 0.7,
          bringToFront = TRUE),
        label = sprintf(
            "<strong>%s</strong><br/>%g",
            tt$Name, eval(parse(text = paste0 ('tt$',dt())  ))
          ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto")
        ) %>% 
      addCircleMarkers(data = p,
                       color = "red",
                       radius = 4,
                       stroke = FALSE,
                       fillOpacity = 1,
                       group = "Hawker Centres") %>%
      #----------------------------------------Peak Times
      addPolygons(
        fillColor = ~pal2(peak),
        weight = 1,
        opacity = 1,
        color = "white",
        group = "Service Area (Voronoi) / Peak Times",
        dashArray = "3",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          #fillOpacity = 0.7,
          bringToFront = TRUE),
        label = sprintf(
          "<strong>%s</strong><br/>%g",
          tt$Name, tt$peak
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto")
      ) %>%
      #-----------------------------------------Hawker Points
      addCircleMarkers(data = p,
                       color = "red",
                       radius = 4,
                       stroke = FALSE,
                       fillOpacity = 1,
                       group = "Hawker Centres") %>%
      
      
      addLayersControl(
        baseGroups = c("Toner Lite (default)", "OSM", "CartoDB"),
        overlayGroups = c("Hawker Centres", "Service Area (Voronoi) / Popular Times","Service Area (Voronoi) / Peak Times"),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      hideGroup("Service Area (Voronoi) / Peak Times")

  })
  
  observe({
    proxy <- leafletProxy("mymap", data = tt)
    proxy %>% clearControls()
    if (input$legend) {
      pal2 <- colorBin(c('deeppink2', "gray80", "khaki1", "darkgoldenrod1","gray35"), domain = tt$peak, bins = bin2)
      proxy %>% addLegend(position = 'bottomleft', pal = pal2, values = ~peak)
      eval(parse(text = paste0( "pal <- colorBin(palette = c('Black', brewer.pal(n=6,name='Reds')), domain = tt$",dt(),", bins = bins)")))
      eval(parse(text = paste0( "proxy %>% addLegend(position = 'bottomright', pal = pal, values = ~",dt(),")")))
      }
  })
}


#Run-------------------------------------------------------

shinyApp(ui, server)