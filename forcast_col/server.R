library(shiny)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)

source("config.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #create slider input depending on data frequency
  observe({

    allDates <- unique(depto$FECCORTE)
    output$dateUI <- renderUI({
      sliderInput(inputId = "dateSel",label = "Fecha",
                  min = min(allDates),
                  max = max(allDates),
                  value = min(allDates),
                  step = 30,
                  timeFormat = "%b %y",
                  animate = animationOptions(interval = 2000, loop = FALSE)
      )
    })
  })

  #filter data depending on selected date
  filteredData <- reactive({

    date = req(input$dateSel)
      if(date==17440){
      full.date = date
    }
    else{
      full.date <- as.POSIXct(date, tz="GMT")
      full.date <- as.character(monthStart(full.date))
    }
    new = depto[depto$FECCORTE == full.date,]
    new
  })

  #create the base leaflet map
  # output$map <- renderLeaflet({
  #
  #   leaflet(depto) %>%
  #     addTiles()  %>%
  #     setView(lat =  4.1645646, lng = -71.7172296, zoom = 5) %>%
  #
  #     addPolygons(
  #       layerId = ~DPTO,
  #       fillColor = ~colorQuantile("YlOrRd", AREA)(AREA),
  #       stroke = TRUE,
  #       opacity = 1.0,
  #       fillOpacity = 1,
  #       color = "#444444",
  #       smoothFactor = 0.5,
  #       weight = 1
  #     ) %>%
  #
  #     #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
  #     leaflet::addLegend(pal = colorPalette, values = df_depto_agg$n_creditos, opacity = 0.9, title = "n_creditos", position = "bottomleft")
  #
  # })

  output$map <- renderLeaflet({
    leaflet(depto) %>%
      addTiles() %>%
      setView(lat =  4.1645646, lng = -71.7172296, zoom = 5) #%>%

  })

  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({

    data = filteredData()

    state_popup <- paste0("<strong>Country: </strong>",
                          data$NOMBRE_DPT)

    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", AREA)(AREA),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                       bringToFront = TRUE))

  #   depto$n_creditos <- filteredData()$n_creditos[match(depto$DPTO, filteredData()$COD_DPT)]
  #
  #   depto@data$LabelText <- paste0(
  #     "<b>Country:</b> ", depto@data$NAME,"<br>",
  #     "<b>n_creditos:</b> ", format(depto@data$n_creditos, nsmall=0, big.mark=","))
  #
  #   if(input$mapType == "Markers"){
  #
  #     leafletProxy("map", data = depto) %>%
  #       clearMarkers() %>%
  #       setShapeStyle(layerId = ~DPTO, fillColor = "lightgray") %>%
  #       addCircleMarkers(lng = ~LON,
  #                        lat = ~LAT,
  #                        radius = ~log(n_creditos) * 2,
  #                        weight = 1,
  #                        opacity = 1,
  #                        color = ~ifelse(n_creditos > 0, "black", "transparent"),
  #                        fillColor = ~ifelse(n_creditos > 0, colorPalette(n_creditos), "transparent"),
  #                        fillOpacity = 0.8,
  #                        label = ~lapply(LabelText, htmltools::HTML))
  #
  #   }else if(input$mapType == "Choropleth"){
  #
  #     leafletProxy("map",
  #                  data = depto) %>%
  #       clearMarkers() %>%
  #       setShapeStyle(layerId = ~DPTO, fillColor = ~ifelse(n_creditos > 0, colorPalette(n_creditos), "lightgray"), label = depto$LabelText)
  #
  #   }
  })
  #
  output$texto <- renderText({

    # dim(filteredData())
    input$dateSel

  })

})
