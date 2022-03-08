library(shiny)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #create slider input depending on data frequency
  observe({

    allDates <- unique(covidData$Date_reported)
    eligibleDates <- allDates[xts::endpoints(allDates, on = input$frequency)]

    if(input$frequency == "weeks"){
      stepSize = 7
    }else{
      stepSize = 1
    }

    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
                  min = min(eligibleDates),
                  max = max(eligibleDates),
                  value = min(eligibleDates),
                  step = stepSize,
                  timeFormat = "%d %b %y",
                  animate = animationOptions(interval = 500, loop = FALSE)
      )
    })
  })

  #filter data depending on selected date
  filteredData <- reactive({
    req(input$dateSel)
    covidData[covidData$Date_reported == input$dateSel, ]
  })

  #create the base leaflet map
  output$map <- renderLeaflet({

    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView(lat = 0, lng = 0, zoom = 2) %>%

      addPolygons(
        layerId = ~ISO2,
        fillColor = "lightgray",
        stroke = TRUE,
        fillOpacity = 1,
        color = "white",
        weight = 1
      ) %>%

      #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
      leaflet::addLegend(pal = colorPalette, values = covidData$Cumulative_cases, opacity = 0.9, title = "Cases", position = "bottomleft")

  })


  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({

    world_spdf$Cases <- filteredData()$Cumulative_cases[match(world_spdf$ISO2, filteredData()$Country_code)]

    world_spdf@data$LabelText <- paste0(
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>",
      "<b>Cases:</b> ", format(world_spdf@data$Cases, nsmall=0, big.mark=","))

    if(input$mapType == "Markers"){

      leafletProxy("map", data = world_spdf) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~ISO2, fillColor = "lightgray") %>%
        addCircleMarkers(lng = ~LON,
                         lat = ~LAT,
                         radius = ~log(Cases) * 2,
                         weight = 1,
                         opacity = 1,
                         color = ~ifelse(Cases > 0, "black", "transparent"),
                         fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "transparent"),
                         fillOpacity = 0.8,
                         label = ~lapply(LabelText, htmltools::HTML))

    }else if(input$mapType == "Choropleth"){

      leafletProxy("map", data = world_spdf) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~ISO2, fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "lightgray"), label = world_spdf$LabelText)

    }
})

})
