library(shiny)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)

# source("config.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #create slider input depending on data frequency

    allDates <- unique(df_full_agg$FECCORTE)
    eligibleDates <- allDates[xts::endpoints(allDates)]

    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
                  min = min(eligibleDates),
                  max = max(eligibleDates),
                  value = min(eligibleDates),
                  step = 3,
                  timeFormat = "%d %b %y",
                  animate = animationOptions(interval = 500, loop = FALSE)
      )
  })

  #filter data depending on selected date
  filteredData <- reactive({
    req(input$dateSel)
    df_full_agg[df_full_agg$FECCORTE == input$dateSel, ]
  })

  #create the base leaflet map
  output$map <- renderLeaflet({
    map = leaflet(deptos4)
    map = addTiles(map)
    map = addPolygons(map,
        fillColor = "lightgray",
        highlightOptions = highlightOptions(color = "red", weight = 2,
                                            bringToFront = TRUE),
        fillOpacity = 0.5,
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        label = lapply(LabelText, htmltools::HTML)
      )

    map <- map %>% leaflet::addLegend(pal = colorPalette,
                                        values = ~deptos4$prom_subsidio,
                                        opacity=0.9,
                                        title = "Promedio de Subsidio",
                                        position = "bottomleft")
    map
  })

  observe({

    df_filterd = filteredData()

    #create label texts
    LabelText <- paste0(
      "<b>Departamento:</b> ", df_filterd$NOMBRE_DPT,"<br>",
      "<b>Valor Subsidio:</b> ", format(df_filterd$prom_subsidio, nsmall=0, big.mark=","),"<br>",
      "<b>Cantidad Créditos:</b> ", format(df_filterd$n_creditos, nsmall=0, big.mark=","),"<br>",
      "<b>Cantidad Operaciones:</b> ", format(df_filterd$prom_operaciones, nsmall=0, big.mark=","),"<br>",
      "<b>Valor Créditos:</b> ", format(df_filterd$prom_millones, nsmall=0, big.mark=","),"<br>",
      "<b>Participación subsidiada:</b> ", format(round((df_filterd$prom_subsidio/df_filterd$prom_millones)*100,2), nsmall=0, big.mark=","))

    qpal <- colorQuantile("YlOrRd", df_filterd$prom_subsidio)(df_filterd$prom_subsidio)

    leafletProxy("map",data = df_filterd) %>% setShapeStyle(fillColor = ~ifelse(prom_subsidio > 0, colorPalette(prom_subsidio), "lightgray"), label = LabelText)
  })

})
