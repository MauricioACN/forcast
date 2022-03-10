library(shiny)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)

source("config.R")

# Define server logic required to draw a histogram
server <- function(input, output) {

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

  observe({

    allvars <- c("n_creditos","prom_operaciones","prom_millones","prom_subsidio")
    output$varUI <- renderUI({
      varSelectInput(inputId = "var",
                  label = "Seleccione la variable de interes:",
                  data = depto@data,selected = allvars[1])
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
    # var = depto[depto$FECCORTE == full.date,input$var]
    new
  })

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
                  fillColor = ~colorQuantile("YlOrRd",AREA)(AREA),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                       bringToFront = TRUE),
                  popup = state_popup)

  })

  output$texto <- renderText({

    paste0(unique(filteredData()$FECCORTE))

  })

  output$value1 <- renderValueBox({
    valueBox(value = textOutput("texto"),
             subtitle = "Fecha de Análisis",width = 2)
  })

  output$value2 <- renderValueBox({

    value = sum(filteredData()$n_creditos)
    valueBox(value = value,subtitle = "Cantidad de Créditos")
  })

  output$value3 <- renderValueBox({

    value = length(filteredData()$prom_subsidio[filteredData()$prom_subsidio>0])
    valueBox(value = value,subtitle = "Cantidad de Subsidios")
  })

  output$value4 <- renderValueBox({
    valueBox(value = textOutput("texto"),
             subtitle = "Fecha de Análisis",width = 2)
  })

  output$value5 <- renderValueBox({
    valueBox(value = textOutput("texto"),
             subtitle = "Fecha de Análisis",width = 2)
  })

  output$value6 <- renderValueBox({
    valueBox(value = textOutput("texto"),
             subtitle = "Fecha de Análisis",width = 2)
  })

}
