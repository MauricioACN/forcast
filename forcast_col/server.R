library(shiny)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(plotly)

source("config.R")

# Define server logic required to draw a histogram
server <- function(input, output) {

  #create slider input depending on data frequency
  observe({

    allDates <- unique(depto$FECCORTE)
    output$dateUI <- renderUI({

      shinyWidgets::sliderTextInput(inputId = "dateSel",
                                    label = "Trimestre",
                                    width = "100%",
                                    choices = allDates,
                                    selected = allDates[1],
                                    grid = FALSE,
                                    animate = animationOptions(interval = 2000)
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
      if(date==min(depto$FECCORTE)){
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
    value = sum(filteredData()$prom_millones)
    value = paste0("$",format(round(value/1e6, 0), trim = TRUE), " M")
    valueBox(value = value,subtitle = "Total Valor Créditos")
  })

  output$value5 <- renderValueBox({

    value = sum(filteredData()$prom_subsidio)
    value = paste0("$",format(round(value/1e6, 0), trim = TRUE), " M")
    valueBox(value = value,subtitle = "Total Valor Subsidios",width = 2)

  })

  output$value6 <- renderValueBox({
    value1 = sum(filteredData()$prom_millones)
    value2 = sum(filteredData()$prom_subsidio)
    value = paste0(format(round(value2/value1*100,2),trim = TRUE), "%")
    valueBox(value = value,subtitle = "Proporción Créd. Subsidios",width = 2)
  })

  output$plot_sector <- renderPlotly({

    prueba =  filteredData()@data %>% group_by(SECTOR) %>%
      summarise(Total_creditos = sum(n_creditos,na.rm = T),
                Total_Ope = sum(prom_operaciones,na.rm = T),
                Total_Valor_Cred = sum(prom_millones,na.rm = T),
                Total_Valor_Subs = sum(prom_subsidio, na.rm = T)) %>%
      arrange(desc(Total_Valor_Subs)) %>%
      as.data.frame()

    prueba = prueba[c(1:5),]

    prueba = prueba %>% arrange(desc(Total_Valor_Subs))

    fig <- plotly::plot_ly(
      x = prueba$SECTOR,
      y = prueba$Total_Valor_Subs,
      name = "Total Valor Subsidios",
      type = "bar"
    )

    fig

  })


  observe({

    alldeptos <- unique(depto$NOMBRE_DPT)
    allsector <- unique(depto$SECTOR)
    allproductor <- unique(depto$TIPO_PRODUCTOR)
    allDates <- unique(depto$FECCORTE)

    output$deptoUI <- renderUI({
      list(
      selectInput(inputId = "deptoSel",
                  label = "Seleccione el o los departamentos a analizar:",
                  choices = c("Todos",alldeptos),
                  selected = "Todos",
                  multiple = T),
      selectInput(inputId = "sectorSel",
                  label = "Seleccione el o los sectores a analizar:",
                  choices = c("Todos",allsector),
                  selected = "Todos",
                  multiple = T),
      selectInput(inputId = "prodSel",
                  label = "Seleccione el o los tipos de productor a analizar:",
                  choices = c("Todos",allproductor),
                  selected = "Todos",
                  multiple = T),
      dateRangeInput(inputId = "rango_fecha",
                     label = "Seleccione el periodo de tiempo a analizar:",
                     start = min(allDates),
                     end = max(allDates),
                     min = min(allDates),
                     max = max(allDates),format = "yyyy-mm",
                     language = "es",weekstart = 1,startview = "year")
    )
    })
  })

  #filter data depending on selected date
  date_filter <- reactive({
      depto@data %>% dplyr::filter(FECCORTE >= as.Date(input$rango_fecha[1],"%Y-%m-01") & FECCORTE <= as.Date(input$rango_fecha[2],"%Y-%m-01"))
  })

  depto_filter <- reactive({
    if (!"Todos" %in% input$deptoSel){
      date_filter() %>% dplyr::filter(NOMBRE_DPT %in% input$deptoSel)
    }
    else{
      date_filter()
    }
  })

  sector_filter <- reactive({
      if (!"Todos" %in% input$sectorSel){
      depto_filter() %>% dplyr::filter(SECTOR %in% input$sectorSel)
    }
    else{
      depto_filter()
    }
  })

  productor_filter <- reactive({
    if (!"Todos" %in% input$prodSel){
      sector_filter() %>% dplyr::filter(TIPO_PRODUCTOR %in% input$prodSel)
    }
    else{
      sector_filter()
    }
  })

  data_part = reactive({
    data_ = productor_filter()
    data =  data_ %>%
      dplyr::group_by(FECCORTE,SECTOR,NOMBRE_DPT) %>%
      dplyr::summarise(Total_creditos = n(),
                Total_operaciones = sum(prom_operaciones,na.rm=T),
                Total_millones = sum(prom_millones,na.rm=T),
                Total_subsidio = sum(prom_subsidio,na.rm=T))

    df = data %>%
      group_by(FECCORTE,SECTOR) %>%
      summarise(Total = sum(Total_subsidio)) %>%
      mutate(Participacion = round(Total/sum(Total)*100,2))
    df
  })

    output$part_graf_depto <-renderPlotly({

    df = data_part()
    #updated plot_ly function call
    fig <- plot_ly(data = df,x = ~FECCORTE, y = ~Total, type = 'bar', text = ~Participacion, name = ~SECTOR, color = ~SECTOR)
    fig <- fig %>% layout(xaxis = list(tickvals = ~FECCORTE, tickformat = "%b %Y", tickfont = list(size = 12)), barmode = 'stack')
    fig

  })

  data_hist <- reactive({
    data_ = productor_filter()
    data =  data_ %>%
      dplyr::group_by(FECCORTE,SECTOR,NOMBRE_DPT) %>%
      dplyr::summarise(Total_creditos = n(),
                       Total_operaciones = sum(prom_operaciones,na.rm=T),
                       Total_millones = sum(prom_millones,na.rm=T),
                       Total_subsidio = sum(prom_subsidio,na.rm=T))
    df = data %>%
      group_by(FECCORTE,SECTOR) %>%
      summarise(Total = sum(Total_subsidio,na.rm = T))
    # df = tidyr::spread(data = df,key = SECTOR,value = Total,fill = 0)
    df
  })


  output$hist_graf_depto <- renderPlotly({

  datos = data_hist()
  datos$Total = datos$Total/1000
  sectors = length(unique(datos$SECTOR))

  if(sectors>1){

    p <- datos %>%
      ggplot(aes(x=FECCORTE, y=Total, group=SECTOR, color=SECTOR))+
      geom_line()+
      xlab("Fecha") + ylab("Subsidios en Miles de Millones")
      theme_classic()
  }
  else{
    p <- datos %>%
      ggplot(aes(x=FECCORTE, y=Total))+
      geom_line()+
      xlab("Fecha") + ylab("Subsidios en Miles de Millones")
    theme_classic()
  }

  plotly::ggplotly(p)

  })

}


