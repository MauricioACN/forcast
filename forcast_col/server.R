library(shiny)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(plotly)

source("config.R")
source("models_functions.R",encoding = "utf-8")
# Define server logic required to draw a histogram
server <- function(input, output,session) {

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

  mapa_data <- reactive({

    date = req(input$dateSel)
    if(date==min(df_depto_agg$FECCORTE)){
      full.date = date
    }
    else{
      full.date <- as.POSIXct(date, tz="GMT")
      full.date <- as.character(monthStart(full.date))
    }
    new = df_depto_agg[df_depto_agg$FECCORTE == full.date,]
    # var = depto[depto$FECCORTE == full.date,input$var]
    new
  })

  output$map <- renderLeaflet({
    leaflet(df_depto_agg) %>%
      addTiles() %>%
      setView(lat =  4.1645646, lng = -71.7172296, zoom = 5) %>%
      leaflet::addLegend(pal = colorPalette, values = df_depto_agg$total_creditos, opacity = 0.9, title = "Valor de Créditos (MM)", position = "bottomleft")

  })

  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({

    data = mapa_data()

    total_sub = ifelse(data$total_subsidio>0,scales::dollar(data$total_subsidio,suffix = " MM"),"Sin Subsidio")
    part_sub = ifelse(data$total_subsidio>0,scales::percent(data$part_sub_cre),"Sin Subsidio")
    sector_sub = ifelse(data$porce_sect_sub>0,paste0(data$SECTOR_influyente," (",scales::percent(data$porce_sect_sub),")"),"Sin Subsidio")
    prod_sub = ifelse(data$porce_prod_sub>0,paste0(data$productor_influyente," (",scales::percent(data$porce_prod_sub),")"),"Sin Subsidio")

    data@data$state_popup <- paste0("<strong>Depto: </strong>",data$NOMBRE_DPT,"<br>",
                          "<strong>N° Créditos: </strong>",scales::number(data$no_creditos,big.mark = ","),"<br>",
                          "<strong>N° Operaciones: </strong>",scales::number(data$total_ope,big.mark = ","),"<br>",
                          "<strong>Total Valor Créditos: </strong>",scales::dollar(data$total_creditos,suffix = " MM"),"<br>",
                          "<strong>Total Valor Subsidios: </strong>",total_sub,"<br>",
                          "<strong>Participación Subsidios: </strong>",part_sub,"<br>",
                          "<strong>Sector Más Subsidiado: </strong>",sector_sub,"<br>",
                          "<strong>Prod. Más Subsidiado: </strong>",prod_sub
                          )

    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", total_creditos)(total_creditos),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                       bringToFront = TRUE),
                  label = ~lapply(state_popup,htmltools::HTML)
                  )

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
    value = scales::number(value,big.mark = ",")
    valueBox(value = value,subtitle = "Cantidad de Créditos")
  })

  output$value3 <- renderValueBox({

    value = length(filteredData()$prom_subsidio[filteredData()$prom_subsidio>0])
    valueBox(value = value,subtitle = "Cantidad de Subsidios")
  })

  output$value4 <- renderValueBox({
    value = sum(filteredData()$prom_millones)
    value = paste0(scales::dollar(round(value/1e6,0))," MM")
    valueBox(value = value,subtitle = "Total Valor Créditos")
  })

  output$value5 <- renderValueBox({

    value = sum(filteredData()$prom_subsidio)
    value = paste0(scales::dollar(round(value/1e6,0))," MM")
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
      xlab("Fecha") + ylab("Subsidios en Miles de Millones")+
      scale_y_continuous(labels=scales::dollar_format())+
      theme_classic()
  }
  else{
    p <- datos %>%
      ggplot(aes(x=FECCORTE, y=Total))+
      geom_line()+
      xlab("Fecha") + ylab("Subsidios en Miles de Millones")+
      scale_y_continuous(labels=scales::dollar_format())+
      theme_classic()
  }

  plotly::ggplotly(p)

  })

  # df_mpio_agg

  ################################## seccion de municipios ##################################

  #create slider input depending on data frequency
  observe({

    allDates_mp <- unique(mpio$FECCORTE)
    output$mpioUI <- renderUI({

      list(
      shinyWidgets::sliderTextInput(inputId = "dateSel_mp",
                                    label = "Trimestre",
                                    width = "100%",
                                    choices = allDates_mp,
                                    selected = allDates_mp[1],
                                    grid = FALSE,
                                    animate = animationOptions(interval = 2000)),
      shiny::selectInput(inputId = "depto_mpio_sel_1",
                         label = "Seleccion el Departamento a Analizar: ",
                         multiple = F,
                         choices = unique(mpio$NOMBRE_DPT),
                         selected = unique(mpio$NOMBRE_DPT)[1])
      )
    })
  })

  #filter data depending on selected date
  filteredData_mp <- reactive({

    date_mp = req(input$dateSel_mp)
    if(date_mp==min(mpio$FECCORTE)){
      full.date_mp = date_mp
    }
    else{
      full.date_mp <- as.POSIXct(date_mp, tz="GMT")
      full.date_mp <- as.character(monthStart(full.date_mp))
    }
    new_mp = mpio[mpio$FECCORTE == full.date_mp,]
    new_mp
  })

  mapa_data_mp <- reactive({

    date_mp_1 = req(input$dateSel_mp)
    if(date_mp_1==min(df_mpio_agg$FECCORTE)){
      full.date_mp1 = date_mp_1
    }
    else{
      full.date_mp1 <- as.POSIXct(date_mp_1, tz="GMT")
      full.date_mp1 <- as.character(monthStart(full.date_mp1))
    }

    cod_dept = unique(df_mpio_agg$DPTO[df_mpio_agg$NOMBRE_DPT==input$depto_mpio_sel_1])
    new_mp1 = df_mpio_agg[(df_mpio_agg$FECCORTE == full.date_mp1)&
                            (df_mpio_agg$DPTO == cod_dept),]
    new_mp1
  })

  output$map_mp <- renderLeaflet({

    colorPalette_mpio <- colorBin(palette = "YlOrBr", domain = mapa_data_mp()$total_creditos, na.color = "transparent", bins = paletteBins)

    leaflet(df_mpio_agg) %>%
      addTiles() %>%
      setView(lat =  4.1645646, lng = -71.7172296, zoom = 5) %>%
      leaflet::addLegend(pal = colorPalette_mpio, values = df_mpio_agg$total_creditos, opacity = 0.9, title = "Valor de Créditos (MM)", position = "bottomleft")

  })

  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({

    data_mp = mapa_data_mp()

    total_sub = ifelse(data_mp$total_subsidio>0,scales::dollar(data_mp$total_subsidio,suffix = " MM"),"Sin Subsidio")
    part_sub = ifelse(data_mp$total_subsidio>0,scales::percent(data_mp$part_sub_cre),"Sin Subsidio")
    sector_sub = ifelse(data_mp$porce_sect_sub>0,paste0(data_mp$SECTOR_influyente," (",scales::percent(data_mp$porce_sect_sub),")"),"Sin Subsidio")
    prod_sub = ifelse(data_mp$porce_prod_sub>0,paste0(data_mp$productor_influyente," (",scales::percent(data_mp$porce_prod_sub),")"),"Sin Subsidio")

    data_mp@data$state_popup <- paste0("<strong>Depto: </strong>",data_mp$NOMBRE_MPI,"<br>",
                                    "<strong>N° Créditos: </strong>",scales::number(data_mp$no_creditos,big.mark = ","),"<br>",
                                    "<strong>N° Operaciones: </strong>",scales::number(data_mp$total_ope,big.mark = ","),"<br>",
                                    "<strong>Total Valor Créditos: </strong>",scales::dollar(data_mp$total_creditos,suffix = " MM"),"<br>",
                                    "<strong>Total Valor Subsidios: </strong>",total_sub,"<br>",
                                    "<strong>Participación Subsidios: </strong>",part_sub,"<br>",
                                    "<strong>Sector Más Subsidiado: </strong>",sector_sub,"<br>",
                                    "<strong>Prod. Más Subsidiado: </strong>",prod_sub
    )

    leafletProxy("map_mp", data = data_mp) %>%
      clearShapes() %>%
      addPolygons(color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~ifelse(total_creditos>0,colorQuantile("YlOrRd", total_creditos)(total_creditos),"transparent"),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  label = ~lapply(state_popup,htmltools::HTML)
      )

  })

  output$texto_mp <- renderText({

    paste0(unique(filteredData_mp()$FECCORTE))

  })

  output$value7 <- renderValueBox({
    valueBox(value = textOutput("texto_mp"),
             subtitle = "Fecha de Análisis",width = 2)
  })

  output$value8 <- renderValueBox({

    value = sum(filteredData_mp()$n_creditos)
    value = scales::number(value,big.mark = ",")
    valueBox(value = value,subtitle = "Cantidad de Créditos")
  })

  output$value9 <- renderValueBox({

    value = length(filteredData_mp()$prom_subsidio[filteredData_mp()$prom_subsidio>0])
    valueBox(value = value,subtitle = "Cantidad de Subsidios")
  })

  output$value10 <- renderValueBox({
    value = sum(filteredData_mp()$prom_millones)
    value = paste0(scales::dollar(round(value/1e6,0))," MM")
    valueBox(value = value,subtitle = "Total Valor Créditos")
  })

  output$value11 <- renderValueBox({

    value = sum(filteredData_mp()$prom_subsidio)
    value = paste0(scales::dollar(round(value/1e6,0))," MM")
    valueBox(value = value,subtitle = "Total Valor Subsidios",width = 2)

  })

  output$value12 <- renderValueBox({
    value1 = sum(filteredData_mp()$prom_millones)
    value2 = sum(filteredData_mp()$prom_subsidio)
    value = paste0(format(round(value2/value1*100,2),trim = TRUE), "%")
    valueBox(value = value,subtitle = "Proporción Créd. Subsidios",width = 2)
  })

  output$plot_sector_mp <- renderPlotly({

    prueba =  filteredData_mp()@data %>% group_by(SECTOR) %>%
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

  output$standarUI <- renderUI({

    list(
      dateRangeInput(inputId = "mpio_rango_fecha",
                     label = "Seleccione el periodo de tiempo a analizar:",
                     start = min(mpio@data$FECCORTE),
                     end = max(mpio@data$FECCORTE),
                     min = min(mpio@data$FECCORTE),
                     max = max(mpio@data$FECCORTE),format = "yyyy-mm",
                     language = "es",weekstart = 1,startview = "year"),
    selectInput(inputId = "mpio_depto_Sel",
                label = "Seleccione el o los departamentos a analizar:",
                "",multiple = T),
    selectInput(inputId = "mpioSel",
                label = "Seleccione el o los departamentos a analizar:",
                "",multiple = T),
    selectInput(inputId = "mpio_sectorSel",
                label = "Seleccione el o los sectores a analizar:",
                "",multiple = T),
    selectInput(inputId = "mpio_prodSel",
                label = "Seleccione el o los tipos de productor a analizar:",
                "",multiple = T)
    )
  })

  #filter data depending on selected date
  mpio_date_filter <- reactive({
    mpio@data %>% dplyr::filter(FECCORTE >= as.Date(input$mpio_rango_fecha[1],"%Y-%m-01") & FECCORTE <= as.Date(input$mpio_rango_fecha[2],"%Y-%m-01"))
  })

  observeEvent(input$mpio_rango_fecha,{
    updateSelectInput(session = session,
                      inputId = "mpio_depto_Sel",
                      choices = c("Todos",unique(mpio_date_filter()$NOMBRE_DPT)),
                      selected = "Todos")
  })

  depto_mpio_filter <- reactive({
    if (!"Todos" %in% input$mpio_depto_Sel){
      mpio_date_filter() %>% dplyr::filter(NOMBRE_DPT %in% input$mpio_depto_Sel)
    }
    else{
      mpio_date_filter()
    }
  })

  observeEvent(input$mpio_depto_Sel,{
          updateSelectInput(session = session,
                            inputId = "mpioSel",
                            choices = c("Todos",unique(depto_mpio_filter()$NOMBRE_MPI)),
                            selected = "Todos")
  })

  mpio_filter <- reactive({
    if (!"Todos" %in% input$mpioSel){
      depto_mpio_filter() %>% dplyr::filter(NOMBRE_DPT %in% input$mpioSel)
    }
    else{
      depto_mpio_filter()
    }
  })

  observeEvent(input$mpio_depto_Sel,{
    updateSelectInput(session = session,
                      inputId = "mpio_sectorSel",
                      choices = c("Todos",unique(mpio_filter()$SECTOR)),
                      selected = "Todos")
  })

  mpio_sector_filter <- reactive({
    if (!"Todos" %in% input$mpio_sectorSel){
      mpio_filter() %>% dplyr::filter(SECTOR %in% input$mpio_sectorSel)
    }
    else{
      mpio_filter()
    }
  })

  observeEvent(input$mpio_sectorSel,{
    updateSelectInput(session = session,
                      inputId = "mpio_prodSel",
                      choices = c("Todos",unique(mpio_sector_filter()$TIPO_PRODUCTOR)),
                      selected = "Todos")
  })

  mpio_productor_filter <- reactive({
    if (!"Todos" %in% input$mpio_prodSel){
      mpio_sector_filter() %>% dplyr::filter(TIPO_PRODUCTOR %in% input$mpio_prodSel)
    }
    else{
      mpio_sector_filter()
    }
  })

  mpio_data_part <- reactive({
    data_ = mpio_productor_filter()
    data =  data_ %>%
      dplyr::group_by(FECCORTE,SECTOR,NOMBRE_MPI) %>%
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

  output$part_graf_mpio <-renderPlotly({

    df = mpio_data_part()
    #updated plot_ly function call
    fig <- plot_ly(data = df,x = ~FECCORTE, y = ~Total, type = 'bar', text = ~Participacion, name = ~SECTOR, color = ~SECTOR)
    fig <- fig %>% layout(xaxis = list(tickvals = ~FECCORTE, tickformat = "%b %Y", tickfont = list(size = 12)), barmode = 'stack')
    fig

  })

  data_hist_mpio <- reactive({
    data_ = mpio_productor_filter()
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


  output$hist_graf_mpio <- renderPlotly({

    datos = data_hist_mpio()
    datos$Total = datos$Total/1000
    sectors = length(unique(datos$SECTOR))

    if(sectors>1){

      p <- datos %>%
        ggplot(aes(x=FECCORTE, y=Total, group=SECTOR, color=SECTOR))+
        geom_line()+
        xlab("Fecha") + ylab("Subsidios en Miles de Millones")+
        scale_y_continuous(labels=scales::dollar_format())+
        theme_classic()
    }
    else{
      p <- datos %>%
        ggplot(aes(x=FECCORTE, y=Total))+
        geom_line()+
        xlab("Fecha") + ylab("Subsidios en Miles de Millones")+
        scale_y_continuous(labels=scales::dollar_format())+
        theme_classic()
    }

    plotly::ggplotly(p)

  })


  ######################## Proyecciones ############################

  output$proyeUI <- renderUI({
    list(
      shiny::selectInput(inputId = "proye_depto",
                         label = "Seleccion el Departamento a Analizar: ",
                         multiple = F,
                         choices = unique(depto$NOMBRE_DPT),
                         selected = unique(depto$NOMBRE_DPT)[1]),
    shiny::selectInput(inputId = "time_forecast",
                       label = "Seleccion la cantidad de meses que desea pronosticar: ",
                       multiple = F,
                       choices = c(1,6,12,24),
                       selected = 12),
    shiny::selectInput(inputId = "model",
                       label = "Seleccion el o los modelos que desea aplciar: ",
                       multiple = T,
                       choices = c("Pronóstico Naive",
                                   "Modelo Exponencial Suavizado",
                                   "Pronóstico de Holt-Winters",
                                   "Pronóstico TBATS",
                                   "Auto Arima",
                                   "Sarima"),
                       selected = "Auto Arima"),
  shiny::textOutput(outputId = "texto_para_model")
    )

  })

  output$texto_para_model <- renderText({

    if(length(input$model)>1){

      "Se aplicará el promedio de los modelos seleccionados"
    }

  })

  data_model_trans <- reactive({

    data_model %>% filter(COD_DPT==input$proye_depto)

  })

}


