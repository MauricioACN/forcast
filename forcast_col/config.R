#helper function for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){

  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))

  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('

window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)


#you only have to do this once!
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
#system("unzip world_shape_file.zip")

#load spatial data
# world_spdf <- readOGR(
#   dsn = getwd() ,
#   layer = "TM_WORLD_BORDERS_SIMPL-0.3",
#   verbose = FALSE
# )

# #load covid data set
# covidData <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
# covidData <- na.omit(covidData)
# covidData$Date_reported <- as.Date(covidData$Date_reported)
#
# #select a certain date
# selectedData <- covidData[covidData$Date_reported == "2020-07-15", ]
#
# #match cases and spatial data via ISO2/Country Code
# world_spdf$Cases <- selectedData$Cumulative_cases[match(world_spdf$ISO2, selectedData$Country_code)]

# #create label texts
# world_spdf@data$LabelText <- paste0(
#   "<b>Country:</b> ", world_spdf@data$NAME,"<br>",
#   "<b>Cases:</b> ", format(world_spdf@data$Cases, nsmall=0, big.mark=","))

# #define colorpalette for chart legend
# paletteBins <- c(0, 50000, 100000, 250000, 500000, 1000000, 2500000, 5000000, 10000000)
# colorPalette <- colorBin(palette = "YlOrBr", domain = df_full_agg$n_creditos, na.color = "transparent", bins = paletteBins)
