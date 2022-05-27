

library(leaflet)
library(rgdal)
library(rmapshaper)
library(raster)
library(sf)
library(spData)


data(us_states)
# 1.0 download the map
#https://gadm.org/download_country.html
#3USA <- raster::getData("GADM", country= "USA", level=0)
# Convert to sf object via st_as_sf() -> results in multipolygon
##bp_sf <- sf::st_as_sf(USA)
# object can be subdivided into single polygons using st_cast()
##bps_sf <- sf::st_cast(bp_sf, "POLYGON")
#filter on area
#BpSf <- bps_sf[as.numeric(sf::st_area(bps_sf))>=10000,]

library(RColorBrewer)
n <- 100
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))
states<<-us_states$NAME %>% unique()
regions<<-us_states$REGION %>% unique()



plot_uk_map<-function(regional.filter){
#setup the colouring
  

  
#subset data

#BpSf <- bps_sf[bps_sf$NAME_1 %in% regional.filter,]
BpSf <- us_states[us_states$REGION %in% regional.filter,]

colour.df<-data.frame(region=states,col=col_vector[1:length(states)] )  
  
#colour.df<-colour.df[colour.df$region %in% regional.filter, ]

# 2. create a color vector region is a state
pal <- colorFactor(palette = colour.df$col, domain = BpSf$NAME )



# 3. show  the map
#https://rquer.netlify.app/leaflet_maps_second_part/
BpSf  %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addPolygons(weight = 1,
              stroke = TRUE,
              color = "white",
              fillColor = ~pal(NAME),
              fillOpacity = 0.7,
              dashArray = "3",
              label = ~NAME,
              popup = ~paste("State/Union Territory:", NAME,
                             "<br/>",
                             "Country:", NAME),
              highlight = highlightOptions(
                weight = 2,
                dashArray = "",
                color = "grey",
                bringToFront = TRUE
              )) %>%
  addLegend("topleft", pal = pal, values = ~NAME,
            title = "USA regions",
            labFormat = labelFormat(prefix = "-"),
            opacity = 1
  )

}