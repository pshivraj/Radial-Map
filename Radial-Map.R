library(ggmap)
library(sp)
library(rgdal)
library(rgeos)
#load city data
hamburg <- read.csv("ham.csv")
# get the stadium coordinates
ham <- geocode("hamburg")
stadium <- as.data.frame(geocode("hamburg"))
# create spatialPoint object
coordinates(ham) <- ~ lon + lat
proj4string(ham) <- CRS("+init=epsg:4326")

# reproject to Google Mercator (meters)
ham.mrc <- spTransform(ham, CRS = CRS("+init=epsg:3857"))

# concentric rings (in miles):
dist.miles <-  seq(5,40, 5)

# create a dataframe with the circle coordinates
circ.data <- do.call(rbind,
                     lapply(dist.miles,function(n){
                       circ <- gBuffer(ham.mrc, width = n * 1000, quadsegs=20)
                       circ.t <- spTransform(circ, CRS=CRS("+init=epsg:4326"))
                       coords <- lapply(circ.t@polygons, function(x) {x@Polygons[[1]]@coords})
                       data.frame(x=coords[[1]][,1], y=coords[[1]][,2], distance=n)
                     }))
# text positions
text.pos <- cbind(aggregate( y ~ distance, data=circ.data, FUN=min), x=ham$lon, row.names = NULL)

# get basemap
ham.map <- get_googlemap("hamburg",maptype = "roadmap", color = "bw", scale = 2,zoom=11)
# plot
ggmap(ham.map) + 
  geom_path(data=circ.data, aes(x=x, y=y, group=distance), alpha=0.5) +
  geom_point(
    aes(x=longitude, y=latitude), colour="#00667E", 
    data=hamburg, alpha=0.8, na.rm = T,size= na.omit((hamburg$conv)/5))+
  geom_point(data=stadium, mapping=aes(x=lon, y=lat), alpha=1,size=3) +
  geom_text(data=text.pos, aes(x=x, y=y, label=paste0(distance,"km")))
