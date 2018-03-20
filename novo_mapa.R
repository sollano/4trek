install.packages("ggmap")
library(ggmap)

lon <- trans.rep0[[1]][c("lonmin","lonmax")]
lat <- trans.rep0[[1]][c("latmin","latmax")]
bbox <- make_bbox(lon, lat, f=0.05)

google_map <- get_map(bbox, maptype = "roadmap", source = "google", zoom=14)
google_map <- get_map(bbox, maptype = "satellite", source = "google", zoom=14)
google_map <- get_map(bbox, maptype = "hybrid", source = "google", zoom=14)

ggmap(google_map) +
  geom_raster(data=na.omit(trans.rep0[[ 2 ]]), aes(x=longitude, y=latitude, fill=media) ) +
  coord_cartesian(x = c(lon$lonmin - 0.001, lon$lonmax + 0.001),
                  y = c(lat$latmin - 0.003, lat$latmax + 0.003)  ) 
ggsave("plots/media_reg_medicao_1_novo.png",width = 12, height = 10)

  


  
