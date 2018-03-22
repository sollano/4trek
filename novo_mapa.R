#install.packages("ggmap")
library(ggmap)

lon <- trans.rep0[[1]][c("lonmin","lonmax")]
lat <- trans.rep0[[1]][c("latmin","latmax")]
bbox <- make_bbox(lon, lat, f=0.05)

google_map <- get_map(bbox, maptype = "roadmap", source = "google", zoom=14)
google_map <- get_map(bbox, maptype = "hybrid", source = "google", zoom=14)
google_map <- get_map(bbox, maptype = "satellite", source = "google", zoom=14)

mp_new <- ggmap(google_map) +
  geom_raster(data=na.omit(trans.rep0[[ 2 ]]), aes(x=longitude, y=latitude, fill=media) ) +
  coord_cartesian(x = c(lon$lonmin - 0.001, lon$lonmax + 0.001),
                  y = c(lat$latmin - 0.003, lat$latmax + 0.003)  ) +
  labs(x="Longitude", y="Latitude", fill="Velocidade média (km/h)") +
  ggthemes::theme_igray(base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20), 
    legend.title = element_text(size = 20),
    legend.key.width = unit( 2, units = "cm"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title   = element_text(size = 26,face="bold"), 
    axis.text    = element_text(size = 22),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    strip.text.x = element_text(size = 22)   )
  
mp_new

ggsave("plots/media_reg_medicao_1_novo.png", mp_new, width = 12, height = 10)

  


  
