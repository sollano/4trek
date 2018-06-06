# Dados e pacotes ####
#install.packages("ggmap")
library(ggmap)
source("CEP_regionalizado.r")

dados_completo <- read.csv("dados/GPS_FINAL_DATA.TXT")

## Verificar dados
str(dados_completo)
head(dados_completo)

## Remover alguns dados
dados_completo <- dados_completo[ ! dados_completo$Data %in% c("6/12/2017"),]

## Limpar dados
dados_completo <- data.cleaner(dados_completo)

## Verificar dados novamente
str(dados_completo)
head(dados_completo)

## Transformar dados em lista, baseado na data de coleta
dados_lista <- split(dados_completo, dados_completo$Data)

## Determinar a ultima medicao, e a medicao teste
#medicao_teste <- length(dados_lista)
ultima_med_cont <- length(dados_lista) #-1

## Nomes dos objetos da lista
names(dados_lista)

# ggcanvas ####
# Cria um objeto com as definicoes dos mapas para fonte, tamanho de fonte, tema, etc
ggcanvas <- ggthemes::theme_igray(base_family = "serif") +
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


# Criar mapa do google da área ####

## Calcular Média regionalizada
trans.rep0 <- media.regionalizada(dados_lista[[1]], janela = 50 )
trans.rep0

# obter max e min longitude
lon <- trans.rep0[[1]][c("lonmin","lonmax")]
# obter max e min latitude
lat <- trans.rep0[[1]][c("latmin","latmax")]
# criar bbox, uma caixa com os limites dos dados
bbox <- make_bbox(lon, lat, f=0.05)

# Pegar o mapa da google da caixa
google_map <- get_map(bbox, maptype = "satellite", source = "google", zoom=14)

# Mapa da Media regionalizada ####

## Calcular Média regionalizada
trans.rep0 <- media.regionalizada(dados_lista[[1]], janela = 50 )
trans.rep0

# Fazer um mapa com a caixa, e adicionar o trajeto
mp_mr <- ggmap(google_map) +
  geom_raster(data=na.omit(trans.rep0[[ 2 ]]), aes(x=longitude, y=latitude, fill=media) ) +
  coord_cartesian(x = c(lon$lonmin - 0.001, lon$lonmax + 0.001),
                  y = c(lat$latmin - 0.003, lat$latmax + 0.003)  ) +
  labs(x="Longitude", y="Latitude", fill="Velocidade média (km/h)") +
  ggcanvas

mp_mr

# Salvar mapa
#ggsave("plots/media_reg.png", mp_mr, width = 12, height = 10)

# Mapas dos limites naturais inferior e superior ####

trans.limites <- limites.naturais(referencia = dados_lista[[1]], 
                                  lista.arquivos = dados_lista[2:ultima_med_cont], janela =  50 )
trans.limites


# Fazer um mapa com a caixa, e adicionar o trajeto
mp_lim_sup <- ggmap(google_map) +
  geom_raster(data=na.omit(trans.limites[[ "limites" ]]), aes(x=longitude, y=latitude, fill=LNS) ) +
  coord_cartesian(x = c(lon$lonmin - 0.001, lon$lonmax + 0.001),
                  y = c(lat$latmin - 0.003, lat$latmax + 0.003)  ) +
  labs(x="Longitude", y="Latitude", fill="Limite natural superior") +
  ggcanvas

mp_lim_sup

# Salvar o mapa
#ggsave("plots/lns.png", mp_lim_sup, width = 12, height = 10)


mp_lim_inf <- ggmap(google_map) +
  geom_raster(data=na.omit(trans.limites[[ "limites" ]]), aes(x=longitude, y=latitude, fill=LNI) ) +
  coord_cartesian(x = c(lon$lonmin - 0.001, lon$lonmax + 0.001),
                  y = c(lat$latmin - 0.003, lat$latmax + 0.003)  ) +
  labs(x="Longitude", y="Latitude", fill="Limite natural inferior") +
  ggcanvas

mp_lim_inf

# Salvar o mapa
#ggsave("plots/lni.png", mp_lim_inf, width = 12, height = 10)




# Mapa do teste de uma medicao comparada aos limites ####

# Dados
dados_teste <- data.cleaner(read.csv("dados/GPS_13-12.TXT"))
teste_1 <- verificar.dados(arquivo = dados_teste, limites = trans.limites)
teste_1

# Fazer um mapa com a caixa, e adicionar o trajeto
mp_teste <- ggmap(google_map) +
  geom_raster(data=na.omit(teste_1), aes(x=longitude, y=latitude, fill=check) ) +
  coord_cartesian(x = c(lon$lonmin - 0.001, lon$lonmax + 0.001),
                  y = c(lat$latmin - 0.003, lat$latmax + 0.003)  ) +
  labs(x="Longitude", y="Latitude",  fill="Controle") +
  ggcanvas

mp_teste

# Salvar o mapa
#ggsave("plots/teste_1.png", mp_teste, width = 12, height = 10)

