## Preparar dados ####
source("CEP_regionalizado.r")
# ultima medicao: 5/12/2017
x <- read.csv("dados/GPS.TXT")
x$Data <- "6/12/2017"
#dados_completo <- read.csv("dados/GPS_fixed.TXT")
#dados_completo <- read.csv("dados/GPS_FINAL_DATA.TXT")
dados_completo <- rbind(dados_completo, x)

#dados_completo <- dados_completo[ ! dados_completo$Data %in% c("1/11/2017", "28/11/2017", "31/10/2017", "6/11/2017"),]
levels(dados_completo$Data)
compara_medicoes(dados_completo, 50)

write.csv(dados_completo, "dados/GPS_FINAL_DATA.TXT", row.names = F)
