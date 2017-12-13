## Preparar dados ####
source("CEP_regionalizado.r")

dados_completo <- read.csv("dados/GPS.TXT")
#dados_completo <- read.csv("dados/GPS_OK_DATA.TXT")

#dados_completo <- dados_completo[ ! dados_completo$Data %in% c("1/11/2017", "28/11/2017", "31/10/2017", "6/11/2017"),]

compara_medicoes(dados_completo, 50)
