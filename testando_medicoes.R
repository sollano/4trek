## Preparar dados ####
source("CEP_regionalizado.r")

<<<<<<< HEAD
dados_completo <- read.csv("dados/GPS.TXT")
#dados_completo <- read.csv("dados/GPS_fixed.TXT")
=======
#dados_completo <- read.csv("dados/GPS_old.TXT")
dados_completo <- read.csv("dados/GPS.TXT")
#dados_completo <- read.csv("dados/GPS_fixit.TXT")
#dados_completo <- read.csv("dados/GPS_13-12.TXT")
>>>>>>> e0d8775be4db94888da225cd29f830dc308445b3
#dados_completo <- read.csv("dados/GPS_OK_DATA.TXT")

#dados_completo <- dados_completo[ ! dados_completo$Data %in% c("1/11/2017", "28/11/2017", "31/10/2017", "6/11/2017"),]
levels(dados_completo$Data)
#compara_medicoes(dados_completo[dados_completo$Data=="15/12/2017",], 50)
compara_medicoes(dados_completo, 50)
