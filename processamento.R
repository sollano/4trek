## Carregar pacotes, funções e dados ####

library(ggplot2)
source("CEP_regionalizado.r")

dados_completo <- read.csv("dados/GPS_OK_DATA.TXT")

## Preparar dados ####

## Verificar dados
str(dados_completo)
head(dados_completo)

## Remover alguns dados
#dados_completo <- dados_completo[ ! dados_completo$Data %in% c("1/11/2017", "28/11/2017", "31/10/2017"),]

## Limpar dados
dados_completo <- data.cleaner(dados_completo)

## Verificar dados novamente
str(dados_completo)
head(dados_completo)

## Transformar dados em lista, baseado na data de coleta
dados_lista <- split(dados_completo, dados_completo$Data)

## Determinar a ultima medicao, e a medicao teste
medicao_teste <- length(dados_lista)
ultima_med_cont <- length(dados_lista)-1

## Nomes dos objetos da lista
names(dados_lista)

## Teste da função media.regionalizada ####

## Calcular Média regionalizada
trans.rep0 <- media.regionalizada(dados_lista[[1]], janela = 50 )
trans.rep0

 ## Plotar a média regionalizada
mp <- ggplot(data=trans.rep0[[ 2 ]], aes(y=latitude, x=longitude))
mp + geom_raster(aes(fill=media)) + scale_fill_continuous(na.value = 'white' )

## Teste da função limites.naturais ####

## Calcular os limites com as primeiras 5 medições
trans.limites <- limites.naturais(referencia = dados_lista[[1]], 
                                  lista.arquivos = dados_lista[2:ultima_med_cont], janela =  50 )
trans.limites 

mp2 <- ggplot(data=trans.limites[["limites"]], aes(y=latitude, x=longitude))
mp2 + geom_raster(aes(fill=LNS)) + scale_fill_continuous(na.value = 'white' )

## Testar os limites com as medições 6 e 7
teste_1 <- verificar.dados(arquivo = dados_lista[[medicao_teste]], limites = trans.limites)
teste_1

## Teste 1 grafico
mp <- ggplot(data=na.omit(teste_1), aes(y=latitude, x=longitude))
mp + geom_raster(aes(fill=check)) + theme_bw()







