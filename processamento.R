## Carregar pacotes, funções e dados ####

library(ggplot2)
source("CEP_regionalizado.r")

dados_completo <- read.csv("dados/GPS_FINAL_DATA.TXT")

## Preparar dados ####

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

## Teste da função media.regionalizada ####

## Calcular Média regionalizada
trans.rep0 <- media.regionalizada(dados_lista[[1]], janela = 50 )
trans.rep0

 ## Plotar a média regionalizada
mp <- ggplot(data=trans.rep0[[ 2 ]], aes(y=latitude, x=longitude)) + 
  geom_raster(aes(fill=media)) + 
  scale_fill_continuous(na.value = 'white' ) +
  labs(x="Longitude", y="Latitude", fill="Velocidade média (km/h)") +
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

mp

ggsave("plots/media_reg_medicao_1.jpg", plot = mp, width = 12, height = 10)

## Teste da função limites.naturais ####

## Calcular os limites com as primeiras 5 medições
trans.limites <- limites.naturais(referencia = dados_lista[[1]], 
                                  lista.arquivos = dados_lista[2:ultima_med_cont], janela =  50 )
trans.limites 

mp2 <- ggplot(data=trans.limites[["limites"]], aes(y=latitude, x=longitude)) + 
  geom_raster(aes(fill=LNS)) +
  scale_fill_continuous(na.value = 'white' ) +
  labs(x="Longitude", y="Latitude", fill="Limite natural superior") +
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
mp2

ggsave("plots/media_reg_all_lns.jpg", plot = mp2, width = 12, height = 10)

mp3 <- ggplot(data=trans.limites[["limites"]], aes(y=latitude, x=longitude)) + 
  geom_raster(aes(fill=LNI)) +
  scale_fill_continuous(na.value = 'white' ) +
  labs(x="Longitude", y="Latitude", fill="Limite natural inferior") +
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
mp3

ggsave("plots/media_reg_all_lni.jpg", plot = mp3, width = 12, height = 10)


## Testar os limites com a ultima medição
#teste_1 <- verificar.dados(arquivo = dados_lista[[medicao_teste]], limites = trans.limites)
dados_teste <- data.cleaner(read.csv("dados/GPS_13-12.TXT"))
teste_1 <- verificar.dados(arquivo = dados_teste, limites = trans.limites)
teste_1

## Teste 1 grafico
mp_teste <- ggplot(data=na.omit(teste_1), aes(y=latitude, x=longitude)) +
  geom_raster(aes(fill=check)) + 
  theme_bw() +
  labs(x="Longitude", y="Latitude",  fill="Controle") +
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

mp_teste

ggsave("plots/media_teste.jpg", plot = mp_teste, width = 12, height = 10)






