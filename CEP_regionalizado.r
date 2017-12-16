# Graus para radiano
degrees.to.radians<-function(decimaldegrees=45.5){
  if(!is.numeric(decimaldegrees)) stop("Please enter a numeric value for minutes!\n")
  radians<-decimaldegrees*pi/180
  return(radians)
}

# Distancia entre grausdecimais
distance.Decdegree = function(lon1, lat1, lon2, lat2){
    lon1 = degrees.to.radians(lon1)
    lat1 = degrees.to.radians(lat1)
    lon2 = degrees.to.radians(lon2)
    lat2 = degrees.to.radians(lat2)
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * atan2(sqrt(a), sqrt(1-a))
    d = (6373 * c) * 1000
    return(d)
}

# Converte graus em metros
decimaldegrees.to.meters = function(degree){
  distance = degree * 111325
  return(distance)
}

# Converte metros em graus
meters.to.decimaldegrees = function(distance){
  degree = distance / 111325
  return(degree)
}

# Calcula média regionalizada de uma variável
media.regionalizada = function(file, janela = 50, latlon = NA){

  janela.graus = meters.to.decimaldegrees(janela)
  i = 1
  media.longitude = NA
  media.latitude = NA
  media.media = NA
  media.desvpad = NA
  
  if(any(is.na(latlon))){
    lonmin = min(file$Longitude)
    lonmax = max(file$Longitude)
    latmin = min(file$Latitude)
    latmax = max(file$Latitude)
  } else {
    lonmin = latlon[1,1]
    lonmax = latlon[1,2]
    latmin = latlon[1,3]
    latmax = latlon[1,4]
  }
  
  for(lon.min in seq(lonmin, lonmax, janela.graus)){
    for (lat.min in seq(latmin, latmax, janela.graus)){
    
      dados.janela = file[file$Latitude > lat.min & 
                            file$Latitude < lat.min + janela.graus &
                            file$Longitude > lon.min & 
                            file$Longitude < lon.min + janela.graus,]
      
      media.longitude[i] = lon.min
      media.latitude[i] = lat.min
      if(dim(dados.janela)[1] != 0){
        media.media[i] = mean(dados.janela$Velocidade)
        media.desvpad[i] = sd(dados.janela$Velocidade)
      } else {
        media.media[i] = NA
        media.desvpad[i] = NA
      }
      i = i + 1
      
    }
  }
  regiao = data.frame(lonmin, lonmax, latmin, latmax)
  
  media.regionalizada = data.frame(longitude = media.longitude, 
                                   latitude = media.latitude, 
                                   media = media.media,
                                   desvpad = media.desvpad)
  
  return(list(regiao, media.regionalizada))
}

# Calcular os limites naturais da atividade
limites.naturais = function(referencia, lista.arquivos, janela = 50){

  
  lst.referencia = media.regionalizada(referencia, janela = janela)
  
  res.latlon = data.frame(
    longitude = lst.referencia[[2]]$longitude, 
    latitude  = lst.referencia[[2]]$latitude  )
  
  res.media = data.frame(lst.referencia[[2]]$media)
  
  for (f in lista.arquivos){
    f.temp = media.regionalizada(f, janela = janela, latlon = lst.referencia[[1]])
    res.media = cbind(res.media, f.temp[[2]]$media)
  }
  
  media.atividade = apply(res.media, MARGIN = 1, FUN = mean)
  desvpad.atividade = apply(res.media, MARGIN = 1, FUN = sd)
  LNI = media.atividade - 3 * desvpad.atividade
  LNS = media.atividade + 3 * desvpad.atividade
  
  return(list(janela = janela, min.max = lst.referencia[[1]], limites = data.frame(res.latlon, media.atividade, desvpad.atividade, LNI, LNS)))
}

# Verificar CEP
verificar.dados = function(arquivo, limites){
  check = NA
  
  arquivo.media = media.regionalizada(arquivo, janela = limites[[1]], latlon = limites[[2]])[[2]]
  
  for (i in seq(1, dim(limites[[3]])[1], 1)){
    #print(i)
    if(is.na(arquivo.media$media[i]) | is.na(limites[[3]]$LNI[i])){
      check[i] = NA
    } else if (arquivo.media$media[i] <= limites[[3]]$LNS[i] & arquivo.media$media[i] >= limites[[3]]$LNI[i]){
      check[i] = 0
    } else {
      check[i] = 1
    }
  }
  arquivo.media$check = check
  return(arquivo.media)
}

# Limpar dados
data.cleaner <- function(file){
  
  # Remove espacos no comeco e final dos dados
  file <- as.data.frame(apply(file, 2, trimws, "both"))
  
  # Remove datas em que o dia é zero (erros de medicao)
  file <- file[as.numeric(format(as.Date(file$Data, format=c("%d/%m/%Y")),"%d"))!=0,]
  
  # Remove linhas com o cabecalho adicionais
  file <- file[file[["Latitude"]] != "Latitude" & !is.na(file[["Latitude"]]),]
  
  # Converte valores para numerico
  file[c("Latitude", "Longitude", "Velocidade", "Temperatura", "Umidade")] <-sapply(
    file[c("Latitude", "Longitude", "Velocidade", "Temperatura", "Umidade")], 
    function(x) {as.numeric(as.character(x)) } ) #para se converter fator para numerico,
  # e preciso converter para character primeiro
  
  #remover medicoes que sao zero (erros de medicao)
  file <- file[file[["Latitude"]] != 0 & file[["Longitude"]] != 0, ]
  
  #remover ou alterar depois... Especifico para os dados utilizados
  file <- file[file[["Latitude"]] < 0 & file[["Longitude"]] < 0, ]
  
  
  # Remover niveis inutilizados
  file <- droplevels(file)
  
  return(file)
  
}

# Plota um grafico comparando todas as medicoes do dado,
# utilizando a primeira medicao como base
compara_medicoes <- function(file, janela){
  
  file <- data.cleaner(file)
  
  ## Transformar dados em lista, baseado na data de coleta
  file_lista <- split(file, file$Data)
  
  valor_janela <- janela
  
  length(file_lista)
  
  ## Calcular Média regionalizada da primeira medicao
  trans.rep1 <- media.regionalizada(file_lista[[1]], janela = valor_janela )
  
  # calcular media para as demais e salvar em um dataframe
  file_test_plot <- as.data.frame(do.call(rbind, lapply(file_lista, function(x){
    
    y <- media.regionalizada(x, janela=valor_janela, latlon=trans.rep1[[1]])[[2]]
    y$Data <- x$Data[1]
    return(y)
  }  )))
  
  # Plotar
  p <- ggplot2::ggplot(data=file_test_plot, ggplot2::aes(y=latitude, x=longitude)) +
    ggplot2::geom_raster(ggplot2::aes(fill=media)) + 
    ggplot2::scale_fill_continuous(na.value = 'white' ) + 
    ggplot2::facet_wrap(~Data)
  
  return(p)
}





