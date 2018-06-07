library(qcc)
library(tidyverse)

dados_completo <- data.cleaner( read.csv("dados/GPS_FINAL_DATA.TXT") )

vel <- 
  dados_completo %>% 
  select(Velocidade, Data) %>%
  group_by(Data) %>% 
  mutate(id=row_number()) %>% 
  spread(Data, Velocidade) %>% 
  select(-id) 

vel
tail(vel)
dim(vel)

q_cep_trad <- qcc(vel[1:927,], type="xbar", newdata = vel[928:1855,] )
plot(q_cep_trad, chart.all=FALSE)


