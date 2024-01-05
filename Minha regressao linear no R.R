##Carregar pacotes

library(readr)
library(dplyr)
library(ggplot2)
library(tsibble)
library(lubridate)

#mandar rodar no console para ver se esta tudo correto

####Dados

##agora importa o arquivo com os dados
 dados <- read.csv(file = "https://aluno.analisemacro.com.br/download/47784/?tmstv=1678715174")
 
 ### vamos para a parte da Regressão Linear
 
 modelo <- lm( formula = y ~ tempo, data = dados)
 
 #####extrai o ajuste do modelo, quer dizer resolve a equação
 
 ajuste <- fitted(modelo)
 
 
 ###Cria-se uma nova coluna com os valores ajustados e transforma a coluna para YYYY-MM-DD
 
 dados<- mutate(
   dados,
   tendencia = ajuste,
   data      = as_date(yearquarter(data))
 )

 
 ###apos ajustar a data fazemos o grafico de linha com os valores observados e a reta de ajuste
 
 ggplot(data = dados) +
   aes(x = data) +
   geom_line(mapping = aes(y = y, color = "PIB"), size = 1) +
   geom_line(mapping = aes(y = tendencia, color = "Tendência linear"), size = 2) +
   scale_color_manual(values = c("#282f6b", "#b22200")) +
   theme(legend.position = "bottom") +
   labs(
     title    = "PIB do Brasil",
     subtitle = "Preços de mercado, nº índice sazonalmente ajustado (média de 1995 = 100)",
     y        = "Índice",
     x        = NULL,
     color    = NULL,
     caption  = "Dados: IBGE | Elaboração: Juliano Dias"
   )
 
 