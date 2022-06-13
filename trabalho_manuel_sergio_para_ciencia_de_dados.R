# Nome: Jefferson Silva dos Anjos - 2016200130 - ANJOS, J. S.

# import das libs
install.packages("plot3D")

library(esquisse)
library(readxl)
library(ggplot2)
library(tidyverse)
library(psych)
library(DT)


#para outliers

library(fBasics)
library(sp)
library(rgl)
library("plot3D")

# import do dataframe
df_escola_norte <- read_excel("C:/Users/jeffe/Desktop/git/R/baseR/data/escolas_media_alunos_turma_2010.xls", col_types = c("text", "text", "text","numeric", "text", "numeric", "text","text", "text", "text", "text", "text","text", "text", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric","numeric", "numeric","numeric","numeric"), skip = 8, sheet = 1)
df_escola_nordeste_ext_ma_e_ba <- read_excel("C:/Users/jeffe/Desktop/git/R/baseR/data/escolas_media_alunos_turma_2010.xls", col_types = c("text", "text", "text","numeric", "text", "numeric", "text","text", "text", "text", "text", "text","text", "text", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric","numeric", "numeric","numeric","numeric"), skip = 8, sheet = 2)
df_escola_nordeste_somente_MA_e_BA <- read_excel("C:/Users/jeffe/Desktop/git/R/baseR/data/escolas_media_alunos_turma_2010.xls", col_types = c("text", "text", "text","numeric", "text", "numeric", "text","text", "text", "text", "text", "text","text", "text", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric","numeric", "numeric","numeric","numeric"), skip = 8, sheet = 3)
df_escola_sudeste <- read_excel("C:/Users/jeffe/Desktop/git/R/baseR/data/escolas_media_alunos_turma_2010.xls", col_types = c("text", "text", "text","numeric", "text", "numeric", "text","text", "text", "text", "text", "text","text", "text", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric","numeric", "numeric","numeric","numeric"), skip = 8, sheet = 4)
df_escola_sul <- read_excel("C:/Users/jeffe/Desktop/git/R/baseR/data/escolas_media_alunos_turma_2010.xls", col_types = c("text", "text", "text","numeric", "text", "numeric", "text","text", "text", "text", "text", "text","text", "text", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric","numeric", "numeric","numeric","numeric"), skip = 8, sheet = 5)
df_escola_centro_oeste <- read_excel("C:/Users/jeffe/Desktop/git/R/baseR/data/escolas_media_alunos_turma_2010.xls", col_types = c("text", "text", "text","numeric", "text", "numeric", "text","text", "text", "text", "text", "text","text", "text", "numeric", "numeric", "numeric","numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric","numeric", "numeric","numeric","numeric"), skip = 8, sheet = 6)


# pegando somente as colunas desejadas
df_n <- select(df_escola_norte, "1º Ano":"8ª série/ 9° ano")

#vizualizando a planilha
View(df_n)

# media das series de cada regiao primeiro ano
primeiro_ano <- df_n[["1º Ano"]]
primeiro_e_segundo <- df_n[["1ª série/ 2° ano"]]
segundo_e_terceiro <- df_n[["2ª série/ 3° ano"]]
terceiro_e_quarto <- df_n[["3ª série/ 4° ano"]]
quarto_e_quinto <- df_n[["4ª série/ 5° ano"]]
quinto_e_sexto <- df_n[["5ª série/ 6° ano"]]
sexto_e_setimo <- df_n[["6ª série/ 7° ano"]]
setimo_e_oitavo <- df_n[["7ª série/ 8° ano"]]
oitavo_e_nono <- df_n[["8ª série/ 9° ano"]]

media_primeiro_ano <- mean(primeiro_ano, na.rm=TRUE)
media_primeiro_e_segundo <- mean(primeiro_e_segundo, na.rm = TRUE)
media_segundo_e_terceiro <- mean(segundo_e_terceiro, na.rm = TRUE)
media_terceiro_e_quarto <- mean(terceiro_e_quarto, na.rm = TRUE)
media_quarto_e_quinto <- mean(quarto_e_quinto, na.rm = TRUE)
media_quinto_e_sexto <- mean(quinto_e_sexto, na.rm = TRUE)
media_sexto_e_setimo <- mean(sexto_e_setimo, na.rm = TRUE)
media_setimo_e_oitavo <- mean(setimo_e_oitavo, na.rm = TRUE)
media_oitavo_e_nono <- mean(oitavo_e_nono, na.rm = TRUE)


# grafico
turmas_label <- c("1º Ano",	"1ª série/ 2° ano",	"2ª série/ 3° ano", "3ª série/ 4° ano", "4ª série/ 5° ano",	"5ª série/ 6° ano",	"6ª série/ 7° ano",	"7ª série/ 8° ano",	"8ª série/ 9° ano"
)
media_valores <- c(media_primeiro_ano, media_primeiro_e_segundo, media_segundo_e_terceiro, media_terceiro_e_quarto, media_quarto_e_quinto, media_quinto_e_sexto, media_sexto_e_setimo, media_setimo_e_oitavo, media_oitavo_e_nono)

rotulo <- c("escolaridade", "media")

par(mgp=c(1,1,0))
## salvando a imagem
png(filename = "C:/Users/jeffe/Desktop/git/R/baseR/grafico/plot.png", width = 1000, height = 800)

## grafico sem linhas
##altura, titulo, rotulo x, rotulo y, label, altura_limit, espaçamento dos nomes, aceitar o grid
bar < - barplot(media_valores, main = "grafico região Norte", xlab = rotulo[1], ylab = rotulo[2], names.arg = turmas_label
        , ylim = c(0, 30), cex.names = 0.8, xaxs = "i")


grid(nx=NA, ny=NULL, lty = 2, col = "gray", lwd = 2)



barplot(media_valores, main = "grafico região Norte", xlab = rotulo[1], ylab = rotulo[2], names.arg = turmas_label
        , ylim = c(0, 30), cex.names = 0.8, xaxs = "i", add=TRUE)
## gera o arquivo
dev.off()

################## função para criação da media e grafico #########################
media_grafico <- function(df_nome, nome_imagem){
  df_n <- select(df_nome, "1º Ano":"8ª série/ 9° ano")
  
  #vizualizando a planilha
  View(df_n)
  
  # media das series de cada regiao primeiro ano
  primeiro_ano <- df_n[["1º Ano"]]
  primeiro_e_segundo <- df_n[["1ª série/ 2° ano"]]
  segundo_e_terceiro <- df_n[["2ª série/ 3° ano"]]
  terceiro_e_quarto <- df_n[["3ª série/ 4° ano"]]
  quarto_e_quinto <- df_n[["4ª série/ 5° ano"]]
  quinto_e_sexto <- df_n[["5ª série/ 6° ano"]]
  sexto_e_setimo <- df_n[["6ª série/ 7° ano"]]
  setimo_e_oitavo <- df_n[["7ª série/ 8° ano"]]
  oitavo_e_nono <- df_n[["8ª série/ 9° ano"]]
  
  media_primeiro_ano <- round(mean(primeiro_ano, na.rm=TRUE),2)
  media_primeiro_e_segundo <- round(mean(primeiro_e_segundo, na.rm = TRUE),2)
  media_segundo_e_terceiro <- round(mean(segundo_e_terceiro, na.rm = TRUE),2)
  media_terceiro_e_quarto <- round(mean(terceiro_e_quarto, na.rm = TRUE),2)
  media_quarto_e_quinto <- round(mean(quarto_e_quinto, na.rm = TRUE),2)
  media_quinto_e_sexto <- round(mean(quinto_e_sexto, na.rm = TRUE),2)
  media_sexto_e_setimo <- round(mean(sexto_e_setimo, na.rm = TRUE),2)
  media_setimo_e_oitavo <- round(mean(setimo_e_oitavo, na.rm = TRUE),2)
  media_oitavo_e_nono <- round(mean(oitavo_e_nono, na.rm = TRUE),2)
  
  
  # grafico
  turmas_label <- c("1º Ano",	"1ª série/ 2° ano",	"2ª série/ 3° ano", "3ª série/ 4° ano", "4ª série/ 5° ano",	"5ª série/ 6° ano",	"6ª série/ 7° ano",	"7ª série/ 8° ano",	"8ª série/ 9° ano"
  )
  media_valores <- c(media_primeiro_ano, media_primeiro_e_segundo, media_segundo_e_terceiro, media_terceiro_e_quarto, media_quarto_e_quinto, media_quinto_e_sexto, media_sexto_e_setimo, media_setimo_e_oitavo, media_oitavo_e_nono)
  
  rotulo <- c("escolaridade", "media")
  
  par(mgp=c(1,1,0))
  ## salvando a imagem
  nome <- nome_imagem
  
  caminho <- paste("C:/Users/jeffe/Desktop/git/R/baseR/grafico/",nome,"_plot.png")
  
  png(filename = caminho, width = 1000, height = 800)
  
  ## grafico sem linhas
  ##altura, titulo, rotulo x, rotulo y, label, altura_limit, espaçamento dos nomes, aceitar o grid
  bar <- barplot(media_valores, main = nome_imagem, xlab = rotulo[1], ylab = rotulo[2], names.arg = turmas_label
          , ylim = c(0, 30), cex.names = 0.8, xaxs = "i")
  
  grid(nx=NA, ny=NULL, lty = 2, col = "gray", lwd = 2)
  
  bar <- barplot(media_valores, main = "grafico região Norte", xlab = rotulo[1], ylab = rotulo[2], names.arg = turmas_label
          , ylim = c(0, 30), cex.names = 0.8, xaxs = "i", add=TRUE)
  
  
  text(x = bar, y = media_valores-1, labels = media_valores)
  
  ## gera o arquivo
  dev.off()
  
  ## tabela nova

  return(media_valores)
}

################ retornos dos dados <- media ###############

lista_de_valores_norte <- media_grafico(df_escola_norte, 'df_escola_norte')

lista_de_valores_sul <- media_grafico(df_escola_sul, 'df_escola_sul')

lista_de_valores_ext <- media_grafico(df_escola_nordeste_ext_ma_e_ba, 'df_escola_nordeste_ext_ma_e_ba')

lista_de_valores_ba_e_ma <- media_grafico(df_escola_nordeste_somente_MA_e_BA, 'df_escola_nordeste_somente_MA_e_BA')

lista_de_valores_sudeste <- media_grafico(df_escola_sudeste, 'df_escola_sudeste')

lista_de_valores_centro_oeste <- media_grafico(df_escola_centro_oeste,'df_escola_centro_oeste')

#################### novo dataframe só com medias de cada região ########################

turmas_label <- c("1º Ano",	"1ª série/ 2° ano",	"2ª série/ 3° ano", "3ª série/ 4° ano", "4ª série/ 5° ano",	"5ª série/ 6° ano",	"6ª série/ 7° ano",	"7ª série/ 8° ano",	"8ª série/ 9° ano"
)

df_medias <- data.frame(turmas=turmas_label,norte=lista_de_valores_norte, ext_ma_e_ba = lista_de_valores_ext, somente_ba_ma = lista_de_valores_ba_e_ma, centro_oeste=lista_de_valores_centro_oeste, sul=lista_de_valores_sul, sudeste=lista_de_valores_sudeste)

################### estatisticas basicas ######################

summary(df_medias[2:7])

describeBy(df_medias[2:7])


norte_summary <- summary(df_medias$norte)

ext_ba_ma_summary <-summary(df_medias$ext_ma_e_ba)

somente_ba_e_ma_summary <- summary(df_medias$somente_ba_ma)

sudeste_summary <-summary(df_medias$sudeste)

sul_summary <-summary(df_medias$sul)

centro_oeste_summary <-summary(df_medias$centro_oeste)


############### outPut ####################################

q1x <- quantile(df_medias$norte, c(0.25))


hist(df_medias$norte, labels = TRUE, ylim = c(0,35))
