
library(ggplot2)
library(esquisse)
library(readxl)

df_escola <- read_excel("data/escolas_media_alunos_turma_2010.xls", 
                                              sheet = "NORTE", skip = 8)

df_escola_norte = df_escola

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

print(media_primeiro_ano_n)
print(media_primeiro_e_segundo_n)


# grafico
turmas_label <- c("1º Ano",	"1ª série/ 2° ano",	"2ª série/ 3° ano", "3ª série/ 4° ano", "4ª série/ 5° ano",	"5ª série/ 6° ano",	"6ª série/ 7° ano",	"7ª série/ 8° ano",	"8ª série/ 9° ano"
)
media_valores <- c(media_primeiro_ano, media_primeiro_e_segundo, media_segundo_e_terceiro, media_terceiro_e_quarto, media_quarto_e_quinto, media_quinto_e_sexto, media_sexto_e_setimo, media_setimo_e_oitavo, media_oitavo_e_nono)

rotulo <- c("escolaridade", "media")

par(mgp=c(1,2,0))
## salvando a imagem
png(filename = "caminho onde vai salvar a imagem", width = 1000, height = 800)

## grafico sem linhas
##altura, titulo, rotulo x, rotulo y, label, altura_limit, espaçamento dos nomes, aceitar o grid
barplot(media_valores, main = "grafico região Norte", xlab = rotulo[1], ylab = rotulo[2], names.arg = turmas_label
        , ylim = c(0, 30), cex.names = 0.8, xaxs = "i")

## gera o arquivo
dev.off()

