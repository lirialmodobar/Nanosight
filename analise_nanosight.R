#Importar bibliotecas e scripts necessarios
library(tidyr)
library(dplyr)
library(readr)
library(broom)
#Definir pasta de trabalho e importar scripts necessários
setwd("C:/Users/Belle/Documents/NTA")
source("gera_pdf_qq_csv_norm.R")
source("analise_nanosight_utils.R")
#Trazer tabelas de interesse para o R
meuwd <- setwd ("C:/Users/Belle/Documents/NTA")
isolar_exp_sum <- list.files(pattern="*ExperimentSummary.csv") 
importar_exp_sum <- lapply(isolar_exp_sum, read.csv, header = FALSE, col.names = c(paste0("V", 1:6))) #importa todas as expsum corretamente ao nomear as colunas
#Extrair dados de interesse de ExpSum
##Para ter o dado do ID
isolar_exp_sum2 <- gsub (" W1", "_w1", isolar_exp_sum)
isolar_exp_sum2 <- gsub (" W2", "_w2", isolar_exp_sum2)
id_sample <- separate(as.data.frame(isolar_exp_sum2), 1, into=c("ids", "resto"), sep=" ") [,1]
id_sample <- extract_standardized_id(id_sample) #padroniza as ids para como o inpd usa
##Para ter o dado do quanto diluiu (assumindo que está no campo diluent, e não dilution factor, como a Jess fez)
diluicao <- as.data.frame(sapply(importar_exp_sum, index_lapply_sapply, 11, 2)) #se estiver no campo diluent (dados Jessica)
diluicao[diluicao == "" | is.na(diluicao)] <- deparse(quote(1/100)) #se nao estiver em campo nenhum e n tiver precisado diluir mais (dados padrao Belle)
diluicao[diluicao == "PBS"] <- "1/100"
diluicao[diluicao == "10X diluido" | diluicao == "10x diluida" | diluicao == "10x"] <- "1/1000"
##Para ter a concentração da amostra antes de diluir (concentração real)
concentracao_average <- as.numeric(sapply(importar_exp_sum, index_lapply_sapply, 44, 5)) #obter média da concentração
separar_fracao_diluicao <- separate(diluicao, 1, into=c("numerador_diluicao", "denominador_diluicao"), sep="/") #gera uma tabela com o numerador em uma coluna e denominador em outra para que seja possível ter um dado do tipo numeric a partir da fração de diluição anotada (usando só as.numeric, retorna NA, pq a barra confunde o R)
numerador <- as.numeric(separar_fracao_diluicao[,1]) #isolar o numerador da tabela gerada para poder transformar em numeric
denominador <- as.numeric(separar_fracao_diluicao[,2]) #isolar o denominador da tabela gerada para poder transformar em numeric
concentracao_real <- concentracao_average/(numerador/denominador) #obtém de fato a concentração real
##Para obter o restante dos dados
tamanho_mean_average <- as.numeric(sapply(importar_exp_sum, index_lapply_sapply, 73, 5)) #obter average da mean do tamanho
tamanho_mean_average_alt <- as.numeric(sapply(importar_exp_sum, index_lapply_sapply, 77, 5)) #obter average da mean do tamanho
na_indexes_tamanho_mean <- which(is.na(tamanho_mean_average))
tamanho_mean_average_alt <- tamanho_mean_average_alt[na_indexes_tamanho_mean]
tamanho_mean_average[is.na(tamanho_mean_average)] <- tamanho_mean_average_alt
tamanho_mode_average <- as.numeric(sapply(importar_exp_sum, index_lapply_sapply, 74,5)) #obter average da moda do tamanho
tamanho_mode_average_alt <- as.numeric(sapply(importar_exp_sum, index_lapply_sapply, 78,5)) #obter average da moda do tamanho
na_indexes_tamanho_mode <- which(is.na(tamanho_mode_average))
tamanho_mode_average_alt <- tamanho_mode_average_alt[na_indexes_tamanho_mode]
tamanho_mode_average[is.na(tamanho_mode_average)] <- tamanho_mode_average_alt
#Juntando todos os dados obtidos do nanosight em uma tabela
concentracao_real <- concentracao_real[-c(139:143,146)] #ficar de olho pra ver se essas pos nao mudam conforme add amostra e qqr coisa atualizar os numeros
tamanho_mean_average <- tamanho_mean_average[-c(139:143,146)] #ficar de olho pra ver se essas pos nao mudam conforme add amostra e qqr coisa atualizar os numeros
tamanho_mode_average <- tamanho_mode_average[-c(139:143,146)] #ficar de olho pra ver se essas pos nao mudam conforme add amostra e qqr coisa atualizar os numeros
tabela_tudo_nanosight <- data.frame(id_sample, concentracao_real, tamanho_mean_average, tamanho_mode_average)
tabela_tudo_nanosight<- na.omit(tabela_tudo_nanosight) #amostra 10521_w2 tinha uma leitura a mais sem valores
#Juntar dados do nanosight com informações da amostra
sample_information <- read.csv("sampleinformation_liriel320_atualizada.csv") #chamando/lendo/importando infos da amostra
sample_information$id_sample <- paste(sample_information$subjectid, sample_information$wave, sep = "_")
nanosight_plus_sampleinfo <- inner_join(tabela_tudo_nanosight, sample_information, by="id_sample")
anti_join(tabela_tudo_nanosight, nanosight_plus_sampleinfo, by = "id_sample") #6 amostras sem correspondência
write.csv(nanosight_plus_sampleinfo, "nanosight_plus_sampleinfo_all.csv", row.names = FALSE)
###Para selecionar as amostras dentro do zscore entre -1.5 e +1.5:
nanosight_plus_sampleinfo$zscore_mean <- scale(nanosight_plus_sampleinfo$tamanho_mean_average)
nanosight_plus_sampleinfo$zscore_mode <- scale(nanosight_plus_sampleinfo$tamanho_mode_average)
nanosight_plus_sampleinfo$zscore_concentracao <- scale(nanosight_plus_sampleinfo$concentracao_real)
nanosight_sem_outliers <- nanosight_plus_sampleinfo[nanosight_plus_sampleinfo$zscore_mean >= -1.5 & nanosight_plus_sampleinfo$zscore_mean <= 1.5 &
                                                  nanosight_plus_sampleinfo$zscore_mode >= -1.5 & nanosight_plus_sampleinfo$zscore_mode <= 1.5 &
                                                  nanosight_plus_sampleinfo$zscore_concentracao >= -1.5 & nanosight_plus_sampleinfo$zscore_concentracao <= 1.5, ]
outliers <- anti_join(nanosight_plus_sampleinfo, nanosight_sem_outliers, by = "id_sample") #26 outliers
###Testes de normalidade
shapiro_mean <- shapiro.test(nanosight_sem_outliers$tamanho_mean_average)
shapiro_mode <- shapiro.test(nanosight_sem_outliers$tamanho_mode_average)
shapiro_concentracao <- shapiro.test(nanosight_sem_outliers$concentracao_real)
shapiro_testes <- data.frame(mean = c(shapiro_mean), mode = c(shapiro_mode), concentracao = c(shapiro_concentracao))
shapiro_testes <- select(shapiro_testes, mean.p.value, mode.p.value, concentracao.p.value)
###QQPlot
dados_mean <- rnorm(nanosight_sem_outliers$tamanho_mean_average)
pdf("qqplot_mean.pdf")
qqnorm(dados_mean)
qqline(dados_mean, col = "red")
dev.off()
dados_mode <- rnorm(nanosight_sem_outliers$tamanho_mode_average)
pdf("qqplot_mode.pdf")
qqnorm(dados_mode)
qqline(dados_mode, col = "red")
dev.off()
#Contando número de amostras dos grupos e waves
library(stringr)
numero_amostras_grupos <- nanosight_sem_outliers %>%
  group_by(Trajetoria) %>%
  summarize(Amostras = n(),)
numero_amostras_grupos_w1 <- nanosight_sem_outliers %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w1")) %>%
  group_by(Trajetoria) %>%
  summarise(w1 = n())
numero_amostras_grupos_w2 <- nanosight_sem_outliers %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w2")) %>%
  group_by(Trajetoria) %>%
  summarise(w2 = n())
numero_amostras_grupos_waves <- numero_amostras_grupos %>%
  inner_join(numero_amostras_grupos_w1, by = "Trajetoria") %>%
  inner_join(numero_amostras_grupos_w2, by = "Trajetoria")
write.csv(numero_amostras_grupos_waves, "amostras_grupos_waves.csv", row.names = FALSE)
#Contando numero de amostras dos grupos e waves dos outliers
outliers_grupos <- outliers %>%
  group_by(Trajetoria) %>%
  summarize(Amostras = n(),)
outliers_grupos_w1 <- outliers %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w1")) %>%
  group_by(Trajetoria) %>%
  summarise(w1 = n())
outliers_grupos_w2 <- outliers %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w2")) %>%
  group_by(Trajetoria) %>%
  summarise(w2 = n())
outliers_grupos_waves <- outliers_grupos %>%
  inner_join(outliers_grupos_w1, by = "Trajetoria") %>%
  inner_join(outliers_grupos_w2, by = "Trajetoria")
write.csv(outliers_grupos_waves, "outliers_grupos_waves.csv", row.names = FALSE)

##########PAREI AQUI EM 05/09/23#################
#Separar w1 de w2
tabela_w1 <- nanosight_sem_outliers[nanosight_sem_outliers$wave == "w1",]
write.csv(tabela_w1, "nanosight_sem_outliers_w1.csv", row.names = FALSE)
tabela_w2 <- nanosight_sem_outliers[nanosight_sem_outliers$wave == "w2",]
write.csv(tabela_w2, "nanosight_sem_outliers_w2.csv", row.names = FALSE)
##Separar grupos
grupo_persistentes <- nanosight_sem_outliers[nanosight_sem_outliers$Trajetoria == "A",]
write.csv(grupo_persistentes, "nanosight_sem_outliers_persistentes.csv", row.names = FALSE)
grupo_incidentes <- nanosight_sem_outliers[nanosight_sem_outliers$Trajetoria == "B",]
write.csv(grupo_incidentes, "nanosight_sem_outliers_incidentes.csv", row.names = FALSE)
grupo_remitentes <- nanosight_sem_outliers[nanosight_sem_outliers$Trajetoria == "C",]
write.csv(grupo_remitentes, "nanosight_sem_outliers_remitentes.csv", row.names = FALSE)
grupo_controles <- nanosight_sem_outliers[nanosight_sem_outliers$Trajetoria == "D",]
write.csv(grupo_controles, "nanosight_sem_outliers_controles.csv", row.names = FALSE)
