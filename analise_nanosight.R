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
id_sample <- separate(as.data.frame(isolar_exp_sum), 1, into=c("ids", "resto"), sep=" ") [,1]
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
concentracao_real <- concentracao_real[-c(91:95,98)] #ficar de olho pra ver se essas pos nao mudam conforme add amostra e qqr coisa atualizar os numeros
tamanho_mean_average <- tamanho_mean_average[-c(91:95,98)] #ficar de olho pra ver se essas pos nao mudam conforme add amostra e qqr coisa atualizar os numeros
tamanho_mode_average <- tamanho_mode_average[-c(91:95,98)] #ficar de olho pra ver se essas pos nao mudam conforme add amostra e qqr coisa atualizar os numeros
tabela_tudo_nanosight <- data.frame(id_sample, concentracao_real, tamanho_mean_average, tamanho_mode_average)
tabela_tudo_nanosight<- na.omit(tabela_tudo_nanosight) #pq tinha essa linha mesmo??? rodar ate antes dela pra ver
#Juntar dados do nanosight com informações da amostra
sample_information <- read.csv("sampleinformation_liriel320_atualizada.csv") #chamando/lendo/importando infos da amostra
sample_information$id_sample <- paste(sample_information$subjectid, sample_information$wave, sep = "_")
nanosight_plus_sampleinfo <- inner_join(tabela_tudo_nanosight, sample_information, by="id_sample")
write.csv(nanosight_plus_sampleinfo, "nanosight_plus_sampleinfo_all.csv", row.names = FALSE)
#Separar w1 de w2
tabela_w1 <- nanosight_plus_sampleinfo[nanosight_plus_sampleinfo$wave == "w1",]
write.csv(tabela_w1, "nanosight_plus_sampleinfo_w1.csv", row.names = FALSE)
tabela_w2 <- nanosight_plus_sampleinfo[nanosight_plus_sampleinfo$wave == "w2",]
write.csv(tabela_w2, "nanosight_plus_sampleinfo_w2.csv", row.names = FALSE)
##Separar grupos
grupo_persistentes <- nanosight_plus_sampleinfo[nanosight_plus_sampleinfo$Trajetoria == "A",]
write.csv(grupo_persistentes, "nanosight_plus_sampleinfo_grupo_persistentes.csv", row.names = FALSE)
grupo_incidentes <- nanosight_plus_sampleinfo[nanosight_plus_sampleinfo$Trajetoria == "B",]
write.csv(grupo_incidentes, "nanosight_plus_sampleinfo_grupo_incidentes.csv", row.names = FALSE)
grupo_remitentes <- nanosight_plus_sampleinfo[nanosight_plus_sampleinfo$Trajetoria == "C",]
write.csv(grupo_remitentes, "nanosight_plus_sampleinfo_grupo_remitentes.csv", row.names = FALSE)
grupo_controles <- nanosight_plus_sampleinfo[nanosight_plus_sampleinfo$Trajetoria == "D",]
write.csv(grupo_controles, "nanosight_plus_sampleinfo_grupo_controles.csv", row.names = FALSE)
#Teste de normalidade para tds as waves, w1 e w2
#teste_norm_em_csv()
##Para dados da Jess:
###all: conc normal (barely), p = 0.0511; tamanho_mean_av normal, p = 0.461; tamanho_mode_av normal, p = 0.194; age normal, p = 0.955
###w1 conc normal, p=0.964; tamanho_mean_av normal, p =0.662; tamanho_mode_av normal, p=0.951, age normal p=0.275
###w2 conc nao deu normal, p=0.00559!!!; tamanho_mean_av normal, p=0.294; tamanho_mode_av normal, p = 0.482, age normal p = 0.152
#QQ para tds as waves, w1 e w2
#gera_qq_em_pdf()
###Para selecionar as amostras dentro do zscore entre -2 e +2:
tabela_tudo_nanosight$zscore_mean <- scale(tabela_tudo_nanosight$tamanho_mean_average)
tabela_tudo_nanosight$zscore_mode <- scale(tabela_tudo_nanosight$tamanho_mode_average)
tabela_tudo_nanosight$zscore_concentracao <- scale(tabela_tudo_nanosight$concentracao_real)
nanosight_sem_outliers <- tabela_tudo_nanosight[tabela_tudo_nanosight$zscore_mean >= -2 & tabela_tudo_nanosight$zscore_mean <= 2 &
                                                tabela_tudo_nanosight$zscore_mode >= -2 & tabela_tudo_nanosight$zscore_mode <= 2 &
                                                tabela_tudo_nanosight$zscore_concentracao >= -2 & tabela_tudo_nanosight$zscore_concentracao <= 2, ]
###Testes de normalidade
shapiro_mean <- shapiro.test(nanosight_sem_outliers$tamanho_mean_average)
shapiro_mode <- shapiro.test(nanosight_sem_outliers$tamanho_mode_average)
shapiro_concentracao <- shapiro.test(nanosight_sem_outliers$concentracao_real)
shapiro_testes <- data.frame(mean = c(shapiro_mean), mode = c(shapiro_mode), concentracao = c(shapiro_concentracao))
shapiro_testes <- select(shapiro_testes, mean.p.value, mode.p.value, concentracao.p.value)
###Gerar QQ Plots em pdf
pdf("qq_mean.pdf")
qqnorm(mean_sem_outliers)
qqline(mean_sem_outliers, col = 2)
dev.off()
mode_sem_outliers <- nanosight_sem_outliers$tamanho_mode_average
pdf("qq_mode.pdf")
qqnorm(mode_sem_outliers)
qqline(mode_sem_outliers, col = 2)
dev.off()
