#Importar bibliotecas e scripts necessarios
library(tidyr)
library(dplyr)
library(readr)
library(broom)

#Definir pasta de trabalho e importar scripts necessários
setwd("C:/Users/Belle/Documents/Belle")
source("gera_pdf_qq_csv_norm.R")
source("analise_nanosight_utils.R")

#Trazer tabelas de interesse para o R
meuwd <- setwd ("C:/Users/Belle/Documents/Belle")
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
##Porcentagem de vesículas pequenas e grandes (corte 150.5 nm)
concentration_EV_pequenas <- as.numeric(sapply(importar_exp_sum, function(x) {sum(as.numeric(x[82:232, 5]))}))
concentration_EV_pequenas_alt <- as.numeric(sapply(importar_exp_sum, function(x) {sum(as.numeric(x[87:237, 5]))}))
na_indexes_concentration_EV_pequenas <- which(is.na(concentration_EV_pequenas))
concentration_EV_pequenas_alt <- concentration_EV_pequenas_alt[na_indexes_concentration_EV_pequenas]
concentration_EV_pequenas[is.na(concentration_EV_pequenas)] <- concentration_EV_pequenas_alt
concentration_EV_grandes <- as.numeric(sapply(importar_exp_sum, function(x) {sum(as.numeric(x[233:1081, 5]))}))
concentration_EV_grandes_alt <- as.numeric(sapply(importar_exp_sum, function(x) {sum(as.numeric(x[238:1086, 5]))}))
na_indexes_concentration_EV_grandes <- which(is.na(concentration_EV_grandes))
concentration_EV_grandes_alt <- concentration_EV_grandes_alt[na_indexes_concentration_EV_grandes]
concentration_EV_grandes[is.na(concentration_EV_grandes)] <- concentration_EV_grandes_alt
EV_pequenas_porcentagem <- (concentration_EV_pequenas/(concentration_EV_grandes + concentration_EV_pequenas)) * 100
##Juntando todos os dados obtidos do nanosight em uma tabela
tabela_tudo_nanosight <- data.frame(id_sample, concentracao_real, tamanho_mean_average, tamanho_mode_average, EV_pequenas_porcentagem)
duplicatas <- duplicated(tabela_tudo_nanosight[, c("id_sample")], fromLast = TRUE) #a 2a leitura é a correta
nanosight_sem_duplicatas <- subset(tabela_tudo_nanosight, !duplicatas)
##Juntar dados do nanosight com informações da amostra
sample_information <- read.csv("sampleinformation_liriel320_atualizada.csv") #chamando/lendo/importando infos da amostra
sample_information$id_sample <- paste(sample_information$subjectid, sample_information$wave, sep = "_")
nanosight_plus_sampleinfo <- inner_join(nanosight_sem_duplicatas, sample_information, by="id_sample")
nanosight_plus_sampleinfo$Grupo <- paste(nanosight_plus_sampleinfo$Trajetoria, nanosight_plus_sampleinfo$wave, sep = "_")
sem_correspondencia <- anti_join(nanosight_sem_duplicatas, sample_information, by = "id_sample")
write.csv(nanosight_plus_sampleinfo, "nanosight_plus_sampleinfo_all.csv", row.names = FALSE)
##Amostras sem análise no Nanosight
sample_info_sem_nanosight <- anti_join(sample_information, nanosight_sem_duplicatas, by="id_sample")
##Acrescentando coluna com e sem Transtorno Psiquiátrico (TP)
nanosight_plus_sampleinfo <- mutate(nanosight_plus_sampleinfo, 
                                    TP = ifelse(Grupo %in% c("A_w1", "B_w1", "A_w2", "C_w2"), "sem TP",
                                                ifelse(Grupo %in% c("C_w1", "D_w1", "B_w2", "D_w2"), "com TP", NA)))

#Contagem dos dados
##Número de amostras dos grupos e waves
library(stringr)
numero_amostras_grupos <- nanosight_plus_sampleinfo %>%
  group_by(Trajetoria) %>%
  summarize(Amostras = n(),)
numero_amostras_grupos_w1 <- nanosight_plus_sampleinfo %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w1")) %>%
  group_by(Trajetoria) %>%
  summarise(w1 = n())
numero_amostras_grupos_w2 <- nanosight_plus_sampleinfo %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w2")) %>%
  group_by(Trajetoria) %>%
  summarise(w2 = n())
numero_amostras_grupos_waves <- numero_amostras_grupos %>%
  inner_join(numero_amostras_grupos_w1, by = "Trajetoria") %>%
  inner_join(numero_amostras_grupos_w2, by = "Trajetoria")
write.csv(numero_amostras_grupos_waves, "amostras_grupos_waves.csv", row.names = FALSE)
##Amostras sem pares
linhas_sem_pares <- !duplicated(nanosight_plus_sampleinfo$subjectid) & !duplicated(nanosight_plus_sampleinfo$subjectid, fromLast = TRUE)
nanosight_sem_pares <- nanosight_plus_sampleinfo[linhas_sem_pares, ]

#Distribuição dos dados
##Boxplot da Média dos Tamanhos
library(ggplot2)
boxplot_tamanho_mean <- ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_boxplot(fill = "#DDA0DD", color = "#800080", alpha = 0.7) +
  labs(title = "Mean Size Boxplot by Group", x = "Group", y = "Mean size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mean.pdf", plot = boxplot_tamanho_mean + theme(text = element_text(size=20)), width = 7, height = 7)
boxplot_tamanho_mean_w1 <- ggplot(subset(nanosight_plus_sampleinfo, wave == "w1"), aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_boxplot(fill = "#DDA0DD", color = "#800080", alpha = 0.7) +
  labs(title = "Mean Size Boxplot for Category w1", x = "Group", y = "Mean size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mean_w1.pdf", plot = boxplot_tamanho_mean_w1 + theme(text = element_text(size=20)), width = 7, height = 7)
boxplot_tamanho_mean_w2 <- ggplot(subset(nanosight_plus_sampleinfo, wave == "w2"), aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_boxplot(fill = "#DDA0DD", color = "#800080", alpha = 0.7) +
  labs(title = "Mean Size Boxplot for Category w2", x = "Group", y = "Mean size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mean_w2.pdf", plot = boxplot_tamanho_mean_w2 + theme(text = element_text(size=20)), width = 7, height = 7)
##Gráfico de densidade:
plot(density(nanosight_plus_sampleinfo$tamanho_mean_average))

#Análise de Variância
##Teste ANOVA
modelo_anova <- aov(tamanho_mean_average ~ Trajetoria, data = nanosight_plus_sampleinfo) #inicial, p=0,0571 (não deu)
summary_modelo_anova <- summary(modelo_anova)
modelo_anova10 <- summary(aov(tamanho_mean_average ~ wave, data = nanosight_plus_sampleinfo)) #ver se nas waves é comparável (não deu MESMO)
modelo_anova5 <- summary(aov(tamanho_mean_average ~ TP, data = nanosight_wave_w1)) #Análise transversal w1 (NÃO DEU)
modelo_anova6 <- summary(aov(tamanho_mean_average ~ TP, data = nanosight_wave_w2)) #Análise transversal w2 (NÃO DEU)
modelo_anova7 <- summary(aov(tamanho_mean_average ~ TP, data = nanosight_plus_sampleinfo)) #Análise transversal todas (NÃO DEU)
modelo_anova2 <- summary(aov(tamanho_mean_average ~ Grupo, data = nanosight_plus_sampleinfo)) #Trajetoria_wave (eu)* (P=0,09 NÃO DEU)
modelo_anova3 <- aov(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_plus_sampleinfo) #p=5,79e-0,5 (DEU)
summary_modelo_anova3 <- summary(modelo_anova3)
modelo_anova4 <- summary(aov(EV_pequenas_porcentagem ~ wave, data = nanosight_plus_sampleinfo)) #NÃO DEU
modelo_anova11 <- aov(tamanho_mean_average ~ TP, data = nanosight_plus_sampleinfo) #p=0,05714 (não deu)
summary_modelo_anova11 <- summary(modelo_anova11)
modelo_anova12 <- aov(EV_pequenas_porcentagem ~ TP, data = nanosight_plus_sampleinfo) #não deu
summary_modelo_anova12 <- summary(modelo_anova12)
##Testando as premissas da ANOVA
###Homogeneidade das amostras
####Teste de Levene
library("car")
leveneTest(tamanho_mean_average ~ Trajetoria, data = nanosight_plus_sampleinfo) # >0.05, homogêneo
leveneTest(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_plus_sampleinfo) # < 0,05, não homogêneo
leveneTest(tamanho_mean_average ~ TP, data = nanosight_plus_sampleinfo) # quase igual a 0,05, não deu
###Normalidade dos resíduos
####Teste de Shapiro
shapiro.test(resid(modelo_anova)) # < 0,05 difere da normalidade
shapiro.test(resid(modelo_anova3)) # < 0,05 difere da normalidade
shapiro.test(resid(modelo_anova11)) # < 0,05 difere da normalidade
####QQPlot para visualizar a distribuição das amostras em relação a uma distribuição normal
dados_mean <- rnorm(nanosight_plus_sampleinfo$tamanho_mean_average)
pdf("qqplot_mean.pdf")
qqnorm(dados_mean)
qqline(dados_mean, col = "red")
dev.off()
dados_EV_pequenas_porcentagem <- rnorm(nanosight_plus_sampleinfo$EV_pequenas_porcentagem)
pdf("qqplot_EV_pequenas_porcentagem.pdf")
qqnorm(dados_EV_pequenas_porcentagem)
qqline(dados_EV_pequenas_porcentagem, col = "red")
dev.off()
#Comparação das médias par-a-par
##Teste de Tukey
tukey_test3 <- TukeyHSD(modelo_anova3) #significativo para B-A e D-B
tukey_test <- TukeyHSD(modelo_anova) #chega perto D-B, B-A distoa dos outros tbm apesar de não ser significante
##Teste LSD
library(lsmeans)
lsd_mean <- lsmeans(modelo_anova, specs = "Trajetoria")
lsd_mean_result <- pairs(lsd_mean)
