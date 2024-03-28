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
##Acrescentando coluna com e sem Transtorno Psiquiátrico (TP_transversal)
nanosight_plus_sampleinfo <- mutate(nanosight_plus_sampleinfo, 
                                    TP_transversal = ifelse(Grupo %in% c("A_w1", "B_w1", "A_w2", "C_w2"), "caso",
                                                ifelse(Grupo %in% c("C_w1", "D_w1", "B_w2", "D_w2"), "controle", NA)))
##Acrescentando coluna de quem nunca teve TP e de quem já teve em algum momento da vida (TP_longitudinal)
nanosight_plus_sampleinfo <- mutate(nanosight_plus_sampleinfo,
                                    TP_longitudinal = ifelse(Trajetoria %in% c("A"), "caso",
                                                             ifelse(Trajetoria %in% c("B", "C", "D"), "controle", NA)))
##Retirando duas linhas da tabela (pq no boxplot de média de tamanho elas ficaram muito fora)
nanosight_plus_sampleinfo <- nanosight_plus_sampleinfo[-c(133,170), ]
##Tabela só com w1 e só w2
nanosight_w1 <- subset(nanosight_plus_sampleinfo, wave == "w1")
nanosight_w2 <- subset(nanosight_plus_sampleinfo, wave == "w2")

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

#Análise de Variância
##ANOVA para média e trajetória, considerando todos, só w1, e só w2
modelo_anova_media_trajetoria <- aov(tamanho_mean_average ~ Trajetoria, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_trajetoria <- summary(modelo_anova_media_trajetoria) #DEU (antes de tirar os outliers não tinha dado, p=0,0571)
modelo_anova_media_trajetoria_w1 <- aov(tamanho_mean_average ~ Trajetoria, data = nanosight_w1)
summary_modelo_anova_media_trajetoria_w1 <- summary(modelo_anova_media_trajetoria_w1) #DEU
modelo_anova_media_trajetoria_w2 <- aov(tamanho_mean_average ~ Trajetoria, data = nanosight_w2)
summary_modelo_anova_media_trajetoria_w2 <- summary(modelo_anova_media_trajetoria_w2) #NÃO DEU p=0,3
##ANOVA para média e presença ou não de TP, considerando todos, w1 e w2
modelo_anova_media_TP <- aov(tamanho_mean_average ~ TP_longitudinal, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_TP <- summary(modelo_anova_media_TP) #Análise longitudinal NÃO DEU
modelo_anova_media_w1_TP <- aov(tamanho_mean_average ~ TP_transversal, data = nanosight_w1) 
summary_modelo_anova_media_w1_TP <- summary(modelo_anova_media_w1_TP) #Análise transversal w1 (NÃO DEU, CONTINUA NÃO DANDO)
modelo_anova_media_w2_TP <- aov(tamanho_mean_average ~ TP_transversal, data = nanosight_w2) 
summary_modelo_anova_media_w2_TP <- summary(modelo_anova_media_w2_TP) #Análise transversal w2 (NÃO DEU, CONTINUA NÃO DANDO)
modelo_anova_media_grupos <- aov(tamanho_mean_average ~ Grupo, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_grupos <- summary(modelo_anova_media_grupos) #Trajetoria_wave (eu)* (P=0,09 NÃO DEU / AGORA DEU)
##ANOVA para porcentagem de EVs pequenas e trajetoria, considerando todos, w1 e w2
modelo_anova_porcentagem_trajetoria <- aov(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_plus_sampleinfo) 
summary_modelo_anova_porcentagem_trajetoria <- summary(modelo_anova_porcentagem_trajetoria) #DEU
modelo_anova_porcentagem_trajetoria_w1 <- aov(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_w1)
summary_modelo_anova_porcentagem_trajetoria_w1 <- summary(modelo_anova_porcentagem_trajetoria_w1) #DEU
modelo_anova_porcentagem_trajetoria_w2 <- aov(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_w2)
summary_modelo_anova_porcentagem_trajetoria_w2 <- summary(modelo_anova_porcentagem_trajetoria_w2) #DEU
##ANOVA para porcentagem de EVs pequenas e TP
modelo_anova_porcentagem_tp_longitudinal <- aov(EV_pequenas_porcentagem ~ TP_longitudinal, data = nanosight_plus_sampleinfo)
summary_modelo_anova_porcentagem_tp_longitudinal <- summary(modelo_anova_porcentagem_tp_longitudinal) #NAO DEU
modelo_anova_porcentagem_tp_transversal_w1 <- aov(EV_pequenas_porcentagem ~ TP_transversal, data = nanosight_w1)
summary_modelo_anova_porcentagem_tp_transversal_w1 <- summary(modelo_anova_porcentagem_tp_transversal_w1) #NAO DEU
modelo_anova_porcentagem_tp_transversal_w2 <- aov(EV_pequenas_porcentagem ~ TP_transversal, data = nanosight_plus_sampleinfo)
summary_modelo_anova_porcentagem_tp_transversal_w2 <- summary(modelo_anova_porcentagem_tp_transversal_w2) #NAO DEU
##ANOVA para media e sexo
modelo_anova_media_sexo <- aov(tamanho_mean_average ~ sex, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_sexo <- summary(modelo_anova_media_sexo) #NAO DEU
##ANOVA para media e estado
modelo_anova_media_site <- aov(tamanho_mean_average ~ site, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_site <- summary(modelo_anova_media_site) #NÃO DEU
##ANOVA para media e TPs
###dcany
modelo_anova_media_dcany <- aov(tamanho_mean_average ~ dcany, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcany <- summary(modelo_anova_media_dcany) #NÃO DEU
###dcmadep
modelo_anova_media_dcmadep <- aov(tamanho_mean_average ~ dcmadep, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcmadep <- summary(modelo_anova_media_dcmadep) #NÃO DEU
###dcanyanx
modelo_anova_media_dcanyanx <- aov(tamanho_mean_average ~ dcanyanx, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcanyanx <- summary(modelo_anova_media_dcanyanx) #NÃO DEU
###dcgena
modelo_anova_media_dcgena <- aov(tamanho_mean_average ~ dcgena, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcgena <- summary(modelo_anova_media_dcgena) #NÃO DEU
###dcanyhk
modelo_anova_media_dcanyhk <- aov(tamanho_mean_average ~ dcanyhk, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcanyhk <- summary(modelo_anova_media_dcanyhk) #NÃO DEU
###dcpsych
modelo_anova_media_dcpsych <- aov(tamanho_mean_average ~ dcpsych, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcpsych <- summary(modelo_anova_media_dcpsych) #NÃO DEU
###dcmania
modelo_anova_media_dcmania <- aov(tamanho_mean_average ~ dcmania, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcmania <- summary(modelo_anova_media_dcmania) #NÃO DEU
###dcptsd
modelo_anova_media_dcptsd <- aov(tamanho_mean_average ~ dcptsd, data = nanosight_plus_sampleinfo)
summary_modelo_anova_media_dcptsd <- summary(modelo_anova_media_dcptsd) #DEU (p=0,04) -> NÃO DEU PÕS AJUSTE DE WHITE
###concentracao_real
modelo_anova_concentracao_trajetoria <- aov(concentracao_real ~ Trajetoria, data = nanosight_plus_sampleinfo)
summary_modelo_anova_concentracao_trajetoria <- summary(modelo_anova_concentracao_trajetoria)
##Testando as premissas da ANOVA
###Homogeneidade das amostras
####Teste de Levene
library("car")
leveneTest(tamanho_mean_average ~ Trajetoria, data = nanosight_plus_sampleinfo) #não homogêneo p=0,01 (antes da retirada, era homogêneo p>0,05)
leveneTest(tamanho_mean_average ~ Trajetoria, data = nanosight_w1) #não homogêneo, p=0,002
leveneTest(tamanho_mean_average ~ Grupo, data = nanosight_plus_sampleinfo) #não homogêneo, p<0,05
leveneTest(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_plus_sampleinfo) #não homogêneo, p < 0,05 
leveneTest(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_w1) #não homogêneo, p<0,05
leveneTest(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_w2) #não homogêneo, p<0,05
leveneTest(EV_pequenas_porcentagem ~ TP_longitudinal, data = nanosight_plus_sampleinfo) #homogêneo p > 0,05
leveneTest(concentracao_real ~ Trajetoria, data = nanosight_plus_sampleinfo) #não homogeneo, p<0,05
###Normalidade dos resíduos
####Teste de Shapiro
shapiro.test(resid(modelo_anova_media_trajetoria)) # < 0,05 difere da normalidade
shapiro.test(resid(modelo_anova_media_trajetoria_w1)) # dentro da normalidade, p=0,5645
shapiro.test(resid(modelo_anova_media_grupos)) # > 0,05, dentro da normalidade
shapiro.test(resid(modelo_anova_porcentagem_trajetoria)) # difere da normalidade, p < 0,05
shapiro.test(resid(modelo_anova_porcentagem_trajetoria_w1)) #difere da normalidade p < 0,05
shapiro.test(resid(modelo_anova_porcentagem_trajetoria_w2)) #difere da normalidade p < 0,05
shapiro.test(resid(modelo_anova_porcentagem_tp_longitudinal)) # difere da normalidade p <<<< 0,5
shapiro.test(resid(modelo_anova_concentracao_trajetoria)) #difere da normalidade p <<<<<<<<<<< 0,5
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
tukey_test_media_trajetoria <- TukeyHSD(modelo_anova_media_trajetoria) #significativo para B-A e D-B
tukey_test_media_trajetoria_w1 <- TukeyHSD(modelo_anova_media_trajetoria_w1) #significativo para B-A
tukey_test_media_grupos <- TukeyHSD(modelo_anova_media_grupos) #significativo para A_w1 e B_w1
tukey_test_porcentagem_trajetoria <- TukeyHSD(modelo_anova_porcentagem_trajetoria) #significativo para B-A, C-B, D-B (B em relação a todos)
tukey_test_porcentagem_trajetoria_w1 <- TukeyHSD(modelo_anova_porcentagem_trajetoria_w1) #significativo para B-A e D-B
tukey_test_porcentagem_trajetoria_w2 <- TukeyHSD(modelo_anova_porcentagem_trajetoria_w2) #significativo para D-B
tukey_test_porcentagem_tp_longitudinal <- TukeyHSD(modelo_anova_porcentagem_tp_longitudinal) #significativo para controle-caso
tukey_test_concentracao_trajetoria <- TukeyHSD(modelo_anova_concentracao_trajetoria) #significativo para B-A, C-B, D-B (B em relação a todos)
##Teste LSD = mesmos resultados do tukey em todos
library(lsmeans)
lsd_mean_media_trajetoria <- lsmeans(modelo_anova_media_trajetoria, specs = "Trajetoria")
lsd_mean_result_media_trajetoria <- summary(pairs(lsd_mean_media_trajetoria)) #A-B e B-D
lsd_mean_media_trajetoria_w1 <- lsmeans(modelo_anova_media_trajetoria_w1, specs = "Trajetoria")
lsd_mean_result_media_trajetoria_w1 <- summary(pairs(lsd_mean_media_trajetoria_w1)) #A-B
lsd_mean_media_grupos <- lsmeans(modelo_anova_media_grupos, specs = "Grupo")
lsd_mean_result_media_grupos <- summary(pairs(lsd_mean_media_grupos)) #A_w1 e B_w1
lsd_mean_porcentagem_trajetoria <- lsmeans(modelo_anova_porcentagem_trajetoria, specs = "Trajetoria")
lsd_mean_result_porcentagem_trajetoria <- summary(pairs(lsd_mean_porcentagem_trajetoria)) #A-B, B-C, B-D
lsd_mean_porcentagem_trajetoria_w1 <- lsmeans(modelo_anova_porcentagem_trajetoria_w1, specs = "Trajetoria")
lsd_mean_result_porcentagem_trajetoria_w1 <- summary(pairs(lsd_mean_porcentagem_trajetoria_w1)) #A-B, B-D
lsd_mean_porcentagem_trajetoria_w2 <- lsmeans(modelo_anova_porcentagem_trajetoria_w2, specs = "Trajetoria")
lsd_mean_result_porcentagem_trajetoria_w2 <- summary(pairs(lsd_mean_porcentagem_trajetoria_w2)) #B-D
lsd_mean_porcentagem_tp_longitudinal <- lsmeans(modelo_anova_porcentagem_tp_longitudinal, specs = "TP_longitudinal")
lsd_mean_result_porcentagem_tp_longitudinal <- summary(pairs(lsd_mean_porcentagem_tp_longitudinal)) #caso-controle
##Tamanho do efeito
library("lsr")
etaSquared(modelo_anova_media_trajetoria)
plot(TukeyHSD(modelo_anova_media_trajetoria), las = 1)
etaSquared(modelo_anova_concentracao_trajetoria)
plot(TukeyHSD(modelo_anova_concentracao_trajetoria), las = 1)
##Correção para valores heterocedásticos (ajuste de White)
library("car")
modelo <- lm(tamanho_mean_average ~ Trajetoria, data = nanosight_plus_sampleinfo)
Anova(modelo, Type="II", white.adjust=TRUE) #DEU
modelo2 <- lm(tamanho_mean_average ~ Trajetoria, data = nanosight_w2)
Anova(modelo2, Type="II", white.adjust=TRUE) #NÃO DEU
modelo3 <- lm(tamanho_mean_average ~ TP_longitudinal, data = nanosight_plus_sampleinfo)
Anova(modelo3, Type="II", white.adjust=TRUE) #NÃO DEU
modelo4 <- lm(tamanho_mean_average ~ TP_transversal, data = nanosight_w1)
Anova(modelo4, Type="II", white.adjust=TRUE) #NÃO DEU
modelo5 <- lm(tamanho_mean_average ~ Grupo, data = nanosight_plus_sampleinfo)
Anova(modelo5, Type="II", white.adjust=TRUE) #DEU
modelo6 <- lm(EV_pequenas_porcentagem ~ TP_longitudinal, data = nanosight_plus_sampleinfo)
Anova(modelo6, Type="II", white.adjust=TRUE) #NAO DEU
modelo7 <- lm(tamanho_mean_average ~ sex, data = nanosight_plus_sampleinfo)
Anova(modelo7, Type="II", white.adjust=TRUE) #NAO DEU
modelo8 <- lm(tamanho_mean_average ~ dcany, data = nanosight_plus_sampleinfo)
Anova(modelo8, Type="II", white.adjust=TRUE) #NÃO DEU
modelo9 <- lm(tamanho_mean_average ~ dcanyanx, data = nanosight_plus_sampleinfo)
Anova(modelo9, Type="II", white.adjust=TRUE)
modelo10 <- lm(tamanho_mean_average ~ dcptsd, data = nanosight_plus_sampleinfo)
Anova(modelo10, Type="II", white.adjust=TRUE)

#Analises pareadas
##Separando os dados
nanosight_pares <- nanosight_plus_sampleinfo[duplicated(nanosight_plus_sampleinfo$subjectid) | duplicated(nanosight_plus_sampleinfo$subjectid, fromLast = TRUE), ]
nanosight_pares_A <- subset(nanosight_pares, Trajetoria == "A")
nanosight_pares_B <- subset(nanosight_pares, Trajetoria == "B")
nanosight_pares_C <- subset(nanosight_pares, Trajetoria == "C")
nanosight_pares_D <- subset(nanosight_pares, Trajetoria == "D")
nanosight_pares_A_w1 <- subset(nanosight_pares_A, wave == "w1")
nanosight_pares_A_w2 <- subset(nanosight_pares_A, wave == "w2")
nanosight_pares_B_w1 <- subset(nanosight_pares_B, wave == "w1")
nanosight_pares_B_w2 <- subset(nanosight_pares_B, wave == "w2")
nanosight_pares_C_w1 <- subset(nanosight_pares_C, wave == "w1")
nanosight_pares_C_w2 <- subset(nanosight_pares_C, wave == "w2")
nanosight_pares_D_w1 <- subset(nanosight_pares_D, wave == "w1")
nanosight_pares_D_w2 <- subset(nanosight_pares_D, wave == "w2")
##Teste T pareado
t.test(nanosight_pares_A_w1$tamanho_mean_average, nanosight_pares_A_w2$tamanho_mean_average, paired = TRUE) #sem diferença (p=0,05734)
t.test(nanosight_pares_A_w1$EV_pequenas_porcentagem, nanosight_pares_A_w2$EV_pequenas_porcentagem, paired = TRUE) #sem diferença (p=0,1)
t.test(nanosight_pares_B_w1$tamanho_mean_average, nanosight_pares_B_w2$tamanho_mean_average, paired = TRUE) #sem diferença (p=0,2)
t.test(nanosight_pares_B_w1$EV_pequenas_porcentagem, nanosight_pares_B_w2$EV_pequenas_porcentagem, paired = TRUE) #sem diferença (p=0,3)
t.test(nanosight_pares_C_w1$tamanho_mean_average, nanosight_pares_C_w2$tamanho_mean_average, paired = TRUE) #sem diferença (p=0,2)
t.test(nanosight_pares_C_w1$EV_pequenas_porcentagem, nanosight_pares_C_w2$EV_pequenas_porcentagem, paired = TRUE) #sem diferença (p=0,7)
t.test(nanosight_pares_D_w1$tamanho_mean_average, nanosight_pares_D_w2$tamanho_mean_average, paired = TRUE) #sem diferença (p=0,8)
t.test(nanosight_pares_D_w1$EV_pequenas_porcentagem, nanosight_pares_D_w2$EV_pequenas_porcentagem, paired = TRUE) #sem diferença (p=0,8)

######################################

#Testes não paramétricos para avaliar w1-w2 e TP/sem TP
##Teste de normalidade (pressuposto para o teste t de student)
library("nortest")
lillie.test(nanosight_plus_sampleinfo$tamanho_mean_average) #Não deu normal, p < 0,05
##Teste de Shapiro para normalidade
shapiro.test(nanosight_plus_sampleinfo$tamanho_mean_average[nanosight_plus_sampleinfo$TP_transversal == "controle"]) #limítrofe p=0,05113
shapiro.test(nanosight_plus_sampleinfo$tamanho_mean_average[nanosight_plus_sampleinfo$TP_transversal == "caso"]) #fora da normalidade p=0,003866
##Homogeneidade de variância
library(car)
leveneTest(tamanho_mean_average ~ TP_transversal, data = nanosight_plus_sampleinfo, center = median) #DEU
##Teste T
t.test(nanosight_plus_sampleinfo$tamanho_mean_average ~ nanosight_plus_sampleinfo$TP_transversal, paired = F, conf.level = 0.95, var.eq = T) #sem diferença
##Tamanho do efeito
library("effsize")
cohen.d(tamanho_mean_average ~ TP_transversal, data = nanosight_plus_sampleinfo, paired = F) #sem diferença

##################

