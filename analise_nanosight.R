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
#Para analise de Levene as colunas de transtornos não podem ser numeros, portanto (F=False T=True)
nanosight_plus_sampleinfo <- nanosight_plus_sampleinfo %>%
  mutate(
    dcany = ifelse(dcany == 0, "F", "T"),
    dcmadep = ifelse(dcmadep == 0, "F", "T"),
    dcanyanx = ifelse(dcanyanx == 0, "F", "T"),
    dcgena = ifelse(dcgena == 0, "F", "T"),
    dcanyhk = ifelse(dcanyhk == 0, "F", "T"),
    dcpsych = ifelse(dcpsych == 0, "F", "T"),
    dcmania = ifelse(dcmania == 0, "F", "T"),
    dcptsd = ifelse(dcptsd == 0, "F", "T")
  )
##Retirando duas linhas da tabela (pq no boxplot de média de tamanho elas ficaram muito fora)
nanosight_plus_sampleinfo_sem_outliers <- nanosight_plus_sampleinfo[-c(133,170), ]
##Tabela só com w1 e só w2
nanosight_w1 <- subset(nanosight_plus_sampleinfo, wave == "w1")
nanosight_w2 <- subset(nanosight_plus_sampleinfo, wave == "w2")
nanosight_w1_sem_outliers <- subset(nanosight_plus_sampleinfo_sem_outliers, wave == "w1")
nanosight_w2_sem_outliers <- subset(nanosight_plus_sampleinfo_sem_outliers, wave == "w2")
##Amostras sem pares
linhas_sem_pares <- !duplicated(nanosight_plus_sampleinfo$subjectid) & !duplicated(nanosight_plus_sampleinfo$subjectid, fromLast = TRUE)
nanosight_sem_pares <- nanosight_plus_sampleinfo[linhas_sem_pares, ] #perde 7
linhas_sem_pares_sem_outliers <- !duplicated(nanosight_plus_sampleinfo_sem_outliers$subjectid) & !duplicated(nanosight_plus_sampleinfo_sem_outliers$subjectid, fromLast = TRUE)
nanosight_sem_pares_sem_outliers <- nanosight_plus_sampleinfo_sem_outliers[linhas_sem_pares_sem_outliers, ] #perde 9 (os 2 pares dos 2 outliers)

#Contagem dos dados
##Número de amostras dos grupos e waves
tabela_contagem_trajetoria_wave <- table(nanosight_plus_sampleinfo$Trajetoria, nanosight_plus_sampleinfo$wave)
tabela_contagem_trajetoria_wave_sem_outliers <- table(nanosight_plus_sampleinfo_sem_outliers$Trajetoria, nanosight_plus_sampleinfo_sem_outliers$wave)

#Análise de Variância
##lista de formulas para todas as amostras
formulas <- list(
  tamanho_mean_average ~ Trajetoria,
  tamanho_mean_average ~ TP_longitudinal,
  tamanho_mean_average ~ Grupo, 
  tamanho_mean_average ~ site, 
  tamanho_mean_average ~ dcany, 
  tamanho_mean_average ~ dcmadep,
  tamanho_mean_average ~ dcanyanx, 
  tamanho_mean_average ~ dcgena, 
  tamanho_mean_average ~ dcanyhk,
  tamanho_mean_average ~ dcpsych, 
  tamanho_mean_average ~ dcptsd, 
  EV_pequenas_porcentagem ~ Trajetoria, 
  EV_pequenas_porcentagem ~ TP_longitudinal, 
  concentracao_real ~ Trajetoria
)
##nomeando as formulas
apelidos <- c(
  "tamanho_trajetoria",
  "tamanho_tp_longitudinal",
  "tamanho_grupo",
  "tamanho_estado",
  "tamanho_dcany",
  "tamanho_dcmadep",
  "tamanho_dcanyanx",
  "tamanho_dcgena",
  "tamanho_dcanyhk",
  "tamanho_dcpsych",
  "tamanho_pctsd",
  "porcentagem_trajetoria",
  "porcentagem_tp_longitudinal",
  "concentracao_trajetoria"
)
##atribuindo os apelidos às fórmulas
names(formulas) <- apelidos
##Função para criar modelo ANOVA e gerar resumo
criar_anova <- function(formula, dataset, apelidos) {
  modelo <- aov(formula, data = dataset)
  resumo <- summary(modelo)
  assign(paste("summary_modelo_anova_", apelidos, sep = ""), resumo, envir = .GlobalEnv)
  return(resumo)
}
##loop para criar os modelos e resumos para todas as amostras e para a exclusão dos outliers
p_values <- c()
p_values_sem_outliers <- c()
for(i in seq_along(formulas)) {
  #para a tabela com outliers
  resumo <- criar_anova(formulas[[i]], nanosight_plus_sampleinfo, apelidos[i])
  p_value <- resumo[[1]]$`Pr(>F)`[1]
  p_values <- c(p_values, p_value)
  #para a tabela sem outliers
  resumo_sem_outliers <- criar_anova(formulas[[i]], nanosight_plus_sampleinfo_sem_outliers, apelidos[i])
  p_value_sem_outliers <- resumo_sem_outliers[[1]]$`Pr(>F)`[1]
  p_values_sem_outliers <- c(p_values_sem_outliers, p_value_sem_outliers)
}
## Criar a tabela combinada com os resultados de ambas as análises
tabela_p_values_anova <- data.frame(Apelido = apelidos, P_Valor = p_values, P_Valor_Sem_Outliers = p_values_sem_outliers)
##lista de formulas para w1 e w2 + apelidos
formulas_waves <- list(
  tamanho_mean_average ~ Trajetoria,
  tamanho_mean_average ~ TP_transversal, 
  EV_pequenas_porcentagem ~ Trajetoria, 
  EV_pequenas_porcentagem ~ TP_transversal
)
apelidos_waves <- c(
  "tamanho_trajetoria",
  "tamanho_tp_transversal",
  "porcentagem_trajetoria",
  "porcentagem_tp_transversal"
)
names(formulas_waves) <- apelidos_waves
##função para criar modelo ANOVA e gerar resumo para waves
criar_anova_waves <- function(formula, dataset, apelidos_waves) {
  modelo_waves <- aov(formula, data = dataset)
  resumo_waves <- summary(modelo_waves)
  assign(paste("summary_modelo_anova_waves", apelidos_waves, sep = ""), resumo_waves, envir = .GlobalEnv)
  return(resumo_waves)
}
##loop para criar os modelos e resumos para todas as amostras e para a exclusão dos outliers
p_values_w1 <- c()
p_values_w2 <- c()
p_values_w1_sem_outliers <- c()
p_values_w2_sem_outliers <- c()
for(i in seq_along(formulas_waves)) {
  #para a tabela w1
  resumo_w1 <- criar_anova_waves(formulas_waves[[i]], nanosight_w1, apelidos_waves[i])
  p_value_w1 <- resumo_w1[[1]]$`Pr(>F)`[1]
  p_values_w1 <- c(p_values_w1, p_value_w1)
  #para a tabela w2
  resumo_w2 <- criar_anova_waves(formulas_waves[[i]], nanosight_w2, apelidos_waves[i])
  p_value_w2 <- resumo_w2[[1]]$`Pr(>F)`[1]
  p_values_w2 <- c(p_values_w2, p_value_w2)
  #para a tabela w1 sem outliers
  resumo_w1_sem_outliers <- criar_anova_waves(formulas_waves[[i]], nanosight_w1_sem_outliers, apelidos_waves[i])
  p_value_w1_sem_outliers <- resumo_w1_sem_outliers[[1]]$`Pr(>F)`[1]
  p_values_w1_sem_outliers <- c(p_values_w1_sem_outliers, p_value_w1_sem_outliers)
  #para a tabela w2 sem outliers
  resumo_w2_sem_outliers <- criar_anova_waves(formulas_waves[[i]], nanosight_w2_sem_outliers, apelidos_waves[i])
  p_value_w2_sem_outliers <- resumo_w2_sem_outliers[[1]]$`Pr(>F)`[1]
  p_values_w2_sem_outliers <- c(p_values_w2_sem_outliers, p_value_w2_sem_outliers)
}
## Criar a tabela combinada com os resultados de w1/w2 e w1/w2 sem outliers
tabela_p_values_anova_waves <- data.frame(Apelido = apelidos_waves, P_Valor_w1 = p_values_w1, P_Valor_w2 = p_values_w2, P_Valor_w1_sem_outliers = p_values_w1_sem_outliers, P_Valor_w2_sem_outliers = p_values_w2_sem_outliers)

#Premissas da ANOVA
##Normalidade (Shapiro não sera considerado porque a curva mostrou distribuição normal)
###QQPlot para visualizar a distribuição das amostras em relação a uma distribuição normal
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
##Homogeneidade (Teste de Levene)
library("car")
##todas
levene_results <- c()
levene_results_sem_outliers <- c()
for (i in seq_along(formulas)) {
  #para todas as amostras
  modelo_levene <- aov(formulas[[i]], data = nanosight_plus_sampleinfo)
  levene <- leveneTest(modelo_levene)
  levene_results <- c(levene_results, levene$`Pr(>F)`[1])
  #para amostras sem outliers
  modelo_levene_sem_outliers <- aov(formulas[[i]], data = nanosight_plus_sampleinfo_sem_outliers)
  levene_sem_outliers <- leveneTest(modelo_levene_sem_outliers)
  levene_results_sem_outliers <- c(levene_results_sem_outliers, levene_sem_outliers$`Pr(>F)`[1])
}
tabela_levene <- data.frame(Apelido = apelidos, P_Valor_Levene = levene_results, P_Valor_Levene_sem_outliers = levene_results_sem_outliers)
##waves
levene_results_w1 <- c()
levene_results_w2 <- c()
levene_results_w1_sem_outliers <- c()
levene_results_w2_sem_outliers <- c()
for (i in seq_along(formulas_waves)) {
  #para w1
  modelo_levene_w1 <- aov(formulas_waves[[i]], data = nanosight_w1)
  levene_w1 <- leveneTest(modelo_levene_w1)
  levene_results_w1 <- c(levene_w1, levene$`Pr(>F)`[1])
  #para w2
  modelo_levene_w2 <- aov(formulas_waves[[i]], data = nanosight_w2)
  levene_w2 <- leveneTest(modelo_levene_w2)
  levene_results_w2 <- c(levene_w2, levene$`Pr(>F)`[1])
  #para w1 sem outliers
  modelo_levene_w1_sem_outliers <- aov(formulas_waves[[i]], data = nanosight_w1_sem_outliers)
  levene_w1_sem_outliers <- leveneTest(modelo_levene_w1_sem_outliers)
  levene_results_w1_sem_outliers <- c(levene_w1_sem_outliers, levene$`Pr(>F)`[1])
  #para w2 sem outliers
  modelo_levene_w2_sem_outliers <- aov(formulas_waves[[i]], data = nanosight_w2_sem_outliers)
  levene_w2_sem_outliers <- leveneTest(modelo_levene_w2_sem_outliers)
  levene_results_w2_sem_outliers <- c(levene_w2_sem_outliers, levene$`Pr(>F)`[1])
}
tabela_levene_waves <- data.frame(Apelido = apelidos_waves, P_Valor_Levene_w1 = levene_results_w1, P_Valor_Levene_w2 = levene_results_w2, P_Valor_Levene_w1_sem_outliers = levene_results_w1_sem_outliers, P_Valor_Levene_w2_sem_outliers = levene_results_w2_sem_outliers)

#Anova de Welch (caso não exista homogeneidade de variâncias) #obs tirei o dcmania pq só tinha uma pessoa e dava erro no teste
##todas
executar_teste_anova_welch <- function(formula, data) {
  resultado_teste_anova_welch <- oneway.test(formula, data = data, var.equal = FALSE)
  return(resultado_teste_anova_welch$p.value)
}
p_valores_welch <- numeric(length(formulas))
p_valores_welch_sem_outliers <- numeric(length(formulas))
for (i in seq_along(formulas)) {
  #todas
  resultado_teste_welch <- oneway.test(formulas[[i]], data = nanosight_plus_sampleinfo, var.equal = FALSE)
  p_valores_welch[i] <- as.character(resultado_teste_welch$p.value)
  #sem outliers
  resultado_teste_welch_sem_outliers <- oneway.test(formulas[[i]], data = nanosight_plus_sampleinfo_sem_outliers, var.equal = FALSE)
  p_valores_welch_sem_outliers[i] <- as.character(resultado_teste_welch_sem_outliers$p.value)
}
tabela_welch <- data.frame(
  Teste = apelidos,
  P_Valor = p_valores_welch, P_Valor_sem_outliers = p_valores_welch_sem_outliers
)
##waves
###w1
p_valores_welch_w1 <- numeric(length(formulas_waves))
p_valores_welch_w2 <- numeric(length(formulas_waves))
p_valores_welch_w1_sem_outliers <- numeric(length(formulas_waves))
p_valores_welch_w2_sem_outliers <- numeric(length(formulas_waves))
for (i in seq_along(formulas_waves)) {
  #w1
  resultado_welch_w1 <- oneway.test(formulas_waves[[i]], data = nanosight_w1, var.equal = FALSE)
  p_valores_welch_w1[i] <- as.character(resultado_welch_w1$p.value)
  #w1 sem outliers
  resultado_welch_w1_sem_outliers <- oneway.test(formulas_waves[[i]], data = nanosight_w1_sem_outliers, var.equal = FALSE)
  p_valores_welch_w1_sem_outliers[i] <- as.character(resultado_welch_w1_sem_outliers$p.value)
  #w2
  resultado_welch_w2 <- oneway.test(formulas_waves[[i]], data = nanosight_w2, var.equal = FALSE)
  p_valores_welch_w2[i] <- as.character(resultado_welch_w2$p.value)
  #w2 sem outliers
  resultado_welch_w2_sem_outliers <- oneway.test(formulas_waves[[i]], data = nanosight_w2_sem_outliers, var.equal = FALSE)
  p_valores_welch_w2_sem_outliers[i] <- as.character(resultado_welch_w2_sem_outliers$p.value)
}
tabela_welch_waves <- data.frame(
  Teste = apelidos_waves,
  P_Valor_w1 = p_valores_welch_w1, P_Valor_w1_sem_outliers = p_valores_welch_w1_sem_outliers, P_Valor_w2 = p_valores_welch_w2, P_Valor_w2_sem_outliers = p_valores_welch_w2_sem_outliers
)

#Comparação das médias
##Teste de Games-Howell
library("remotes")
remotes::install_github("matherion/userfriendlyscience")
library("userfriendlyscience")
###com outliers
oneway(nanosight_plus_sampleinfo$Grupo, y=nanosight_plus_sampleinfo$tamanho_mean_average, posthoc = 'games-howell') #nenhum
oneway(nanosight_plus_sampleinfo$Trajetoria, y=nanosight_plus_sampleinfo$EV_pequenas_porcentagem, posthoc = 'games-howell') #B-A, C-B, D-B (B tem menor porcentagem de VEs pequenas)
oneway(nanosight_plus_sampleinfo$Trajetoria, y=nanosight_plus_sampleinfo$concentracao_real, posthoc = 'games-howell') #deu erro
oneway(nanosight_w1$Trajetoria, y=nanosight_w1$tamanho_mean_average, posthoc = 'games-howell') #B-A e D-B (B tem maior tamanho medio de EVs)
oneway(nanosight_w1$Trajetoria, y=nanosight_w1$EV_pequenas_porcentagem, posthoc = 'games-howell') #B-A e D-B (B tem menor porcentagem de VEs pequenas)
oneway(nanosight_w2$Trajetoria, y=nanosight_w2$EV_pequenas_porcentagem, posthoc = 'games-howell') #D-B (B tem menor porcentagem de VEs pequenas)
###sem outliers
oneway(nanosight_plus_sampleinfo_sem_outliers$Trajetoria, y = nanosight_plus_sampleinfo_sem_outliers$tamanho_mean_average, posthoc = 'games-howell') #B-A e D-B (B tem tamanho medio de VEs maior)
oneway(nanosight_plus_sampleinfo_sem_outliers$Grupo, y=nanosight_plus_sampleinfo_sem_outliers$tamanho_mean_average, posthoc = 'games-howell') #nenhum
oneway(nanosight_w1$Trajetoria, y=nanosight_w1$tamanho_mean_average, posthoc = 'games-howell') #B-A e D-B
oneway(nanosight_plus_sampleinfo_sem_outliers$Trajetoria, y=nanosight_plus_sampleinfo_sem_outliers$EV_pequenas_porcentagem, posthoc = 'games-howell') #B-A, C-B, D-B (B tem menor porcentagem de VEs pequenas)
oneway(nanosight_plus_sampleinfo_sem_outliers$Trajetoria, y=nanosight_plus_sampleinfo_sem_outliers$concentracao_real, posthoc = 'games-howell') #deu erro
oneway(nanosight_w1_sem_outliers$Trajetoria, y=nanosight_w1_sem_outliers$tamanho_mean_average, posthoc = 'games-howell') #B-A e D-B (B tem maior tamanho medio de EVs)
oneway(nanosight_w1_sem_outliers$Trajetoria, y=nanosight_w1_sem_outliers$EV_pequenas_porcentagem, posthoc = 'games-howell') #B-A e D-B (B tem menor porcentagem de VEs pequenas)
oneway(nanosight_w2_sem_outliers$Trajetoria, y=nanosight_w2_sem_outliers$EV_pequenas_porcentagem, posthoc = 'games-howell') #B-A, D-B (B tem menor porcentagem de VEs pequenas)

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
t.test(nanosight_pares_B_w1$tamanho_mean_average, nanosight_pares_B_w2$tamanho_mean_average, paired = TRUE) #sem diferença (p=0,4)
t.test(nanosight_pares_B_w1$EV_pequenas_porcentagem, nanosight_pares_B_w2$EV_pequenas_porcentagem, paired = TRUE) #sem diferença (p=0,7)
t.test(nanosight_pares_C_w1$tamanho_mean_average, nanosight_pares_C_w2$tamanho_mean_average, paired = TRUE) #sem diferença (p=0,2)
t.test(nanosight_pares_C_w1$EV_pequenas_porcentagem, nanosight_pares_C_w2$EV_pequenas_porcentagem, paired = TRUE) #sem diferença (p=0,7)
t.test(nanosight_pares_D_w1$tamanho_mean_average, nanosight_pares_D_w2$tamanho_mean_average, paired = TRUE) #sem diferença (p=0,8)
t.test(nanosight_pares_D_w1$EV_pequenas_porcentagem, nanosight_pares_D_w2$EV_pequenas_porcentagem, paired = TRUE) #sem diferença (p=0,5)

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

