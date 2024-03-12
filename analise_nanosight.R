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
#Juntando todos os dados obtidos do nanosight em uma tabela
tabela_tudo_nanosight <- data.frame(id_sample, concentracao_real, tamanho_mean_average, tamanho_mode_average)
duplicatas <- duplicated(tabela_tudo_nanosight[, c("id_sample")], fromLast = TRUE) #a 2a leitura é a correta
nanosight_sem_duplicatas <- subset(tabela_tudo_nanosight, !duplicatas)
#Juntar dados do nanosight com informações da amostra
sample_information <- read.csv("sampleinformation_liriel320_atualizada.csv") #chamando/lendo/importando infos da amostra
sample_information$id_sample <- paste(sample_information$subjectid, sample_information$wave, sep = "_")
nanosight_plus_sampleinfo <- inner_join(nanosight_sem_duplicatas, sample_information, by="id_sample")
sem_correspondencia <- anti_join(nanosight_sem_duplicatas, sample_information, by = "id_sample")
write.csv(nanosight_plus_sampleinfo, "nanosight_plus_sampleinfo_all.csv", row.names = FALSE)
#Amostras sem análise no Nanosight
sample_info_sem_nanosight <- anti_join(sample_information, nanosight_sem_duplicatas, by="id_sample")
#QQPlot para analisar se as amostras seguem uma distribuição normal
dados_mean <- rnorm(nanosight_plus_sampleinfo$tamanho_mean_average)
pdf("qqplot_mean.pdf")
qqnorm(dados_mean)
qqline(dados_mean, col = "red")
dev.off()
dados_mode <- rnorm(nanosight_plus_sampleinfo$tamanho_mode_average)
pdf("qqplot_mode.pdf")
qqnorm(dados_mode)
qqline(dados_mode, col = "red")
dev.off()
###Testes de Shapiro
shapiro_mean <- shapiro.test(nanosight_plus_sampleinfo$tamanho_mean_average)
shapiro_mode <- shapiro.test(nanosight_plus_sampleinfo$tamanho_mode_average)
shapiro_concentracao <- shapiro.test(nanosight_plus_sampleinfo$concentracao_real)
shapiro_testes <- data.frame(mean = c(shapiro_mean), mode = c(shapiro_mode), concentracao = c(shapiro_concentracao))
shapiro_testes <- select(shapiro_testes, mean.p.value, mode.p.value, concentracao.p.value)
#Contando número de amostras dos grupos e waves
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
#Amostras sem pares
linhas_sem_pares <- !duplicated(nanosight_plus_sampleinfo$subjectid) & !duplicated(nanosight_plus_sampleinfo$subjectid, fromLast = TRUE)
nanosight_sem_pares <- nanosight_plus_sampleinfo[linhas_sem_pares, ]
#Boxplot da Média dos Tamanhos
library(ggplot2)
boxplot_tamanho_mean <- ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_boxplot(fill = "#DDA0DD", color = "#800080", alpha = 0.7) +
  labs(title = "Mean Size Boxplot by Group", x = "Group", y = "Mean size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mean.pdf", plot = boxplot_tamanho_mean + theme(text = element_text(size=20)), width = 8, height = 6)
boxplot_tamanho_mean_w1 <- ggplot(subset(nanosight_plus_sampleinfo, wave == "w1"), aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_boxplot(fill = "#DDA0DD", color = "#800080", alpha = 0.7) +
  labs(title = "Mean Size Boxplot for Category w1", x = "Group", y = "Mean size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mean_w1.pdf", plot = boxplot_tamanho_mean_w1 + theme(text = element_text(size=20)), width = 8, height = 6)
boxplot_tamanho_mean_w2 <- ggplot(subset(nanosight_plus_sampleinfo, wave == "w2"), aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_boxplot(fill = "#DDA0DD", color = "#800080", alpha = 0.7) +
  labs(title = "Mean Size Boxplot for Category w2", x = "Group", y = "Mean size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mean_w2.pdf", plot = boxplot_tamanho_mean_w2 + theme(text = element_text(size=20)), width = 8, height = 6)
#Teste LSD
library(lmerTest)
library(lsmeans)
modelo_anova <- lmer(tamanho_mean_average ~ Trajetoria + (1|Trajetoria), data = nanosight_plus_sampleinfo)
resultado_anova <- anova(modelo_anova)
if (resultado_anova$`Pr(>F)`[1] < 0.05) {
  resultado_lsd <- lsmeans::lstrends(modelo_anova, ~Trajetoria)
  print(resultado_lsd)
} else {
  print("O teste ANOVA não é significativo. Não é apropriado realizar o teste LSD.")
}
