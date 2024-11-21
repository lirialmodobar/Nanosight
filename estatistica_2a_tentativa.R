#Definir pasta de trabalho
setwd("C:/Users/Belle/Documents/Belle - Nanosight")

#Testes de normalidade (premissa para o ANOVA)
shapiro_tamanho_w1 <- shapiro.test(nanosight_intersect_conc_nan_w1$tamanho_mean_average) #p 0.0059
shapiro_tamanho_w2 <- shapiro.test(nanosight_intersect_conc_nan_w2$tamanho_mean_average) #p 0.0704 [NORMAL]
shapiro_porcentagem_w1 <- shapiro.test(nanosight_intersect_conc_nan_w1$EV_pequenas_porcentagem) #p 9.05e-09
shapiro_porcentagem_w2 <- shapiro.test(nanosight_intersect_conc_nan_w2$EV_pequenas_porcentagem) #p 7.86e-10
shapiro_concentracao_w1 <- shapiro.test(nanosight_intersect_conc_nan_w1$concentracao_average) #p 3.09e-07
shapiro_concentracao_w2 <- shapiro.test(nanosight_intersect_conc_nan_w2$concentracao_average) #p 7.99e-07

#Testes de homogeneidade de variâncias (premissa para o ANOVA)
modelo_w1_mean <- aov(tamanho_mean_average~Trajetoria, data = nanosight_intersect_conc_nan_w1)
levene_w1_mean <- leveneTest(modelo_w1_mean) #p = 0.004790367
modelo_w2_mean <- aov(tamanho_mean_average~Trajetoria, data = nanosight_intersect_conc_nan_w2)
levene_w2_mean <- leveneTest(modelo_w2_mean) #p = 0.1519675 [HOMOGÊNEO]
modelo_w1_porcentagem <- aov(EV_pequenas_porcentagem~Trajetoria, data = nanosight_intersect_conc_nan_w1)
levene_w1_porcentagem <- leveneTest(modelo_w1_porcentagem) #p = 0.0002329829
modelo_w2_porcentagem <- aov(EV_pequenas_porcentagem~Trajetoria, data = nanosight_intersect_conc_nan_w2)
levene_w2_porcentagem <- leveneTest(modelo_w2_porcentagem) #p = 0.01346372
modelo_w1_concentracao <- aov(concentracao_real~Trajetoria, data = nanosight_intersect_conc_nan_w1)
levene_w1_concentracao <- leveneTest(modelo_w1_concentracao) #p = 0.2936355 [HOMOGÊNEO]
modelo_w2_concentracao <- aov(concentracao_real~Trajetoria, data = nanosight_intersect_conc_nan_w2)
levene_w2_concentracao <- leveneTest(modelo_w2_concentracao) #p = 0.1667132 [HOMOGÊNEO]

#Realizando ANOVA
library(emmeans)
# Ajuste do modelo ANOVA para usar com emmeans
modelo_aov_mean_w1 <- aov(tamanho_mean_average ~ Trajetoria, data = nanosight_intersect_conc_nan_w1)
modelo_aov_mean_w2 <- aov(tamanho_mean_average ~ Trajetoria, data = nanosight_intersect_conc_nan_w2)
modelo_aov_porcentagem_w1 <- aov(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_intersect_conc_nan_w1)
modelo_aov_porcentagem_w2 <- aov(EV_pequenas_porcentagem ~ Trajetoria, data = nanosight_intersect_conc_nan_w2)
modelo_aov_concentracao_w1 <- aov(concentracao_average ~ Trajetoria, data = nanosight_intersect_conc_nan_w1)
modelo_aov_concentracao_w2 <- aov(concentracao_average ~ Trajetoria, data = nanosight_intersect_conc_nan_w2)

# Comparações post-hoc
posthoc_mean_w1 <- emmeans(modelo_aov_mean_w1, pairwise ~ Trajetoria)
posthoc_mean_w2 <- emmeans(modelo_aov_mean_w2, pairwise ~ Trajetoria)
posthoc_porcentagem_w1 <- emmeans(modelo_aov_porcentagem_w1, pairwise ~ Trajetoria)
posthoc_porcentagem_w2 <- emmeans(modelo_aov_porcentagem_w2, pairwise ~ Trajetoria)
posthoc_concentracao_w1 <- emmeans(modelo_aov_concentracao_w1, pairwise ~ Trajetoria)
posthoc_concentracao_w2 <- emmeans(modelo_aov_concentracao_w2, pairwise ~ Trajetoria)

#### Análises corrigindo para o efeito batch
##ANOVA robusta de 2 vias
#tamanho/batch/trajetoria na w1
anova_tam_batch_traj_w1 <- aov(tamanho_mean_average ~ Trajetoria * batch_nan, data = nanosight_intersect_conc_nan_w1)
summary_anova_tam_batch_traj_w1 <- summary(anova_tam_batch_traj_w1)
anova_df_tam_batch_traj_w1 <- as.data.frame(summary_anova_tam_batch_traj_w1[[1]])
#tamanho/batch/trajetoria na w2
anova_tam_batch_traj_w2 <- aov(tamanho_mean_average ~ Trajetoria * batch_nan, data = nanosight_intersect_conc_nan_w2)
summary_anova_tam_batch_traj_w2 <- summary(anova_tam_batch_traj_w2)
anova_df_tam_batch_traj_w2 <- as.data.frame(summary_anova_tam_batch_traj_w2[[1]])
#porcentagem/batch/trajetoria na w1
anova_porc_batch_traj_w1 <- aov(EV_pequenas_porcentagem ~ Trajetoria * batch_nan, data = nanosight_intersect_conc_nan_w1)
summary_anova_porc_batch_traj_w1 <- summary(anova_porc_batch_traj_w1)
anova_df_porc_batch_traj_w1 <- as.data.frame(summary_anova_porc_batch_traj_w1[[1]])
#porcentagem/batch/trajetoria na w2
anova_porc_batch_traj_w2 <- aov(EV_pequenas_porcentagem ~ Trajetoria * batch_nan, data = nanosight_intersect_conc_nan_w2)
summary_anova_porc_batch_traj_w2 <- summary(anova_porc_batch_traj_w2)
anova_df_porc_batch_traj_w2 <- as.data.frame(summary_anova_porc_batch_traj_w2[[1]])
#concentração/batch/trajetoria na w1
anova_conc_batch_traj_w1 <- aov(concentracao_average ~ Trajetoria * batch_nan, data = nanosight_intersect_conc_nan_w1)
summary_anova_conc_batch_traj_w1 <- summary(anova_conc_batch_traj_w1)
anova_df_conc_batch_traj_w1 <- as.data.frame(summary_anova_conc_batch_traj_w1[[1]])
#concentração/batch/trajetoria na w2
anova_conc_batch_traj_w2 <- aov(concentracao_average ~ Trajetoria * batch_nan, data = nanosight_intersect_conc_nan_w2)
summary_anova_conc_batch_traj_w2 <- summary(anova_conc_batch_traj_w2)
anova_df_conc_batch_traj_w2 <- as.data.frame(summary_anova_conc_batch_traj_w2[[1]])

##Teste de Tukey para comparações múltiplas entre as trajetórias
#Tamanho para batch e Trajetória na w1
nanosight_intersect_conc_nan_w1$batch_nan <- as.factor(nanosight_intersect_conc_nan_w1$batch_nan)
nanosight_intersect_conc_nan_w1$Trajetoria <- as.factor(nanosight_intersect_conc_nan_w1$Trajetoria)
tukey_tam_batch_traj_w1 <- tukey_hsd(anova_tam_batch_traj_w1, "Trajetoria:batch_nan")
summary(tukey_tam_batch_traj_w1)
#Tamanho para batch e Trajetória na w2
nanosight_intersect_conc_nan_w2$batch_nan <- as.factor(nanosight_intersect_conc_nan_w2$batch_nan)
nanosight_intersect_conc_nan_w2$Trajetoria <- as.factor(nanosight_intersect_conc_nan_w2$Trajetoria)
tukey_tam_batch_traj_w2 <- tukey_hsd(anova_tam_batch_traj_w2, "Trajetoria:batch_nan")
summary(tukey_tam_batch_traj_w2)
#Porcentagem para batch e Trajetória na w1
nanosight_intersect_conc_nan_w1$batch_nan <- as.factor(nanosight_intersect_conc_nan_w1$batch_nan)
nanosight_intersect_conc_nan_w1$Trajetoria <- as.factor(nanosight_intersect_conc_nan_w1$Trajetoria)
tukey_porc_batch_traj_w1 <- tukey_hsd(anova_porc_batch_traj_w1, "Trajetoria:batch_nan")
summary(tukey_porc_batch_traj_w1)
#Porcentagem para batch e Trajetória na w2
nanosight_intersect_conc_nan_w2$batch_nan <- as.factor(nanosight_intersect_conc_nan_w2$batch_nan)
nanosight_intersect_conc_nan_w2$Trajetoria <- as.factor(nanosight_intersect_conc_nan_w2$Trajetoria)
tukey_porc_batch_traj_w2 <- tukey_hsd(anova_porc_batch_traj_w2, "Trajetoria:batch_nan")
summary(tukey_porc_batch_traj_w2)
#Concentração para batch e Trajetória na w1
nanosight_intersect_conc_nan_w1$batch_nan <- as.factor(nanosight_intersect_conc_nan_w1$batch_nan)
nanosight_intersect_conc_nan_w1$Trajetoria <- as.factor(nanosight_intersect_conc_nan_w1$Trajetoria)
tukey_conc_batch_traj_w1 <- tukey_hsd(anova_conc_batch_traj_w1, "Trajetoria:batch_nan")
summary(tukey_conc_batch_traj_w1)
#Concentração para batch e Trajetória na w2
nanosight_intersect_conc_nan_w2$batch_nan <- as.factor(nanosight_intersect_conc_nan_w2$batch_nan)
nanosight_intersect_conc_nan_w2$Trajetoria <- as.factor(nanosight_intersect_conc_nan_w2$Trajetoria)
tukey_conc_batch_traj_w2 <- tukey_hsd(anova_conc_batch_traj_w2, "Trajetoria:batch_nan")
summary(tukey_conc_batch_traj_w2)
###Tabela dos resultados dos testes de Tukey
# Adicionar uma coluna de origem em cada tabela antes de combinar
tukey_tam_batch_traj_w1$Source <- "Tamanho - w1"
tukey_tam_batch_traj_w2$Source <- "Tamanho - w2"
tukey_porc_batch_traj_w1$Source <- "Porcentagem - w1"
tukey_porc_batch_traj_w2$Source <- "Porcentagem - w2"
tukey_conc_batch_traj_w1$Source <- "Concentração - w1"
tukey_conc_batch_traj_w2$Source <- "Concentração - w2"
#Tamanho na w1
tukey_tamanho_W1 <- tukey_tam_batch_traj_w1
tukey_tamanho_W1 <- tukey_tamanho_W1[, c("Source", setdiff(colnames(tukey_tamanho_W1), "Source"))] #Source na 1a coluna
pdf("plots/tukey_tamanho_w1.pdf", width = 15, height = 25)
grid.newpage()  # Inicia uma nova página para o gráfico
grid.text("Tukey Test - Mean Size - w1", x = 0.5, y = 0.9, gp = gpar(fontsize = 16, fontface = "bold"))  # Título no topo da página
grid.table(tukey_tamanho_W1)
dev.off()
#Tamanho na w2
tukey_tamanho_W2 <- tukey_tam_batch_traj_w2
tukey_tamanho_W2 <- tukey_tamanho_W2[, c("Source", setdiff(colnames(tukey_tamanho_W2), "Source"))] #Source na 1a coluna
pdf("plots/tukey_tamanho_w2.pdf", width = 15, height = 25)
grid.newpage()  # Inicia uma nova página para o gráfico
grid.text("Tukey Test - Mean Size - w2", x = 0.5, y = 0.9, gp = gpar(fontsize = 16, fontface = "bold"))  # Título no topo da página
grid.table(tukey_tamanho_W2)
dev.off()
#Porcentagem na w1
tukey_porcentagem_W1 <- tukey_tam_batch_traj_w1
tukey_porcentagem_W1 <- tukey_porcentagem_W1[, c("Source", setdiff(colnames(tukey_porcentagem_W1), "Source"))] #Source na 1a coluna
pdf("plots/tukey_porcentagem_w1.pdf", width = 15, height = 25)
grid.newpage()  # Inicia uma nova página para o gráfico
grid.text("Tukey Test - Percentage of Small EV's - w1", x = 0.5, y = 0.9, gp = gpar(fontsize = 16, fontface = "bold"))  # Título no topo da página
grid.table(tukey_porcentagem_W1)
dev.off()
#Porcentagem na w2
tukey_porcentagem_W2 <- tukey_tam_batch_traj_w2
tukey_porcentagem_W2 <- tukey_porcentagem_W2[, c("Source", setdiff(colnames(tukey_porcentagem_W2), "Source"))] #Source na 1a coluna
pdf("plots/tukey_porcentagem_w2.pdf", width = 15, height = 25)
grid.newpage()  # Inicia uma nova página para o gráfico
grid.text("Tukey Test - Percentage of Small EV's - w2", x = 0.5, y = 0.9, gp = gpar(fontsize = 16, fontface = "bold"))  # Título no topo da página
grid.table(tukey_porcentagem_W2)
dev.off()
#Concentração na w1
tukey_concentracao_W1 <- tukey_tam_batch_traj_w1
tukey_concentracao_W1 <- tukey_concentracao_W1[, c("Source", setdiff(colnames(tukey_concentracao_W1), "Source"))] #Source na 1a coluna
pdf("plots/tukey_concentracao_w1.pdf", width = 15, height = 25)
grid.newpage()  # Inicia uma nova página para o gráfico
grid.text("Tukey Test - Concentration of particles - w1", x = 0.5, y = 0.9, gp = gpar(fontsize = 16, fontface = "bold"))  # Título no topo da página
grid.table(tukey_concentracao_W1)
dev.off()
#Concentração na w2
tukey_concentracao_W2 <- tukey_tam_batch_traj_w2
tukey_concentracao_W2 <- tukey_concentracao_W2[, c("Source", setdiff(colnames(tukey_concentracao_W2), "Source"))] #Source na 1a coluna
pdf("plots/tukey_concentracao_w2.pdf", width = 15, height = 25)
grid.newpage()  # Inicia uma nova página para o gráfico
grid.text("Tukey Test - Concentration of particles - w2", x = 0.5, y = 0.9, gp = gpar(fontsize = 16, fontface = "bold"))  # Título no topo da página
grid.table(tukey_concentracao_W2)
dev.off()

################# Mais uma tentativa considerando o número de amostras de Trajetoria na batch

