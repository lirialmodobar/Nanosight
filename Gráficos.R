library(ggplot2)
library(tidyverse)
library(ggpubr)
library(Hmisc)

#Definir pasta de trabalho e importar scripts necessários
setwd("C:/Users/Belle/Documents/Belle - Nanosight")

#Graficos IMPORTANTES

# Gráfico de dispersão de concentração vs. tamanho, colorido por Grupo
ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average, y = concentracao_real, color = Trajetoria)) +
  geom_point(size = 3, alpha = 0.7) +  # Pontinhos com tamanho e transparência ajustados
  labs(title = "Gráfico de Dispersão: Concentração vs. Tamanho", 
       x = "Tamanho Médio (nm)", 
       y = "Concentração (partículas/ml)") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),  # Ajuste do tamanho da fonte geral
    axis.text = element_text(size = 14),  # Tamanho da fonte dos textos dos eixos
    axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5),  # Tamanho e alinhamento centralizado do título
    legend.text = element_text(size = 14),  # Tamanho da fonte da legenda
    legend.title = element_text(size = 16)  # Tamanho do título da legenda
  ) +
  scale_color_manual(values = c("A" = "#0f8bf7", "B" = "#f7830f", "C" = "#ff12d0", "D" = "#2a8008")) + # Cores para os grupos
  scale_y_log10() 
#Salvar
ggsave("plots/dispersao_concentracao_tamanho.pdf", width = 8, height = 6)

#Boxplot Jitter da distribuição da média dos tamanhos nas Trajetorias
##Todas as amostras
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = tamanho_mean_average, fill = Grupo)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Boxplot com transparência e sem outliers
  geom_jitter(aes(color = Grupo), size = 2, width = 0.2, alpha = 0.7) +  # Adiciona jitter
  scale_fill_manual(values = c("#0f8bf7", "#5aaef9", "#f7830f", "#f9a34b", "#ff12d0", "#ff5adf", "#2a8008", "#52a835"), # Paleta de cores para boxplot
                    labels = c("Controle-w1", "Controle-w2", "Incidente-w1", "Incidente-w2", "Remitente-w1", "Remitente-w2", "Persistente-w1", "Persistente-w2")) + 
  scale_color_manual(values = c("#0f8bf7", "#5aaef9", "#f7830f", "#f9a34b", "#ff12d0", "#ff5adf", "#2a8008", "#52a835"), # Paleta de cores para jitter
                     labels = c("Controle-w1", "Controle-w2", "Incidente-w1", "Incidente-w2", "Remitente-w1", "Remitente-w2", "Persistente-w1", "Persistente-w2")) +  
  labs(title = "Boxplot Jitter por Trajetória e wave", 
       y = "Média do Tamanho (nm)", 
       x = "Trajetória e Wave") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),  # Tamanho da fonte geral
    axis.text = element_text(size = 14),  # Tamanho da fonte dos textos dos eixos
    axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5),  # Tamanho e alinhamento centralizado do título
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)  # Tamanho da fonte do título da legenda
  ) 
###### Para salvar
ggsave("plots/boxplot_com_jitter_tamanho.pdf", width = 8, height = 6)

#Boxplot Jitter da % VEs pequenas nas Trajetorias
##Todas as amostras
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = EV_pequenas_porcentagem, fill = Grupo)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Boxplot com transparência e sem outliers
  geom_jitter(aes(color = Grupo), size = 2, width = 0.2, alpha = 0.7) +  # Adiciona jitter
  scale_fill_manual(values = c("#0f8bf7", "#5aaef9", "#f7830f", "#f9a34b", "#ff12d0", "#ff5adf", "#2a8008", "#52a835"), # Paleta de cores para boxplot
                    labels = c("Controle-w1", "Controle-w2", "Incidente-w1", "Incidente-w2", "Remitente-w1", "Remitente-w2", "Persistente-w1", "Persistente-w2")) + 
  scale_color_manual(values = c("#0f8bf7", "#5aaef9", "#f7830f", "#f9a34b", "#ff12d0", "#ff5adf", "#2a8008", "#52a835"), # Paleta de cores para jitter
                     labels = c("Controle-w1", "Controle-w2", "Incidente-w1", "Incidente-w2", "Remitente-w1", "Remitente-w2", "Persistente-w1", "Persistente-w2")) +  
  labs(title = "Boxplot Jitter por Trajetória e wave", 
       y = "Porcentagem de VEs < 128.5 nm", 
       x = "Trajetória e Wave") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),  # Tamanho da fonte geral
    axis.text = element_text(size = 14),  # Tamanho da fonte dos textos dos eixos
    axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5),  # Tamanho e alinhamento centralizado do título
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)  # Tamanho da fonte do título da legenda
  ) 
###### Para salvar
ggsave("plots/boxplot_com_jitter_porcentagem.pdf", width = 8, height = 6)

#Boxplot Jitter da Concentração de partículas nas Trajetorias
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = concentracao_real, fill = Grupo)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Boxplot com transparência e sem outliers
  geom_jitter(aes(color = Grupo), size = 2, width = 0.2, alpha = 0.7) +  # Adiciona jitter
  scale_fill_manual(values = c("#0f8bf7", "#5aaef9", "#f7830f", "#f9a34b", "#ff12d0", "#ff5adf", "#2a8008", "#52a835"), # Paleta de cores para boxplot
                    labels = c("Controle-w1", "Controle-w2", "Incidente-w1", "Incidente-w2", "Remitente-w1", "Remitente-w2", "Persistente-w1", "Persistente-w2")) + 
  scale_color_manual(values = c("#0f8bf7", "#5aaef9", "#f7830f", "#f9a34b", "#ff12d0", "#ff5adf", "#2a8008", "#52a835"), # Paleta de cores para jitter
                     labels = c("Controle-w1", "Controle-w2", "Incidente-w1", "Incidente-w2", "Remitente-w1", "Remitente-w2", "Persistente-w1", "Persistente-w2")) +  
  scale_y_log10() +
  labs(title = "Boxplot Jitter por Trajetória e wave", 
       y = "Concentração (partículas/ml)", 
       x = "Trajetória e Wave") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),  # Tamanho da fonte geral
    axis.text = element_text(size = 14),  # Tamanho da fonte dos textos dos eixos
    axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5),  # Tamanho e alinhamento centralizado do título
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)  # Tamanho da fonte do título da legenda
  ) 
#Salvar
ggsave("plots/boxplot_com_jitter_concentracao.pdf", width = 8, height = 6)

###Batchs
##Tamanho
ggplot(nanosight_plus_sampleinfo, aes(x = batch_nan, y = tamanho_mean_average, fill = batch_nan)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Boxplot com transparência e sem outliers
  geom_jitter(aes(color = Trajetoria), size = 2, width = 0.2, alpha = 0.7) +  # Adiciona jitter com cores baseadas em Trajetoria
  labs(title = "Boxplot Jitter Tamanho x Batch", 
       y = "Média do Tamanho (nm)", 
       x = "Batch") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),  # Tamanho da fonte geral
    axis.text = element_text(size = 14),  # Tamanho da fonte dos textos dos eixos
    axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5),  # Tamanho e alinhamento centralizado do título
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)  # Tamanho da fonte do título da legenda
  ) +
  scale_color_manual(values = c("A" = "#0f8bf7", "B" = "#f7830f", "C" = "#ff12d0", "D" = "#2a8008"))  # Cores para as trajetórias
#Salvar
ggsave("plots/boxplot_com_jitter_batch_trajetoria.pdf", width = 8, height = 6)
##Concentração - escala logaritma p/ visualizar melhor as concentrações "pequenas"
ggplot(nanosight_plus_sampleinfo, aes(x = batch_nan, y = concentracao_real, fill = batch_nan)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Boxplot com transparência e sem outliers
  geom_jitter(aes(color = Trajetoria), size = 2, width = 0.2, alpha = 0.7) +  # Adiciona jitter com cores baseadas em Trajetoria
  labs(title = "Boxplot Jitter Concentração x Batch", 
       y = "Concentração (partículas/ml)", 
       x = "Batch") +
  theme_minimal() +
  theme(
    text = element_text(size = 16),  # Tamanho da fonte geral
    axis.text = element_text(size = 14),  # Tamanho da fonte dos textos dos eixos
    axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5),  # Tamanho e alinhamento centralizado do título
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)  # Tamanho da fonte do título da legenda
  ) +
  scale_color_manual(values = c("A" = "#0f8bf7", "B" = "#f7830f", "C" = "#ff12d0", "D" = "#2a8008")) +  # Cores para as trajetórias
  scale_y_log10()  # Aplica escala logarítmica no eixo Y
#Salvar
ggsave("plots/boxplot_com_jitter_batch_concentracao_trajetoria.pdf", width = 8, height = 6)

### Outras opções de gráficos para visualizar as médias (Welch ANOVA) ####

#Barplot com intervalos de confiança (Welch ANOVA)
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = tamanho_mean_average, fill = Grupo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", color = "black", width = 0.7) +  # Barras para médias
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = "dodge") +  # Barras de erro
  labs(title = "Barplot com Intervalos de Confiança (Welch ANOVA)",
       y = "Tamanho Médio (nm)", 
       x = "Grupo") +
  theme_minimal()
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = concentracao_real, fill = Grupo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", color = "black", width = 0.7) +  # Barras para médias
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = "dodge") +  # Barras de erro
  labs(title = "Barplot com Intervalos de Confiança (Welch ANOVA)",
       y = "Concentração (partículas/ml", 
       x = "Grupo") +
  theme_minimal()

#Violin plot (Welch ANOVA)
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = tamanho_mean_average, fill = Grupo)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Boxplot interno para mostrar a mediana
  labs(title = "Violin Plot com Boxplot Interno (Welch ANOVA)",
       y = "Tamanho Médio (nm)", 
       x = "Grupo") +
  theme_minimal()

#Médias com intervalo de confiança (Welch ANOVA)
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = tamanho_mean_average, color = Grupo)) +
  stat_summary(fun = mean, geom = "point", size = 4) +  # Plotar as médias
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +  # Barras de erro com IC
  labs(title = "Médias com Intervalos de Confiança (Welch ANOVA)",
       y = "Tamanho Médio (nm)", 
       x = "Grupo") +
  theme_minimal()

####Daqui para baixo IGNORAR

#QQPlot para visualizar a distribuição das amostras em relação a uma distribuição normal
dados_w1_sem_outliers_mean <- rnorm(nanosight_w1_sem_outliers_mean$tamanho_mean_average)
pdf("plots/qqplot_w1_sem_outliers_mean.pdf")
qqnorm(dados_w1_sem_outliers_mean)
qqline(dados_w1_sem_outliers_mean, col = "red")
dev.off()
dados_w1_sem_outliers_porcentagem <- rnorm(nanosight_w1_sem_outliers_porcentagem$EV_pequenas_porcentagem)
pdf("plots/qqplot_w1_sem_outliers_porcentagem.pdf")
qqnorm(dados_w1_sem_outliers_porcentagem)
qqline(dados_w1_sem_outliers_porcentagem, col = "red")
dev.off()
dados_w2_sem_outliers_mean <- rnorm(nanosight_w2_sem_outliers_mean$tamanho_mean_average)
pdf("plots/qqplot_w2_sem_outliers_mean.pdf")
qqnorm(dados_w2_sem_outliers_mean)
qqline(dados_w2_sem_outliers_mean, col = "red")
dev.off()
dados_w2_sem_outliers_porcentagem <- rnorm(nanosight_w2_sem_outliers_porcentagem$EV_pequenas_porcentagem)
pdf("plots/qqplot_w2_sem_outliers_porcentagem.pdf")
qqnorm(dados_w2_sem_outliers_porcentagem)
qqline(dados_w2_sem_outliers_porcentagem, col = "red")
dev.off()
dados_concentracao <- rnorm(nanosight_plus_sampleinfo$concentracao_real)
pdf("plots/qqplot_concentracao.pdf")
qqnorm(dados_concentracao)
qqline(dados_concentracao, col = "red")
dev.off()

##Histograma com curva de densidade
#Tamanho
ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +  # Histograma
  stat_function(fun = dnorm, args = list(mean = mean(nanosight_plus_sampleinfo$tamanho_mean_average), 
                                         sd = sd(nanosight_plus_sampleinfo$tamanho_mean_average)), 
                color = "red", size = 1) +  # Curva normal
  labs(title = "Histograma com Curva de Densidade Normal",
       x = "Tamanho Médio (nm)", 
       y = "Densidade") +
  theme_minimal()
#Salvar
ggsave("plots/histograma_comparativo_normalidade.pdf", width = 8, height = 6)

#Boxplot com visualização de assimetria e curtose
ggplot(nanosight_plus_sampleinfo, aes(x = "", y = tamanho_mean_average)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Boxplot da Distribuição de Tamanho Médio",
       y = "Tamanho Médio (nm)", 
       x = "") +
  theme_minimal()

#Density Plot
#Tamanho
ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average)) +
  geom_density(fill = "lightblue", alpha = 0.5) +  # Curva de densidade dos dados
  stat_function(fun = dnorm, args = list(mean = mean(nanosight_plus_sampleinfo$tamanho_mean_average), 
                                         sd = sd(nanosight_plus_sampleinfo$tamanho_mean_average)), 
                color = "red", size = 1, linetype = "dashed") +  # Curva normal
  labs(title = "Density Plot com Curva Normal",
       x = "Tamanho Médio (nm)", 
       y = "Densidade") +
  theme_minimal()
#Salvar
ggsave("plots/density_plot_tamanho_normalidade.pdf", width = 8, height = 6)
#Concentração
ggplot(nanosight_plus_sampleinfo, aes(x = concentracao_real)) +
  geom_density(fill = "lightblue", alpha = 0.5) +  # Curva de densidade dos dados
  stat_function(fun = dnorm, args = list(mean = mean(nanosight_plus_sampleinfo$concentracao_real), 
                                         sd = sd(nanosight_plus_sampleinfo$concentracao_real)), 
                color = "red", size = 1, linetype = "dashed") +  # Curva normal
  labs(title = "Density Plot com Curva Normal",
       x = "Concentração (partículas/ml)", 
       y = "Densidade") +
  theme_minimal()
#Salvar
ggsave("plots/density_plot_concentracao_normalidade.pdf", width = 8, height = 6)

#Variance Plot
variance_data <- nanosight_plus_sampleinfo %>%
  group_by(Grupo) %>%
  summarize(variance_value = var(tamanho_mean_average))
ggplot(variance_data, aes(x = Grupo, y = variance_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Adiciona barras
  labs(title = "Gráfico de Variância por Grupo", 
       y = "Variância", 
       x = "Grupo") +
  theme_minimal()

#Resíduos Padronizados vs Grupos
#tamanho
modelo <- lm(tamanho_mean_average ~ Grupo, data = nanosight_plus_sampleinfo)
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = rstandard(modelo))) +
  geom_boxplot(aes(fill = Grupo)) +  # Boxplot de resíduos padronizados
  labs(title = "Resíduos Padronizados por Grupo - Tamanho das VEs", 
       y = "Resíduos Padronizados", 
       x = "Grupo") +
  theme_minimal()
#Salvar
ggsave("plots/residuos_padronizados_por_grupo_tamanho.pdf", width = 8, height = 6)
#concentracao
modelo <- lm(concentracao_real ~ Grupo, data = nanosight_plus_sampleinfo)
ggplot(nanosight_plus_sampleinfo, aes(x = Grupo, y = rstandard(modelo))) +
  geom_boxplot(aes(fill = Grupo)) +  # Boxplot de resíduos padronizados
  labs(title = "Resíduos Padronizados por Grupo - Concentração de VEs", 
       y = "Resíduos Padronizados", 
       x = "Grupo") +
  theme_minimal()
#Salvar
ggsave("plots/residuos_padronizados_por_grupo_concentracao.pdf", width = 8, height = 6)

#Plot de densidade com desvio padrão
ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average, fill = Grupo)) +
  geom_density(alpha = 0.5) +  # Densidade por Trajetória
  labs(title = "Curvas de Densidade de Tamanho Médio por Grupo", 
       y = "Densidade", 
       x = "Tamanho Médio (nm)") +
  theme_minimal()
ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average, fill = Trajetoria)) +
  geom_density(alpha = 0.5) +  # Densidade por Trajetória
  labs(title = "Curvas de Densidade de Tamanho Médio por Trajetoria", 
       y = "Densidade", 
       x = "Tamanho Médio (nm)") +
  theme_minimal()

##Gráfico de interação
#Tamanho em relação à Trajetória~Batch
pdf("plots/interaction_plot_tamanho_traj_batch.pdf")
interaction.plot(nanosight_plus_sampleinfo$Trajetoria, 
                 nanosight_plus_sampleinfo$batch_nan, 
                 nanosight_plus_sampleinfo$tamanho_mean_average, 
                 type = "b", col = 1:4, pch = c(18, 24, 22, 20),
                 xlab = "Trajetoria", ylab = "Tamanho Médio das VEs (nm)",
                 legend = TRUE)
dev.off()
#Concentracao em relação à Trajetória~Batch
pdf("plots/interaction_plot_concentracao_traj_batch.pdf")
interaction.plot(nanosight_plus_sampleinfo$Trajetoria, 
                 nanosight_plus_sampleinfo$batch_nan, 
                 nanosight_plus_sampleinfo$concentracao_real, 
                 type = "b", col = 1:4, pch = c(18, 24, 22, 20),
                 xlab = "Trajetoria", ylab = "Concentracao das VEs (partículas/ml)",
                 legend = TRUE)
dev.off()

#############

#Mean plots
ggline(dados_ordenados_w1, x = "Trajetoria", y = "tamanho_mean_average", 
       add = c("mean_se", "jitter"), 
       order = c("A", "B", "C", "D"),
       ylab = "Média dos tamanhos das EVs na w1", xlab = "Trajetoria")
ggline(nanosight_w2_sem_outliers, x = "Trajetoria", y = "tamanho_mean_average", 
       add = c("mean_se", "jitter"), 
       order = c("A", "B", "C", "D"),
       ylab = "Média dos tamanhos das EVs na w2", xlab = "Trajetoria")

#Caracterização das amostras
##Gráfico de densidade de Kernel para idade:
ggplot(nanosight_plus_sampleinfo, aes(age)) +
  geom_density(fill = "royalblue", alpha=.4) +
  theme_bw()
ggplot(nanosight_w1, aes(age)) +
  geom_density(fill = "royalblue", alpha=.4) +
  theme_bw()
ggplot(nanosight_w2, aes(age)) +
  geom_density(fill = "royalblue", alpha=.4) +
  theme_bw()

#Análises comparativas
##Gráfico de densidade:
par(mar = c(1, 1, 1, 1) + 0.1)  # Ajusta as margens da figura
plot(density(nanosight_plus_sampleinfo$tamanho_mean_average))
plot(density(nanosight_w1_sem_outliers$tamanho_mean_average))
plot(density(nanosight_w2_sem_outliers$tamanho_mean_average))
plot(density(nanosight_w1_sem_outliers$EV_pequenas_porcentagem))
plot(density(nanosight_w2_sem_outliers$EV_pequenas_porcentagem))
##Histograma das médias dos tamanhos e sua frequencia nas Trajetorias
ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average, fill = Trajetoria)) +
  geom_histogram(color = "black", binwidth = 50)+
  facet_grid(Trajetoria ~ .) +
  labs(y = 'Frequência') +
  scale_fill_manual(values=c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = 'top',
        axis.line = element_line(colour = "black"))

ggboxplot(nanosight_plus_sampleinfo, x = "Trajetoria", y = "tamanho_mean_average",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Média do Tamanho (nm) n=187", xlab = "Trajetoria")
ggboxplot(nanosight_w1, x = "Trajetoria", y = "tamanho_mean_average",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Média do Tamanho na w1 (nm) n=92", xlab = "Trajetoria")
ggboxplot(nanosight_w2, x = "Trajetoria", y = "tamanho_mean_average",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Média do Tamanho na w2 (nm) n=95", xlab = "Trajetoria")

#Boxplot w1 e w2 % sem outliers
ggboxplot(nanosight_w1_sem_outliers, x = "Trajetoria", y = "EV_pequenas_porcentagem",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Boxplot w1 % sem outliers (n=91)", xlab = "Trajetoria")
ggboxplot(nanosight_w2_sem_outliers, x = "Trajetoria", y = "EV_pequenas_porcentagem",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Boxplot w2 % sem outliers (n=94)", xlab = "Trajetoria")

#Boxplot w1 e w2 % com outliers
ggboxplot(nanosight_w1, x = "Trajetoria", y = "EV_pequenas_porcentagem",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Boxplot w1 % (n=92)", xlab = "Trajetoria")
ggboxplot(nanosight_w2, x = "Trajetoria", y = "EV_pequenas_porcentagem",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Boxplot w2 %(n=95)", xlab = "Trajetoria")

#Gráfico Violino para a distribuição da média dos tamanhos nas Trajetorias
ggviolin(nanosight_plus_sampleinfo_sem_outliers_mean, x = "Trajetoria", y = "tamanho_mean_average", fill = "Trajetoria",
        palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
        order = c("A", "B", "C", "D"),
        add = "boxplot", add.params = list(fill = "white"),
        ylab = "Média dos tamanhos (nm)", xlab = "Trajetoria")

# Scatter Plot das médias e modas dos tamanhos
cores_personalizadas <- c("A" = "#74c476", "B" = "#fd8d3c", "C" = "#6baed6", "D" = "#ff9999")
scatter_plot_media_moda_grupos <- ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average, y = tamanho_mode_average, color = Trajetoria)) +
  geom_point(size = 2) +  # Adiciona pontos ao gráfico
  labs(
    title = "Mean and Mode Size Scatter Plot",
    x = "Mean Size (nm)",
    y = "Mode Size (nm)"
  ) +
  theme_minimal() + scale_color_manual(values = cores_personalizadas)
scatter_plot_com_regressao <- scatter_plot_media_moda_grupos +
  geom_smooth(method = "lm", se = FALSE)
print(scatter_plot_com_regressao)
ggsave("scatterplot_mean_mode.pdf", plot = scatter_plot_com_regressao + theme(text = element_text(size=20)), width = 8, height = 6)

# Scatter Plot das médias dos tamanhos e porcentagem de vesículas pequenas
cores_personalizadas <- c("A" = "#74c476", "B" = "#fd8d3c", "C" = "#6baed6", "D" = "#ff9999")
scatter_plot_media_porcentagemEV_grupos <- ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average, y = EV_pequenas_porcentagem, color = Trajetoria)) +
  geom_point(size = 2) +  # Adiciona pontos ao gráfico
  labs(
    title = "Mean Size and Small EVs Percentage Scatter Plot",
    x = "Mean Size (nm)",
    y = "Small EVs Percentage"
  ) +
  theme_minimal() + scale_color_manual(values = cores_personalizadas)
scatter_plot_com_regressao2 <- scatter_plot_media_porcentagemEV_grupos +
  geom_smooth(method = "lm", se = FALSE)
print(scatter_plot_com_regressao2)
ggsave("scatterplot_mean_percentageEV.pdf", plot = scatter_plot_com_regressao2 + theme(text = element_text(size=20)), width = 8, height = 8)

# Scatter Plot das médias dos tamanhos e porcentagem de vesículas pequenas para w1
nanosight_w1 <- subset(nanosight_plus_sampleinfo, wave == "w1")
cores_personalizadas <- c("A" = "#74c476", "B" = "#fd8d3c", "C" = "#6baed6", "D" = "#ff9999")
scatter_plot_media_porcentagemEV_grupos_w1 <- ggplot(nanosight_w1, aes(x = tamanho_mean_average, y = EV_pequenas_porcentagem, color = Trajetoria)) +
  geom_point(size = 2) +  # Adiciona pontos ao gráfico
  labs(
    title = "Mean Size and Small EVs Percentage Scatter Plot for w1",
    x = "Mean Size (nm)",
    y = "Small EVs Percentage"
  ) +
  theme_minimal() + scale_color_manual(values = cores_personalizadas)
scatter_plot_com_regressao3 <- scatter_plot_media_porcentagemEV_grupos_w1 +
  geom_smooth(method = "lm", se = FALSE)
print(scatter_plot_com_regressao3)
ggsave("scatterplot_mean_percentageEV_w1.pdf", plot = scatter_plot_com_regressao3 + theme(text = element_text(size=20)), width = 8, height = 8)

# Scatter Plot das médias dos tamanhos e porcentagem de vesículas pequenas para w2
nanosight_w2 <- subset(nanosight_plus_sampleinfo, wave == "w2")
cores_personalizadas <- c("A" = "#74c476", "B" = "#fd8d3c", "C" = "#6baed6", "D" = "#ff9999")
scatter_plot_media_porcentagemEV_grupos_w2 <- ggplot(nanosight_w2, aes(x = tamanho_mean_average, y = EV_pequenas_porcentagem, color = Trajetoria)) +
  geom_point(size = 2) +  # Adiciona pontos ao gráfico
  labs(
    title = "Mean Size and Small EVs Percentage Scatter Plot for w2",
    x = "Mean Size (nm)",
    y = "Small EVs Percentage"
  ) +
  theme_minimal() + scale_color_manual(values = cores_personalizadas)
scatter_plot_com_regressao4 <- scatter_plot_media_porcentagemEV_grupos_w2 +
  geom_smooth(method = "lm", se = FALSE)
print(scatter_plot_com_regressao4)
ggsave("scatterplot_mean_percentageEV_w2.pdf", plot = scatter_plot_com_regressao4 + theme(text = element_text(size=20)), width = 8, height = 8)

#Grafico de Barras de erro IC 95%
dados_tamanho_w1 <- data.frame(
  trajetoria = c(rep("A", 20), rep("B", 23), rep("C", 20), rep("D", 28)),
  valor = rnorm(91, mean = c(215, 255, 231, 231), sd = c(56.5, 24.0, 41.2, 38.4))
)
# Calcular médias e desvios padrão para cada grupo
summary_stats <- tapply(nanosight_w1_sem_outliers$tamanho_mean_average, nanosight_w1_sem_outliers$Trajetoria, function(x) c(Media = mean(x), DesvioPadrao = sd(x)))
# Converter o resultado em um dataframe
summary_df <- data.frame(do.call(rbind, summary_stats))
# Adicionar a coluna de grupos ao dataframe
summary_df$trajetoria <- rownames(summary_df)
# Plotar o gráfico de barras com barras de erro
ggplot(summary_df, aes(x = trajetoria, y = Media, ymin = Media - 1.95 * DesvioPadrao / sqrt(length("valor")), ymax = Media + 1.96 * DesvioPadrao / sqrt(length("valor")))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(width = 0.2, position = position_dodge(0.9)) +
  labs(x = "trajetoria", y = "tamanho", title = "Gráfico de Barra de Erro com IC = 95%") +
  theme_minimal()
nanosight_w2 <- subset(nanosight_plus_sampleinfo, wave == "w2")
cores_personalizadas <- c("A" = "#74c476", "B" = "#fd8d3c", "C" = "#6baed6", "D" = "#ff9999")
scatter_plot_media_porcentagemEV_grupos_w2 <- ggplot(nanosight_w2, aes(x = tamanho_mean_average, y = EV_pequenas_porcentagem, color = Trajetoria)) +
  geom_point(size = 2) +  # Adiciona pontos ao gráfico
  labs(
    title = "Mean Size and Small EVs Percentage Scatter Plot for w2",
    x = "Mean Size (nm)",
    y = "Small EVs Percentage"
  ) +
  theme_minimal() + scale_color_manual(values = cores_personalizadas)
scatter_plot_com_regressao4 <- scatter_plot_media_porcentagemEV_grupos_w2 +
  geom_smooth(method = "lm", se = FALSE)
print(scatter_plot_com_regressao4)
ggsave("scatterplot_mean_percentageEV_w2.pdf", plot = scatter_plot_com_regressao4 + theme(text = element_text(size=20)), width = 8, height = 8)

#Grafico de Barras de erro IC 95%
dados_tamanho_w1 <- data.frame(
  trajetoria = c(rep("A", 20), rep("B", 23), rep("C", 20), rep("D", 28)),
  valor = rnorm(91, mean = c(215, 255, 231, 231), sd = c(56.5, 24.0, 41.2, 38.4))
)
# Calcular médias e desvios padrão para cada grupo
summary_stats <- tapply(nanosight_w1_sem_outliers$tamanho_mean_average, nanosight_w1_sem_outliers$Trajetoria, function(x) c(Media = mean(x), DesvioPadrao = sd(x)))
# Converter o resultado em um dataframe
summary_df <- data.frame(do.call(rbind, summary_stats))
# Adicionar a coluna de grupos ao dataframe
summary_df$trajetoria <- rownames(summary_df)
# Plotar o gráfico de barras com barras de erro
ggplot(summary_df, aes(x = trajetoria, y = Media, ymin = Media - 1.95 * DesvioPadrao / sqrt(length("valor")), ymax = Media + 1.96 * DesvioPadrao / sqrt(length("valor")))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(width = 0.2, position = position_dodge(0.9)) +
  labs(x = "trajetoria", y = "tamanho", title = "Gráfico de Barra de Erro com IC = 95%") +
  theme_minimal()

dados_tamanho_w2 <- data.frame(
  trajetoria = c(rep("A", 20), rep("B", 22), rep("C", 20), rep("D", 32)),
  valor = rnorm(94, mean = c(239, 250, 246, 229), sd = c(35.7, 33.6, 42.2, 52.0))
)
# Calcular médias e desvios padrão para cada grupo
summary_stats <- tapply(nanosight_w2_sem_outliers$tamanho_mean_average, nanosight_w2_sem_outliers$Trajetoria, function(x) c(Media = mean(x), DesvioPadrao = sd(x)))
# Converter o resultado em um dataframe
summary_df <- data.frame(do.call(rbind, summary_stats))
# Adicionar a coluna de grupos ao dataframe
summary_df$trajetoria <- rownames(summary_df)
# Plotar o gráfico de barras com barras de erro
ggplot(summary_df, aes(x = trajetoria, y = Media, ymin = Media - 1.95 * DesvioPadrao / sqrt(length("valor")), ymax = Media + 1.96 * DesvioPadrao / sqrt(length("valor")))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(width = 0.2, position = position_dodge(0.9)) +
  labs(x = "trajetoria w2", y = "tamanho", title = "Gráfico de Barra de Erro com IC = 95%") +
  theme_minimal()




########################################################################################

###########SANKEYPLOT########################################################
# // Enter Flows between Nodes, like this:
#   //         Source [AMOUNT] Target
# 
# Control [18] Control. #74c476
# Incidence [37] Incidence. #fd8d3c
# Remission [18] Remission. #6baed6
# Persistence [32] Persistence. #ff9999
# Control [2] Outliers #78706f
# Incidence [7] Outliers #78706f
# Remission [6] Outliers #78706f
# Persistence [13] Outliers #78706f
# 
# // You can set a Node's color, like this:
# //            ...or a color for a single Flow:
# :Control #74c476
# :Control. #74c476
# :Incidence #fd8d3c
# :Incidence. #fd8d3c
# :Remission #6baed6
# :Remission. #6baed6
# :Persistence #ff9999
# :Persistence. #ff9999
# :Outliers #78706f
# 
# 
# // Use the controls below to customize
# // your diagram's appearance...
# 
# :
#   //:Budget #708090
# //            ...or a color for a single Flow:
#   //Budget [160] Other Necessities #0F0
# https://sankeymatic.com/build/
