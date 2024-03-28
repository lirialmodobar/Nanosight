library(ggplot2)
library(ggpubr)

##Gráfico de densidade:
plot(density(nanosight_plus_sampleinfo$tamanho_mean_average))

#Histograma das médias dos tamanhos e sua frequencia nas Trajetorias
ggplot(nanosight_plus_sampleinfo, aes(x = tamanho_mean_average, fill = Trajetoria)) +
  geom_histogram(color = "black", binwidth = 50)+
  facet_grid(Trajetoria ~ .) +
  labs(y = 'Frequência') +
  scale_fill_manual(values=c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = 'top',
        axis.line = element_line(colour = "black"))

#Boxplot da distribuição da média dos tamanhos nas Trajetorias
##Todas as amostras
ggboxplot(nanosight_plus_sampleinfo, x = "Trajetoria", y = "tamanho_mean_average",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Média do Tamanho (nm) n=185", xlab = "Trajetoria")
ggboxplot(nanosight_w1, x = "Trajetoria", y = "tamanho_mean_average",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Média do Tamanho na w1 (nm) n=91", xlab = "Trajetoria")
ggboxplot(nanosight_w2, x = "Trajetoria", y = "tamanho_mean_average",
          fill = "Trajetoria", palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
          order = c("A", "B", "C", "D"),
          ylab = "Média do Tamanho na w2 (nm) n=94", xlab = "Trajetoria")

#Gráfico Violino para a distribuição da média dos tamanhos nas Trajetorias
ggviolin(nanosight_plus_sampleinfo, x = "Trajetoria", y = "tamanho_mean_average", fill = "Trajetoria",
        palette = c("#0f8bf7", "#f7830f", "#ff12d0", "#2a8008"),
        order = c("A", "B", "C", "D"),
        add = "boxplot", add.params = list(fill = "white"),
        ylab = "Média dos tamanhos (nm) n=185", xlab = "Trajetoria")

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

