library(ggplot2)

#Boxplot da Média do Tamanho
boxplot_tamanho_mean <- ggplot(nanosight_sem_outliers, aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_boxplot(fill = "#DDA0DD", color = "#800080", alpha = 0.7) +
  labs(title = "Mean Size Boxplot by Group", x = "Group", y = "Mean size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mean.pdf", plot = boxplot_tamanho_mean + theme(text = element_text(size=20)), width = 8, height = 6)

#Boxplot da Moda do Tamanho
boxplot_tamanho_mode <- ggplot(nanosight_sem_outliers, aes(x = Trajetoria, y = tamanho_mode_average)) +
  geom_boxplot(fill = "#7FFF00", color = "#006400", alpha = 0.7) +
  labs(title = "Mode Size Boxplot by Group", x = "Group", y = "Mode size (nm)") +
  theme_classic()
ggsave("boxplot_tamanho_mode.pdf", plot = boxplot_tamanho_mode + theme(text = element_text(size=20)), width = 8, height = 6)

#dataframe dos resultados do Teste de Tukey para a MODA em w1
resultados_tukey_mode_w1 <- data.frame(
  Comparacao = c("B-A", "C-A", "D-A", "C-B", "D-B", "D-C"),
  Diferenca_Media = c(-10.720395, -16.837500, -38.668750, -6.117105, -27.948355, -21.831250),
  Limite_Inferior = c(-44.20560, -56.56240, -73.07152, -39.60231, -54.90650, -56.23402),
  Limite_Superior = c(22.7648075, 22.8874027, -4.2659751, 27.3680969, -0.9902063, 12.5715249),
  Valor_P_Ajustado = c(0.8288959, 0.6737451, 0.0220048, 0.9617163, 0.0395121, 0.3402337)
)
#Gráfico de ponto p/ resultados de p ajustado do teste de Tukey em w1
plot_p_ajustado_moda_w1 <- ggplot(resultados_tukey_mode_w1, aes(x = Comparacao, y = Valor_P_Ajustado)) +
  geom_point(size = 5, color = "red") +
  labs(title = "Adjusted p-Values for Mode Differences\nAmong Groups in w1 (Tukey's Test)",
       x = "Comparisons",
       y = "Adjusted p-Values") +
  theme_light() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotação dos rótulos do eixo x
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue")  # Adicione a linha de referência 0,05
ggsave("grafico_tukey_p_ajustado_moda_w1.pdf", plot = plot_p_ajustado_moda_w1 + theme(text = element_text(size=20)), width = 8, height = 6) #salva

# Scatter Plot das médias e modas dos tamanhos
cores_personalizadas <- c("A" = "#74c476", "B" = "#fd8d3c", "C" = "#6baed6", "D" = "#ff9999")
scatter_plot_media_moda_grupos <- ggplot(nanosight_sem_outliers, aes(x = tamanho_mean_average, y = tamanho_mode_average, color = Trajetoria)) +
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

########################################################################################

#ANOVA MÉDIA TODOS
plot_anova_mean <- ggplot(teste_anova_mean, aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras das Médias por Grupo (ANOVA)",
       x = "Grupo",
       y = "Média") +
  theme_minimal()
ggsave("grafico_anova_mean.pdf", plot = plot_anova_mean, width = 8, height = 6)
#ANOVA MÉDIA w1
plot_anova_mean_w1 <- ggplot(teste_anova_mean_w1, aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras das Médias por Grupo em w1 (ANOVA)",
       x = "Grupo",
       y = "Média") +
  theme_minimal()
ggsave("grafico_anova_mean_w1.pdf", plot = plot_anova_mean_w1, width = 8, height = 6)
#ANOVA MÉDIA w2
plot_anova_mean_w2 <- ggplot(teste_anova_mean_w2, aes(x = Trajetoria, y = tamanho_mean_average)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras das Médias por Grupo em w2 (ANOVA)",
       x = "Grupo",
       y = "Média") +
  theme_minimal()
ggsave("grafico_anova_mean_w2.pdf", plot = plot_anova_mean_w2, width = 8, height = 6)
#ANOVA MODA TODOS
plot_anova_mode <- ggplot(teste_anova_mode, aes(x = Trajetoria, y = tamanho_mode_average)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras das Modas por Grupo (ANOVA)",
       x = "Grupo",
       y = "Média") +
  theme_minimal()
ggsave("grafico_anova_mode.pdf", plot = plot_anova_mode, width = 8, height = 6) #salva o grafico de barras ANOVA moda
#dataframe dos resultados do teste de tukey para a MODA
resultados_tukey_mode <- data.frame(
  Comparacao = c("B-A", "C-A", "D-A", "C-B", "D-B", "D-C"),
  Diferenca_Media = c(-3.95, -1.03, -26.36, 2.91, -22.41, -25.32),
  Limite_Inferior = c(-25.75, -26.32, -48.71, -18.88, -40.72, -47.67),
  Limite_Superior = c(17.85, 24.25, -4.01, 24.71, -4.10, -2.98),
  Valor_P_Ajustado = c(0.9648340, 0.9995617, 0.0139405, 0.9852932, 0.0098725, 0.0197299)
)
#Gráfico de ponto p/ resultados de p ajustado do teste de Tukey
plot_p_ajustado_moda <- ggplot(resultados_tukey_mode, aes(x = Comparacao, y = Valor_P_Ajustado)) +
  geom_point(size = 3, color = "red") +
  labs(title = "Valores P Ajustados da Moda (Teste de Tukey)",
       x = "Comparação",
       y = "Valor P Ajustado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotação dos rótulos do eixo x
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue")  # Adicione a linha de referência 0,05
ggsave("grafico_tukey_p_ajustado_moda.pdf", plot = plot_p_ajustado_moda, width = 8, height = 6) #salva
#Gráfico de Barras das diferenças da Moda entre os grupos
plot_diferencas_mode <- ggplot(resultados_tukey_mode, aes(x = Comparacao, y = Diferenca_Media, fill = Diferenca_Media > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Diferenças Médias entre Grupos (ANOVA)",
       x = "Comparação",
       y = "Diferença Média") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +  # Cores para diferenças positivas e negativas
  geom_text(aes(label = Diferenca_Media), vjust = -0.5, size = 3) +  # Adicione rótulos
  coord_flip()  # Gire os rótulos para facilitar a leitura
ggsave("grafico_anova_mode_diferencas.pdf", plot = plot_diferencas_mode, width = 8, height = 6) #salva o gráfico de barras das diferenças entre os grupos ANOVA
#Gráfico Tukey MODA TODOS
plot_tukey_mode <- ggplot(resultados_tukey_mode, aes(x = Comparacao, y = Diferenca_Media)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_errorbar(aes(ymin = Limite_Inferior, ymax = Limite_Superior), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Diferenças Médias entre Grupos em relação à Moda (Teste de Tukey)",
       x = "Comparação",
       y = "Diferença Média") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotação dos rótulos do eixo x
ggsave("grafico_tukey_mode.pdf", plot = plot_tukey_mode, width = 8, height = 6)
#ANOVA MODA w1
plot_anova_mode_w1 <- ggplot(teste_anova_mode_w1, aes(x = Trajetoria, y = tamanho_mode_average)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras das Modas por Grupo em w1 (ANOVA)",
       x = "Grupo",
       y = "Média") +
  theme_minimal()
#dataframe dos resultados do Teste de Tukey para a MODA em w1
resultados_tukey_mode_w1 <- data.frame(
  Comparacao = c("B-A", "C-A", "D-A", "C-B", "D-B", "D-C"),
  Diferenca_Media = c(-10.720395, -16.837500, -38.668750, -6.117105, -27.948355, -21.831250),
  Limite_Inferior = c(-44.20560, -56.56240, -73.07152, -39.60231, -54.90650, -56.23402),
  Limite_Superior = c(22.7648075, 22.8874027, -4.2659751, 27.3680969, -0.9902063, 12.5715249),
  Valor_P_Ajustado = c(0.8288959, 0.6737451, 0.0220048, 0.9617163, 0.0395121, 0.3402337)
)
#Gráfico de ponto p/ resultados de p ajustado do teste de Tukey em w1
plot_p_ajustado_moda_w1 <- ggplot(resultados_tukey_mode_w1, aes(x = Comparacao, y = Valor_P_Ajustado)) +
  geom_point(size = 3, color = "red") +
  labs(title = "Valores P Ajustados da Moda em w1 (Teste de Tukey)",
       x = "Comparação",
       y = "Valor P Ajustado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotação dos rótulos do eixo x
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue")  # Adicione a linha de referência 0,05
ggsave("grafico_tukey_p_ajustado_moda_w1.pdf", plot = plot_p_ajustado_moda_w1, width = 8, height = 6) #salva
#Gráfico de barras das diferenças da MODA entre os grupos em w1
plot_diferencas_mode_w1 <- ggplot(resultados_tukey_mode_w1, aes(x = Comparacao, y = Diferenca_Media, fill = Diferenca_Media > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Diferenças Médias entre Grupos em w1 (ANOVA)",
       x = "Comparação",
       y = "Diferença Média") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +  # Cores para diferenças positivas e negativas
  geom_text(aes(label = Diferenca_Media), vjust = -0.5, size = 3) +  # Adicione rótulos
  coord_flip()  # Gire os rótulos para facilitar a leitura
ggsave("grafico_anova_mode_diferencas_w1.pdf", plot = plot_diferencas_mode_w1, width = 8, height = 6) #salva o gráfico de barras das diferenças entre os grupos ANOVA
#Tukey MODA w1
plot_tukey_mode_w1 <- ggplot(resultados_tukey_mode_w1, aes(x = Comparacao, y = Diferenca_Media)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_errorbar(aes(ymin = Limite_Inferior, ymax = Limite_Superior), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Diferenças Médias entre Grupos em relação à Moda em w1 (Teste de Tukey)",
       x = "Comparação",
       y = "Diferença Média") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotação dos rótulos do eixo x
ggsave("grafico_tukey_mode_w1.pdf", plot = plot_tukey_mode_w1, width = 8, height = 6)
#ANOVA MODA w2
plot_anova_mode_w2 <- ggplot(teste_anova_mode_w2, aes(x = Trajetoria, y = tamanho_mode_average)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras das Modas por Grupo em w2 (ANOVA)",
       x = "Grupo",
       y = "Média") +
  theme_minimal()
ggsave("grafico_anova_mode_w2.pdf", plot = plot_anova_mode_w2, width = 8, height = 6)

#####################################################

#Gráfico de Pizza do número de amostras dos grupos:
valores <- c(controle, incidente, remitente, persistente)
legendas <- c("Controle", "Incidente", "Remitente", "Persistente")
cores <- c("gray", "pink", "#ADD8E6", "purple")
pie(valores, labels = legendas, main = "Gráfico de Pizza do Número de Amostras por Grupo", col = cores)

#Gráfico de barras empilhadas
ggplot(nanosight_sem_outliers, aes(x = Trajetoria, y = id_sample, fill = wave)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Amostras por Grupo", x = "Grupo", y = "Quantidade de Amostras") +
  scale_fill_manual(values = c("w1" = "purple", "w2" = "pink"), name = "Wave") +
  theme_minimal()

#Gráfico de densidade para o número de amostras por grupo
ggplot(numero_amostras_grupos, aes(x = 0, fill = 1)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuição da Quantidade de Amostras por Grupo", x = "Amostras") +
  theme_minimal()

########################################################

#Contando número de amostras dos grupos e waves das 133 amostras analisadas
library(stringr)
grupos_sampleinfo <- nanosight_plus_sampleinfo %>%
  group_by(Trajetoria) %>%
  summarize(Amostras = n(),)
grupos_sampleinfo_w1 <- nanosight_plus_sampleinfo %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w1")) %>%
  group_by(Trajetoria) %>%
  summarise(w1 = n())
grupos_sampleinfo_w2 <- nanosight_plus_sampleinfo %>%
  filter(Trajetoria %in% c("A", "B", "C", "D") & str_detect(wave, "w2")) %>%
  group_by(Trajetoria) %>%
  summarise(w2 = n())
grupos_sampleinfo_waves <- grupos_sampleinfo %>%
  inner_join(grupos_sampleinfo_w1, by = "Trajetoria") %>%
  inner_join(grupos_sampleinfo_w2, by = "Trajetoria")
write.csv(grupos_sampleinfo_waves, "grupos_sampleinfo_waves.csv", row.names = FALSE)
library(dplyr)
library(ggplot2)
dados <- grupos_sampleinfo_waves %>%
  rename(Total_Amostras = Amostras)
# dados em um formato adequado para o ggplot2
dados_melted <- dados %>%
  pivot_longer(cols = c(w1, w2), names_to = "Wave", values_to = "Amostras")
# gráfico de barras empilhadas
ggplot_grupos_sampleinfo_waves <- ggplot(dados_melted, aes(x = Trajetoria, y = Amostras, fill = Wave)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Samples by Group and Wave",
    x = "Group",
    y = "Number of Samples",
    fill = "Wave"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("w1" = "#CD853F", "w2" = "#F4A460"))
ggsave("ggplot_grupos_sampleinfo_waves.pdf", plot = ggplot_grupos_sampleinfo_waves) 

#######################################################################################
###tudo de novo mas só com as amostras sem os outliers

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
library(dplyr)
library(ggplot2)
dados2 <- numero_amostras_grupos_waves %>%
  rename(Total_Amostras = Amostras)
# dados em um formato adequado para o ggplot2
dados_melted2 <- dados2 %>%
  pivot_longer(cols = c(w1, w2), names_to = "Wave", values_to = "Amostras")
# gráfico de barras empilhadas
ggplot_amostras_grupos_waves <- ggplot(dados_melted2, aes(x = Trajetoria, y = Amostras, fill = Wave)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Samples by Group and Wave\n (without outliers)",
    x = "Group",
    y = "Number of Samples",
    fill = "Wave"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("w1" = "#CD853F", "w2" = "#F4A460"))
ggsave("ggplot_amostras_grupos_waves.pdf", plot = ggplot_amostras_grupos_waves) 
