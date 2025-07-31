# modelo de análise para investigar, tempo (W1/W2), grupo (4 categorias) e interação tempo grupo e setar os contrastes após a análise com correção. Dá para fazer isso com modelo misto também.

#Carregar pacotes
library(lme4)
library(lmerTest)   # para valores-p
library(emmeans)    # para contrastes pós-hoc
library(ggplot2)    # para visualização
library(writexl)    # para salvar em excel
library(patchwork)

###Analise post hoc com contraste por tempo e grupo

##Size
# Modelo linear misto
mod_lin_mist_size <- lmer(tamanho_mean_average ~ wave * Trajetoria + (1 | subjectid), data = nanosight_intersect)
summary(mod_lin_mist_size)
#Médias marginais por grupo em cada tempo:
emmeans(mod_lin_mist_size, ~ Trajetoria | wave)
#Médias marginais por tempo em cada grupo:
emmeans(mod_lin_mist_size, ~ wave | Trajetoria)
#Comparações dentro de cada tempo:
pairs(emmeans(mod_lin_mist_size, ~ Trajetoria | wave), adjust = "tukey")
#Comparações dentro de cada grupo:
pairs(emmeans(mod_lin_mist_size, ~ wave | Trajetoria), adjust = "tukey")
#####Para salvar em excel
# 1. Calcular médias marginais (EMMeans)
emm_group_by_wave_size <- emmeans(mod_lin_mist_size, ~ Trajetoria | wave)
emm_wave_by_group_size <- emmeans(mod_lin_mist_size, ~ wave | Trajetoria)
# 2. Comparações (contrastes) entre níveis
pairs_group_by_wave_size <- pairs(emm_group_by_wave_size, adjust = "tukey")
pairs_wave_by_group_size <- pairs(emm_wave_by_group_size, adjust = "tukey")
# 3. Converter todos os resultados para data.frames
df_emm_group_by_wave_size <- as.data.frame(emm_group_by_wave_size)
df_emm_wave_by_group_size <- as.data.frame(emm_wave_by_group_size)
df_pairs_group_by_wave_size <- as.data.frame(pairs_group_by_wave_size)
df_pairs_wave_by_group_size <- as.data.frame(pairs_wave_by_group_size)
# 4. Escrever tudo em um arquivo Excel com múltiplas abas
write_xlsx(
  list(
    "EMMeans_Trajetoria_por_Wave" = df_emm_group_by_wave_size,
    "EMMeans_Wave_por_Trajetoria" = df_emm_wave_by_group_size,
    "Contrastes_Trajetoria_por_Wave" = df_pairs_group_by_wave_size,
    "Contrastes_Wave_por_Trajetoria" = df_pairs_wave_by_group_size
  ),
  path = "resultados_emmeans_size.xlsx"
)

##concentration
mod_lin_mist_conc <- lmer(concentracao_real ~ wave * Trajetoria + (1 | subjectid), data = nanosight_intersect)
summary(mod_lin_mist_conc)
#Médias marginais por grupo em cada tempo:
emmeans(mod_lin_mist_conc, ~ Trajetoria | wave)
#Médias marginais por tempo em cada grupo:
emmeans(mod_lin_mist_conc, ~ wave | Trajetoria)
#Comparações dentro de cada tempo:
pairs(emmeans(mod_lin_mist_conc, ~ Trajetoria | wave), adjust = "tukey")
#Comparações dentro de cada grupo:
pairs(emmeans(mod_lin_mist_conc, ~ wave | Trajetoria), adjust = "tukey")
#####Para salvar em excel
# 1. Calcular médias marginais (EMMeans)
emm_group_by_wave <- emmeans(mod_lin_mist_conc, ~ Trajetoria | wave)
emm_wave_by_group <- emmeans(mod_lin_mist_conc, ~ wave | Trajetoria)
# 2. Comparações (contrastes) entre níveis
pairs_group_by_wave <- pairs(emm_group_by_wave, adjust = "tukey")
pairs_wave_by_group <- pairs(emm_wave_by_group, adjust = "tukey")
# 3. Converter todos os resultados para data.frames
df_emm_group_by_wave <- as.data.frame(emm_group_by_wave)
df_emm_wave_by_group <- as.data.frame(emm_wave_by_group)
df_pairs_group_by_wave <- as.data.frame(pairs_group_by_wave)
df_pairs_wave_by_group <- as.data.frame(pairs_wave_by_group)
# 4. Escrever tudo em um arquivo Excel com múltiplas abas
write_xlsx(
  list(
    "EMMeans_Trajetoria_por_Wave" = df_emm_group_by_wave,
    "EMMeans_Wave_por_Trajetoria" = df_emm_wave_by_group,
    "Contrastes_Trajetoria_por_Wave" = df_pairs_group_by_wave,
    "Contrastes_Wave_por_Trajetoria" = df_pairs_wave_by_group
  ),
  path = "resultados_emmeans_concentracao.xlsx"
)

##percentage
mod_lin_mist_perc <- lmer(EV_pequenas_porcentagem ~ wave * Trajetoria + (1 | subjectid), data = nanosight_intersect)
summary(mod_lin_mist_perc)
#Médias marginais por grupo em cada tempo:
emmeans(mod_lin_mist_perc, ~ Trajetoria | wave)
#Médias marginais por tempo em cada grupo:
emmeans(mod_lin_mist_perc, ~ wave | Trajetoria)
#Comparações dentro de cada tempo:
pairs(emmeans(mod_lin_mist_perc, ~ Trajetoria | wave), adjust = "tukey")
#Comparações dentro de cada grupo:
pairs(emmeans(mod_lin_mist_perc, ~ wave | Trajetoria), adjust = "tukey")
#####Para salvar em excel
# 1. Calcular médias marginais (EMMeans)
emm_group_by_wave_perc <- emmeans(mod_lin_mist_perc, ~ Trajetoria | wave)
emm_wave_by_group_perc <- emmeans(mod_lin_mist_perc, ~ wave | Trajetoria)
# 2. Comparações (contrastes) entre níveis
pairs_group_by_wave_perc <- pairs(emm_group_by_wave_perc, adjust = "tukey")
pairs_wave_by_group_perc <- pairs(emm_wave_by_group_perc, adjust = "tukey")
# 3. Converter todos os resultados para data.frames
df_emm_group_by_wave_perc <- as.data.frame(emm_group_by_wave_perc)
df_emm_wave_by_group_perc <- as.data.frame(emm_wave_by_group_perc)
df_pairs_group_by_wave_perc <- as.data.frame(pairs_group_by_wave_perc)
df_pairs_wave_by_group_perc <- as.data.frame(pairs_wave_by_group_perc)
# 4. Escrever tudo em um arquivo Excel com múltiplas abas
write_xlsx(
  list(
    "EMMeans_Trajetoria_por_Wave" = df_emm_group_by_wave_perc,
    "EMMeans_Wave_por_Trajetoria" = df_emm_wave_by_group_perc,
    "Contrastes_Trajetoria_por_Wave" = df_pairs_group_by_wave_perc,
    "Contrastes_Wave_por_Trajetoria" = df_pairs_wave_by_group_perc
  ),
  path = "resultados_emmeans_percentage.xlsx"
)

###Graficos visuais modelos lineares mistos
# Gráfico A) Size
gA <- emmip(mod_lin_mist_size, Trajetoria ~ wave, CIs = TRUE) +
  labs(title = "A) Interaction Time x Trajectory", y = "Mean Size (nm)", x = "Time") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  guides(linetype = guide_legend(title = "Trajectory:"),
         color = guide_legend(title = "Trajectory:"))
# Gráfico B) Concentration
gB <- emmip(mod_lin_mist_conc, Trajetoria ~ wave, CIs = TRUE) +
  labs(title = "B) Interaction Time x Trajectory", y = "Concentration (particles/mL)", x = "Time") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  guides(linetype = guide_legend(title = "Trajectory:"),
         color = guide_legend(title = "Trajectory:"))
# Gráfico C) Percentage
gC <- emmip(mod_lin_mist_perc, Trajetoria ~ wave, CIs = TRUE) +
  labs(title = "C) Interaction Time x Trajectory", y = "Small EVs (%)", x = "Time") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  guides(linetype = guide_legend(title = "Trajectory:"),
         color = guide_legend(title = "Trajectory:"))
# Combinar os gráficos
painel_emmip <- gA / gB / gC + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# Salvar
ggsave("painel_emmip.png", painel_emmip, width = 8, height = 14, dpi = 300)


###Graficos descritivos
##Ggplot size
ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = tamanho_mean_average, fill = wave)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7, width = 0.6) +
  geom_jitter(aes(color = wave), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
              size = 2.2, alpha = 0.8) +
  labs(title = "EV's mean size for Trajectory and Time", 
       x = "Trajectory", 
       y = "Size (nm)", 
       fill = "Time point", 
       color = "Time point") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
ggsave("size_trajectory_time_readable.pdf", width = 9, height = 6)
##Ggplot concentration
ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = concentracao_real, fill = wave)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7, width = 0.6) +
  geom_jitter(aes(color = wave), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
              size = 2.2, alpha = 0.8) +
  labs(title = "EV's concentration size for Trajectory and Time", 
       x = "Trajectory", 
       y = "Concentration (particles/mL)", 
       fill = "Time point", 
       color = "Time point") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
ggsave("concentration_trajectory_time_readable.pdf", width = 9, height = 6)
##Ggplot percentage of small EVs
ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = EV_pequenas_porcentagem, fill = wave)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7, width = 0.6) +
  geom_jitter(aes(color = wave), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
              size = 2.2, alpha = 0.8) +
  labs(title = "Small EV's percentage for Trajectory and Time", 
       x = "Trajectory", 
       y = "Percentage (%)", 
       fill = "Time point", 
       color = "Time point") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
ggsave("percentage_trajectory_time_readable.pdf", width = 9, height = 6)

###Gráficos descritivos para salvar tudo junto em um pdf
# Gráfico A - Size
plot_a <- ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = tamanho_mean_average, fill = wave)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7, width = 0.6) +
  geom_jitter(aes(color = wave), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), size = 2.2, alpha = 0.8) +
  labs(title = "A) EV's Mean Size", x = "Trajectory", y = "Size (nm)", fill = "Time point", color = "Time point") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
# Gráfico B - Concentration
plot_b <- ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = concentracao_real, fill = wave)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7, width = 0.6) +
  geom_jitter(aes(color = wave), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), size = 2.2, alpha = 0.8) +
  labs(title = "B) EV's Concentration", x = "Trajectory", y = "Concentration (particles/mL)", fill = "Time point", color = "Time point") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
# Gráfico C - Small EV %
plot_c <- ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = EV_pequenas_porcentagem, fill = wave)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7, width = 0.6) +
  geom_jitter(aes(color = wave), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), size = 2.2, alpha = 0.8) +
  labs(title = "C) Small EV's Percentage", x = "Trajectory", y = "Percentage (%)", fill = "Time point", color = "Time point") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
# Combinar todos em um painel
painel_final <- (plot_a / plot_b / plot_c) + plot_layout(guides = "collect")
# Salvar em um único arquivo PDF
ggsave("painel_EV_boxplots.pdf", painel_final, width = 11, height = 14)
# Salvar em png
ggsave("painel_EV_boxplots.png", painel_final, width = 11, height = 14, dpi = 300)

