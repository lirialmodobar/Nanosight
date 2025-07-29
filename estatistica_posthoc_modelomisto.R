# modelo de análise para investigar, tempo (W1/W2), grupo (4 categorias) e interação tempo grupo e setar os contrastes após a análise com correção. Dá para fazer isso com modelo misto também.

#Carregar pacotes
library(lme4)
library(lmerTest)   # para valores-p
library(emmeans)    # para contrastes pós-hoc
library(ggplot2)    # para visualização
library(writexl)    # para salvar em excel

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
##Grafico visual
emmip(mod_lin_mist_size, Trajetoria ~ wave, CIs = TRUE) +
  labs(title = "Interaction Time x Trajectory", y = "Mean Size", x = "Time") +
  theme_minimal()
#Salvar
ggsave("tempo-grupo-size.pdf", width = 8, height = 6)
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
##Grafico visual
emmip(mod_lin_mist_conc, Trajetoria ~ wave, CIs = TRUE) +
  labs(title = "Interaction Time x Trajectory", y = "Concentration", x = "Time") +
  theme_minimal()
#Salvar
ggsave("tempo-grupo-conc.pdf", width = 8, height = 6)
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
##Grafico visual
emmip(mod_lin_mist_perc, Trajetoria ~ wave, CIs = TRUE) +
  labs(title = "Interaction Time x Trajectory", y = "Small EV's percentage", x = "Time") +
  theme_minimal()
#Salvar
ggsave("tempo-grupo-perc.pdf", width = 8, height = 6)
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
    "EMMeans_Trajetoria_por_Wave" = df_emm_group_by_wave_size,
    "EMMeans_Wave_por_Trajetoria" = df_emm_wave_by_group_size,
    "Contrastes_Trajetoria_por_Wave" = df_pairs_group_by_wave_size,
    "Contrastes_Wave_por_Trajetoria" = df_pairs_wave_by_group_size
  ),
  path = "resultados_emmeans_percentage.xlsx"
)

###Graficos descritivos
#Ggplot size
ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = tamanho_mean_average, fill = wave)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(title = "EV's mean size for Trajectory and Time", y = "Mean size", x = "Trajectory")
#Salvar
ggsave("size_trajectory_time.pdf", width = 8, height = 6)
#Ggplot concentration
ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = concentracao_real, fill = wave)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(title = "EV's concentration for Trajectory and Time", y = "Concentration", x = "Trajectory")
#Salvar
ggsave("concentration_trajectory_time.pdf", width = 8, height = 6)
#Ggplot percentage of small EVs
ggplot(nanosight_plus_sampleinfo, aes(x = Trajetoria, y = EV_pequenas_porcentagem, fill = wave)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(title = "Small EV's percentage for Trajectory and Time", y = "Small EV's %", x = "Trajectory")
#Salvar
ggsave("percentage_trajectory_time.pdf", width = 8, height = 6)
