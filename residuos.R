# Carregamento de pacotes
pacman::p_load(dplyr, ggplot2, patchwork, broom.mixed, lmerTest, readr)

# Importar dados
df <- read_csv("nanosight_intersect.csv")  # ajuste o caminho

# Ajustar modelos lineares mistos
mod_size <- lmer(tamanho_mean_average ~ wave * Trajetoria + (1 | batch_nan), data = df)
mod_conc <- lmer(concentracao_real ~ wave * Trajetoria + (1 | batch_nan), data = df)
mod_conc_sem_interacao <- lmer(concentracao_real ~ wave + Trajetoria + (1 | batch_nan), data = df)
mod_conc_sem_interacao_sem_random <- lm(concentracao_real ~ wave + Trajetoria, data = df)
mod_perc <- lmer(EV_pequenas_porcentagem ~ wave * Trajetoria + (1 | batch_nan), data = df)

#Função para analise de residuos
analisar_residuos <- function(modelo, titulo) {
  diagn <- augment(modelo)
  
  # Calcular valores ajustados e resíduos
  diagn$.fitted <- predict(modelo, type = "response")
  diagn$.resid <- resid(modelo)
  
  # Calcular resíduos padronizados manualmente
  sigma_resid <- sigma(modelo)  # desvio padrão dos resíduos
  diagn$.std.resid <- diagn$.resid / sigma_resid
  
  # Gráficos:
  p1 <- ggplot(diagn, aes(sample = .resid)) +
    geom_qq_line(color = "red") +
    geom_qq(size = 1, color = "black") +
    labs(y = "Resíduos", x = "Distribuição Teórica", title = paste("QQ Plot -", titulo)) +
    theme_minimal()
  
  p2 <- ggplot(diagn, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Valores Ajustados", y = "Resíduos", title = paste("Resíduos vs Ajustados -", titulo)) +
    theme_minimal()
  
  p3 <- ggplot(diagn, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
    geom_point(alpha = 0.6) +
    geom_smooth(se = FALSE, color = "red", linewidth = 0.4) +
    labs(x = "Valores Ajustados", y = "sqrt(|Resíduos Padronizados|)", title = paste("Escala-Localização -", titulo)) +
    theme_minimal()
  
  # Cook's distance
  cooks <- cooks.distance(modelo)
  p4 <- ggplot(data.frame(obs = seq_along(cooks), cooks = cooks),
               aes(x = obs, y = cooks)) +
    geom_bar(stat = "identity", fill = "black") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    labs(x = "Índice da Observação", y = "Cook's Distance", title = paste("Distância de Cook -", titulo)) +
    theme_minimal()
  
  # Combinar plots
  (p1 + p2) / (p3 + p4)
}

# Gerar gráficos para cada variável
residuos_size <- analisar_residuos(mod_size, "Size")
residuos_conc <- analisar_residuos(mod_conc, "Concentration")
residuos_conc_sem_interacao <- analisar_residuos(mod_conc_sem_interacao, "Concentration with no interaction")
residuos_conc_sem_interacao_sem_random <- analisar_residuos(mod_conc_sem_interacao_sem_random, "Concentration with no interaction and no random")
residuos_perc <- analisar_residuos(mod_perc, "Percentage")

# Salvar cada painel
ggsave("residuos_size.png", residuos_size, width = 12, height = 8, dpi = 300)
ggsave("residuos_conc.png", residuos_conc, width = 12, height = 8, dpi = 300)
ggsave("residuos_conc_sem_interacao.png", residuos_conc_sem_interacao, width = 12, height = 8, dpi = 300)
ggsave("residuos_conc_sem_interacao_sem_random.png", residuos_conc_sem_interacao_sem_random, width = 12, height = 8, dpi = 300)
ggsave("residuos_perc.png", residuos_perc, width = 12, height = 8, dpi = 300)
