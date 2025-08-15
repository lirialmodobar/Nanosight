#Residuos dos testes de modelos

# Certifique-se de que os pacotes necessários estão instalados e carregados
pacman::p_load(dplyr, ggplot2, patchwork, broom.mixed, lme4, glmmTMB, readr, car, emmeans, performance)

# ---
# 1. Ajuste os modelos (conforme o seu código)
# ---

# Modelo 1: Poisson com Efeito Aleatório
modelo_poisson_conc <- glmmTMB(concentracao_real ~ wave + Trajetoria + (1 | subjectid),
                               data = nanosight_intersect,
                               family = poisson(link = "log"))

# Modelo 2: Binomial Negativa tipo 1 com Efeito Aleatório
modelo_nbinom1_conc_random <- glmmTMB(concentracao_real ~ wave + Trajetoria + (1 | subjectid),
                                      data = nanosight_intersect,
                                      family = nbinom1(link = "log"))

# Modelo 3: Poisson sem Efeito Aleatório
modelo_poisson_sem_random <- glmmTMB(concentracao_real ~ wave + Trajetoria,
                                     data = nanosight_intersect,
                                     family = poisson(link = "log"))

# Modelo 4: Binomial Negativa tipo 1 sem Efeito Aleatório
modelo_nbinom1_conc_sem_random <- glmmTMB(concentracao_real ~ wave + Trajetoria,
                                          data = nanosight_intersect,
                                          family = nbinom1(link = "log"))

#Modelo 5: Binominal Negativa tipo 2 sem Efeito Aleatório
modelo_nbinom2_conc_sem_random <- glmmTMB(concentracao_real ~ wave + Trajetoria,
                                          data = nanosight_intersect,
                                          family = nbinom2(link = "log"))


# ---
# 2. Reutilize sua função de análise de resíduos
# ---

analisar_residuos_glmm <- function(modelo, titulo) {
  
  # A função `augment` do broom.mixed já calcula os resíduos de forma apropriada para GLMMs
  diagn <- broom.mixed::augment(modelo, data = model.frame(modelo), type.predict = "response", type.resid = "pearson")
  
  # Gráficos:
  p1 <- ggplot(diagn, aes(sample = .resid)) +
    geom_qq_line(color = "red") +
    geom_qq(size = 1, color = "black") +
    labs(y = "Resíduos de Pearson", x = "Distribuição Teórica", title = paste("QQ Plot -", titulo)) +
    theme_minimal()
  
  p2 <- ggplot(diagn, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Valores Ajustados", y = "Resíduos de Pearson", title = paste("Resíduos vs Ajustados -", titulo)) +
    theme_minimal()
  
  p3 <- ggplot(diagn, aes(x = .fitted, y = sqrt(abs(.resid)))) +
    geom_point(alpha = 0.6) +
    geom_smooth(se = FALSE, color = "red", linewidth = 0.4) +
    labs(x = "Valores Ajustados", y = "sqrt(|Resíduos de Pearson|)", title = paste("Escala-Localização -", titulo)) +
    theme_minimal()
  
  # Combinar plots
  (p1 + p2) / p3
}

# ---
# 3. Gerar os gráficos para cada modelo e salvá-los
# ---

# Modelo 1
residuos_conc_poisson_random <- analisar_residuos_glmm(modelo_poisson_conc, "Poisson com Efeito Aleatório")
ggsave("residuos_conc_poisson_random.png", residuos_conc_poisson_random, width = 12, height = 8, dpi = 300)

# Modelo 2
residuos_conc_nbinom1_random <- analisar_residuos_glmm(modelo_nbinom1_conc_random, "Nbinom1 com Efeito Aleatório")
ggsave("residuos_conc_nbinom1_random.png", residuos_conc_nbinom1_random, width = 12, height = 8, dpi = 300)

# Modelo 3
residuos_conc_poisson_sem_random <- analisar_residuos_glmm(modelo_poisson_sem_random, "Poisson sem Efeito Aleatório")
ggsave("residuos_conc_poisson_sem_random.png", residuos_conc_poisson_sem_random, width = 12, height = 8, dpi = 300)

# Modelo 4
residuos_conc_nbinom1_sem_random <- analisar_residuos_glmm(modelo_nbinom1_conc_sem_random, "Nbinom1 sem Efeito Aleatório")
ggsave("residuos_conc_nbinom1_sem_random.png", residuos_conc_nbinom1_sem_random, width = 12, height = 8, dpi = 300)

# Modelo 5
residuos_conc_nbinom2_sem_random <- analisar_residuos_glmm(modelo_nbinom2_conc_sem_random, "Nbinom2 sem Efeito Aleatório")
ggsave("residuos_conc_nbinom2_sem_random.png", residuos_conc_nbinom2_sem_random, width = 12, height = 8, dpi = 300)
