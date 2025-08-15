#Modelos Lineares Mistos Generalizados (GLMMs)

# Carregamento de pacotes
pacman::p_load(dplyr, ggplot2, patchwork, broom.mixed, lme4, glmmTMB, readr, car, emmeans)

# Ajustar os modelos GLMMs que você usou
modelo_gamma_bobyqa_size <- glmer(tamanho_mean_average ~ wave * Trajetoria + (1 | subjectid),
                                  data = nanosight_intersect,
                                  family = Gamma(link = "log"),
                                  control = glmerControl(optimizer = "bobyqa"))

modelo_nbinom1_conc <- glmmTMB(concentracao_real ~ wave + Trajetoria,
                               data = nanosight_intersect,
                               family = nbinom1(link = "log"))

modelo_perc_beta <- glmmTMB(percentage_prop ~ wave * Trajetoria + (1 | subjectid),
                            data = nanosight_intersect,
                            family = beta_family(link = "logit"))

# Função para análise de resíduos em GLMMs
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
  
  # A distância de Cook não é padrão para GLMMs, mas podemos calcular usando o pacote `influence.ME`
  # No entanto, para fins de diagnóstico rápido, a verificação de outliers nos resíduos é mais comum.
  # Vamos usar o gráfico de resíduos versus valores ajustados, que é um bom indicador de problemas.
  
  # O gráfico de escala-localização também é importante, mas o gráfico de resíduos vs ajustados
  # já é um bom indicativo de heterocedasticidade.
  
  p3 <- ggplot(diagn, aes(x = .fitted, y = sqrt(abs(.resid)))) +
    geom_point(alpha = 0.6) +
    geom_smooth(se = FALSE, color = "red", linewidth = 0.4) +
    labs(x = "Valores Ajustados", y = "sqrt(|Resíduos de Pearson|)", title = paste("Escala-Localização -", titulo)) +
    theme_minimal()
  
  # A distância de Cook pode ser complexa em GLMMs.
  # Para uma análise mais completa, o pacote `influence.ME` é recomendado.
  # Para essa função, vamos manter os gráficos mais essenciais.
  
  # Combinar plots
  (p1 + p2) / p3
}

# Gerar gráficos para cada variável
residuos_size <- analisar_residuos_glmm(modelo_gamma_bobyqa_size_identity, "Tamanho")
residuos_conc <- analisar_residuos_glmm(modelo_nbinom1_conc, "Concentração")
residuos_perc <- analisar_residuos_glmm(modelo_perc_beta, "Porcentagem")

# Salvar cada painel
ggsave("residuos_size_glmm.png", residuos_size, width = 12, height = 8, dpi = 300)
ggsave("residuos_conc_glmm.png", residuos_conc, width = 12, height = 8, dpi = 300)
ggsave("residuos_perc_glmm.png", residuos_perc, width = 12, height = 8, dpi = 300)
