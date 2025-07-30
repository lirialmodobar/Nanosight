#Testes de normalidade (premissa para o ANOVA)
shapiro_teste_tamanho <- shapiro.test(nanosight_intersect$tamanho_mean_average) #0.0006688885
shapiro_teste_concentracao <- shapiro.test(nanosight_intersect$concentracao_real) #2.133966e-12
shapiro_teste_porcentagem <- shapiro.test(nanosight_intersect$EV_pequenas_porcentagem) #8.308021e-14
shapiro_teste_bage <- shapiro.test(nanosight_intersect$bage) #0.04943

#Teste U de Mann-Whitney (ou teste de Wilcoxon-Mann-Whitney) para variaveis continuas em 2 grupos independentes sem distribuiçao normal
wilcox.test(tamanho_mean_average ~ sex, data = nanosight_intersect) #p-value = 0.2395
wilcox.test(concentracao_real ~ sex, data = nanosight_intersect) #p-value = 0.1273
wilcox.test(EV_pequenas_porcentagem ~ sex, data = nanosight_intersect) #p-value = 0.9852
wilcox.test(tamanho_mean_average ~ site, data = nanosight_intersect) #p-value = 0.9031
wilcox.test(concentracao_real ~ site, data = nanosight_intersect) #p-value = 0.1241
wilcox.test(EV_pequenas_porcentagem ~ site, data = nanosight_intersect) #p-value = 0.493

#Teste de Spearman (para variaveis continuas com dados não normais)
cor.test(nanosight_intersect$bage, nanosight_intersect$tamanho_mean_average, method = "spearman")
cor.test(nanosight_intersect$bage, nanosight_intersect$concentracao_real, method = "spearman")
cor.test(nanosight_intersect$bage, nanosight_intersect$EV_pequenas_porcentagem, method = "spearman")

#Teste de Kruskal-Wallis para variaveis continuas e 3 ou mais grupos (idade) com distribuiçao fora da normalidade
nanosight_intersect$bage_floor <- floor(nanosight_intersect$bage)
kruskal.test(tamanho_mean_average ~ bage_floor, data = nanosight_intersect) #p-value = 0.7654
kruskal.test(concentracao_real ~ bage_floor, data = nanosight_intersect) #p-value = 0.2909
kruskal.test(EV_pequenas_porcentagem ~ bage_floor, data = nanosight_intersect) #p-value = 0.9653

#Grafico
library(ggplot2)
library(patchwork)

##Sexo
#transformando a variavel sex em categorica
nanosight_plus_sampleinfo$sex <- factor(nanosight_plus_sampleinfo$sex, 
                     levels = c(1, 2), 
                     labels = c("M", "F"))
# Sexo
g1 <- ggplot(nanosight_plus_sampleinfo, aes(x = sex, y = tamanho_mean_average, fill = sex)) +
  geom_boxplot() +
  labs(title = "Size vs Sex", x = "Sex", y = "Size") +
  theme_minimal()
g2 <- ggplot(nanosight_plus_sampleinfo, aes(x = sex, y = concentracao_real, fill = sex)) +
  geom_boxplot() +
  labs(title = "Concentration vs Sex", x = "Sex", y = "Concentration") +
  theme_minimal()
g3 <- ggplot(nanosight_plus_sampleinfo, aes(x = sex, y = EV_pequenas_porcentagem, fill = sex)) +
  geom_boxplot() +
  labs(title = "Small EVs % vs Sex", x = "Sex", y = "%") +
  theme_minimal()
# Site
g4 <- ggplot(nanosight_plus_sampleinfo, aes(x = site, y = tamanho_mean_average, fill = site)) +
  geom_boxplot() +
  labs(title = "Size vs Site", x = "Site", y = "Size") +
  theme_minimal()
g5 <- ggplot(nanosight_plus_sampleinfo, aes(x = site, y = concentracao_real, fill = site)) +
  geom_boxplot() +
  labs(title = "Concentration vs Site", x = "Site", y = "Concentration") +
  theme_minimal()
g6 <- ggplot(nanosight_plus_sampleinfo, aes(x = site, y = EV_pequenas_porcentagem, fill = site)) +
  geom_boxplot() +
  labs(title = "Small EVs % vs Site", x = "Site", y = "%") +
  theme_minimal()
# Idade
g7 <- ggplot(nanosight_plus_sampleinfo, aes(x = bage, y = tamanho_mean_average)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Size vs Age", x = "Age", y = "Size") +
  theme_minimal()
g8 <- ggplot(nanosight_plus_sampleinfo, aes(x = bage, y = concentracao_real)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Concentration vs Age", x = "Age", y = "Concentration") +
  theme_minimal()
g9 <- ggplot(nanosight_plus_sampleinfo, aes(x = bage, y = EV_pequenas_porcentagem)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Small EVs % vs Age", x = "Age", y = "%") +
  theme_minimal()
# Linha 1: sexo | Linha 2: site | Linha 3: idade
painel_completo <- (g1 | g2 | g3) / (g4 | g5 | g6) / (g7 | g8 | g9)
# Exibir no RStudio
painel_completo
ggsave("painel_vesiculas.pdf", painel_completo, width = 14, height = 12)


####Salvando tabelas editaveis
# Carregue os pacotes
library(writexl)
library(dplyr)

# Função para IC95%
ic95 <- function(x) {
  m <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(length(x))
  ci <- qt(0.975, df = length(x) - 1) * se
  c(IC_lower = m - ci, IC_upper = m + ci)
}

# ======= Estatísticas descritivas por SEXO =======
descr_sex <- nanosight_intersect %>%
  group_by(sex) %>%
  summarise(
    media_tamanho = mean(tamanho_mean_average, na.rm = TRUE),
    mediana_tamanho = median(tamanho_mean_average, na.rm = TRUE),
    sd_tamanho = sd(tamanho_mean_average, na.rm = TRUE),
    IC95_tamanho_lower = ic95(tamanho_mean_average)[1],
    IC95_tamanho_upper = ic95(tamanho_mean_average)[2],
    
    media_conc = mean(concentracao_real, na.rm = TRUE),
    mediana_conc = median(concentracao_real, na.rm = TRUE),
    sd_conc = sd(concentracao_real, na.rm = TRUE),
    IC95_conc_lower = ic95(concentracao_real)[1],
    IC95_conc_upper = ic95(concentracao_real)[2],
    
    media_ev = mean(EV_pequenas_porcentagem, na.rm = TRUE),
    mediana_ev = median(EV_pequenas_porcentagem, na.rm = TRUE),
    sd_ev = sd(EV_pequenas_porcentagem, na.rm = TRUE),
    IC95_ev_lower = ic95(EV_pequenas_porcentagem)[1],
    IC95_ev_upper = ic95(EV_pequenas_porcentagem)[2]
  )

# ======= Estatísticas descritivas por SITE =======
descr_site <- nanosight_intersect %>%
  group_by(site) %>%
  summarise(
    media_tamanho = mean(tamanho_mean_average, na.rm = TRUE),
    mediana_tamanho = median(tamanho_mean_average, na.rm = TRUE),
    sd_tamanho = sd(tamanho_mean_average, na.rm = TRUE),
    IC95_tamanho_lower = ic95(tamanho_mean_average)[1],
    IC95_tamanho_upper = ic95(tamanho_mean_average)[2],
    
    media_conc = mean(concentracao_real, na.rm = TRUE),
    mediana_conc = median(concentracao_real, na.rm = TRUE),
    sd_conc = sd(concentracao_real, na.rm = TRUE),
    IC95_conc_lower = ic95(concentracao_real)[1],
    IC95_conc_upper = ic95(concentracao_real)[2],
    
    media_ev = mean(EV_pequenas_porcentagem, na.rm = TRUE),
    mediana_ev = median(EV_pequenas_porcentagem, na.rm = TRUE),
    sd_ev = sd(EV_pequenas_porcentagem, na.rm = TRUE),
    IC95_ev_lower = ic95(EV_pequenas_porcentagem)[1],
    IC95_ev_upper = ic95(EV_pequenas_porcentagem)[2]
  )

# ======= Estatísticas por idade (bage_floor) =======
nanosight_intersect$bage_floor <- floor(nanosight_intersect$bage)
descr_idade <- nanosight_intersect %>%
  group_by(bage_floor) %>%
  summarise(
    media_tamanho = mean(tamanho_mean_average, na.rm = TRUE),
    mediana_tamanho = median(tamanho_mean_average, na.rm = TRUE),
    sd_tamanho = sd(tamanho_mean_average, na.rm = TRUE),
    
    media_conc = mean(concentracao_real, na.rm = TRUE),
    mediana_conc = median(concentracao_real, na.rm = TRUE),
    sd_conc = sd(concentracao_real, na.rm = TRUE),
    
    media_ev = mean(EV_pequenas_porcentagem, na.rm = TRUE),
    mediana_ev = median(EV_pequenas_porcentagem, na.rm = TRUE),
    sd_ev = sd(EV_pequenas_porcentagem, na.rm = TRUE)
  )

# ======= Testes estatísticos =======

# Shapiro-Wilk (já conhecido)
shapiro_results <- data.frame(
  Variável = c("Size", "Concentration", "Small EV's percentage"),
  Chi_square = c(
    shapiro.test(nanosight_intersect$tamanho_mean_average)$statistic,
    shapiro.test(nanosight_intersect$concentracao_real)$statistic,
    shapiro.test(nanosight_intersect$EV_pequenas_porcentagem)$statistic
  ),
  p_value = c(
    shapiro.test(nanosight_intersect$tamanho_mean_average)$p.value,
    shapiro.test(nanosight_intersect$concentracao_real)$p.value,
    shapiro.test(nanosight_intersect$EV_pequenas_porcentagem)$p.value
  )
)
shapiro_results$p_value <- format(shapiro_results$p_value, scientific=FALSE)
shapiro_results$Interpretation <- ifelse(shapiro_results$p_value > 0.05, "normal distribution", "not normal distribution")
shapiro_results$p_value <- as.numeric(shapiro_results$p_value, scientific=TRUE)

# Wilcoxon (sexo e site)
wilcox_results <- data.frame(
  Comparison = rep(c("Size x Sex", "Concentration x Sex", "Small EV's Percentage x Sex", "Size x Site", "Concentration x Site", "Small EV's Percentage x Site")),
  chi_square = c(
    wilcox.test(tamanho_mean_average ~ sex, data = nanosight_intersect)$statistic,
    wilcox.test(concentracao_real ~ sex, data = nanosight_intersect)$statistic,
    wilcox.test(EV_pequenas_porcentagem ~ sex, data = nanosight_intersect)$statistic,
    wilcox.test(tamanho_mean_average ~ site, data = nanosight_intersect)$statistic,
    wilcox.test(concentracao_real ~ site, data = nanosight_intersect)$statistic,
    wilcox.test(EV_pequenas_porcentagem ~ site, data = nanosight_intersect)$statistic
  ),
  p_value = c(
    wilcox.test(tamanho_mean_average ~ sex, data = nanosight_intersect)$p.value,
    wilcox.test(concentracao_real ~ sex, data = nanosight_intersect)$p.value,
    wilcox.test(EV_pequenas_porcentagem ~ sex, data = nanosight_intersect)$p.value,
    wilcox.test(tamanho_mean_average ~ site, data = nanosight_intersect)$p.value,
    wilcox.test(concentracao_real ~ site, data = nanosight_intersect)$p.value,
    wilcox.test(EV_pequenas_porcentagem ~ site, data = nanosight_intersect)$p.value
  )
)
wilcox_results$Interpretation <- case_when(
  wilcox_results$p_value > +0.05 ~ "no statistical difference",
  wilcox_results$p_value < +0.05 & wilcox_results$p_value == +0.05 ~ "presence of statistical difference"
)

#Spearman (idade)
spearman_results <- data.frame(
  Comparison = c("Age x Size", "Age x Concentration", "Age x Small EV's Percentage"),
  p_value = c(
    cor.test(nanosight_intersect$bage, nanosight_intersect$tamanho_mean_average, method = "spearman")$p.value,
    cor.test(nanosight_intersect$bage, nanosight_intersect$concentracao_real, method = "spearman")$p.value,
    cor.test(nanosight_intersect$bage, nanosight_intersect$EV_pequenas_porcentagem, method = "spearman")$p.value
  ),
  rho = c(
    cor.test(nanosight_intersect$bage, nanosight_intersect$tamanho_mean_average, method = "spearman")$estimate,
    cor.test(nanosight_intersect$bage, nanosight_intersect$concentracao_real, method = "spearman")$estimate,
    cor.test(nanosight_intersect$bage, nanosight_intersect$EV_pequenas_porcentagem, method = "spearman")$estimate
  )
)
spearman_results$Interpretation <- case_when(
  rho < -0.3 & rho == -0.3 ~ "negative correlation", 
  rho > +0.3 & rho == +0.3 ~ "positive correlation",
  rho > -0.3 & rho < +0.3 ~ "no correlation"
  )


# ======= Exportar tudo em Excel =======
write_xlsx(
  list(
    "Normalidade_Shapiro" = shapiro_results,
    "Wilcoxon" = wilcox_results,
    "Spearman" = spearman_results,
    "Descritivas_por_Sexo" = descr_sex,
    "Descritivas_por_Site" = descr_site,
    "Descritivas_por_Idade" = descr_idade
  ),
  path = "analise_completa_nanosight.xlsx"
)

