#Testes dos modelos
# 1) poisson com fator aleatorio no id
# 2) bin neg com fator aleatorio no id
# 3) poisson sem fator aleatorio no id
# 4) bin neg sem fator aleatorio no id 
library(DHARMa)

# 1) Modelo de Concentração com Distribuição Poisson e Efeito Aleatório
# Ajusta o modelo GLMM com a família Poisson e o termo aleatório (1 | subjectid)
modelo_poisson_conc <- glmmTMB(concentracao_real ~ wave + Trajetoria + (1 | subjectid),
                               data = nanosight_intersect,
                               family = poisson(link = "log"))
# Avaliação do Modelo Poisson
# Tabela principal com o teste Wald Chi-Square
cat("Análise de Variância (Anova) para o Modelo Poisson:\n")
Anova(modelo_poisson_conc, type = "II")
# Obter os contrastes para comparações pareadas
cat("\nComparação de médias (emmeans) para o Modelo Poisson:\n")
emmeans(modelo_poisson_conc, specs = pairwise ~ Trajetoria | wave, type = "response")
# Cálculo do ICC, AIC e BIC
# ICC (Coeficiente de Correlação Intraclasse)
icc_poisson <- performance::icc(modelo_poisson_conc)
cat("\nCoeficiente de Correlação Intraclasse (ICC):\n")
summary(icc_poisson)
# AIC (Critério de Informação de Akaike)
aic_poisson <- AIC(modelo_poisson_conc)
cat("\nAIC do Modelo Poisson:\n")
print(aic_poisson)
# BIC (Critério de Informação Bayesiano)
bic_poisson <- BIC(modelo_poisson_conc)
cat("\nBIC do Modelo Poisson:\n")
print(bic_poisson)

# 2) Modelo de Concentração com Distribuição Binomial Negativa tipo 1 (nbinom1) e Efeito Aleatório (1 | subjectid)
## Ajusta o modelo GLMM com a família nbinom1 e o termo aleatório para 'subjectid'
modelo_nbinom1_conc_random <- glmmTMB(concentracao_real ~ wave + Trajetoria + (1 | subjectid),
                                      data = nanosight_intersect,
                                      family = nbinom1(link = "log"))
# Avaliação do Modelo
# Tabela principal com o teste Wald Chi-Square
cat("Análise de Variância (Anova) para o Modelo nbinom1 com Efeito Aleatório:\n")
Anova(modelo_nbinom1_conc_random, type = "II")
# Obter os contrastes para comparações pareadas
cat("\nComparação de médias (emmeans) para o Modelo nbinom1 com Efeito Aleatório:\n")
emmeans(modelo_nbinom1_conc_random, specs = pairwise ~ Trajetoria | wave, type = "response")
# Cálculo de ICC, AIC e BIC
# ICC (Coeficiente de Correlação Intraclasse)
icc_nbinom1_random <- performance::icc(modelo_nbinom1_conc_random)
cat("\nCoeficiente de Correlação Intraclasse (ICC):\n")
summary(icc_nbinom1_random)
# AIC (Critério de Informação de Akaike)
aic_nbinom1_random <- AIC(modelo_nbinom1_conc_random)
cat("\nAIC do Modelo nbinom1 com Efeito Aleatório:\n")
print(aic_nbinom1_random)
# BIC (Critério de Informação Bayesiano)
bic_nbinom1_random <- BIC(modelo_nbinom1_conc_random)
cat("\nBIC do Modelo nbinom1 com Efeito Aleatório:\n")
print(bic_nbinom1_random)

# 2) Modelo de Concentração com Distribuição Binomial Negativa tipo 2 (nbinom2) e Efeito Aleatório (1 | subjectid)
## Ajusta o modelo GLMM com a família nbinom2 e o termo aleatório para 'subjectid'
modelo_nbinom2_conc_random <- glmmTMB(concentracao_real ~ wave + Trajetoria + (1 | subjectid),
                                      data = nanosight_intersect,
                                      family = nbinom2(link = "log"))
# Avaliação do Modelo
# Tabela principal com o teste Wald Chi-Square
cat("Análise de Variância (Anova) para o Modelo nbinom2 com Efeito Aleatório:\n")
Anova(modelo_nbinom2_conc_random, type = "II")
# Obter os contrastes para comparações pareadas
cat("\nComparação de médias (emmeans) para o Modelo nbinom1 com Efeito Aleatório:\n")
emmeans(modelo_nbinom2_conc_random, specs = pairwise ~ Trajetoria | wave, type = "response")
# Cálculo de ICC, AIC e BIC
# ICC (Coeficiente de Correlação Intraclasse)
icc_nbinom2_random <- performance::icc(modelo_nbinom2_conc_random)
cat("\nCoeficiente de Correlação Intraclasse (ICC):\n")
summary(icc_nbinom2_random)
# AIC (Critério de Informação de Akaike)
aic_nbinom2_random <- AIC(modelo_nbinom2_conc_random)
cat("\nAIC do Modelo nbinom2 com Efeito Aleatório:\n")
print(aic_nbinom2_random)
# BIC (Critério de Informação Bayesiano)
bic_nbinom2_random <- BIC(modelo_nbinom2_conc_random)
cat("\nBIC do Modelo nbinom2 com Efeito Aleatório:\n")
print(bic_nbinom2_random)


# 3) Modelo de Concentração com Distribuição Poisson (sem efeito aleatório)
# Ajusta o modelo GLM com a família Poisson e sem o termo aleatório (1 | subjectid)
modelo_poisson_sem_random <- glmmTMB(concentracao_real ~ wave + Trajetoria,
                                     data = nanosight_intersect,
                                     family = poisson(link = "log"))
# Avaliação do Modelo Poisson
# Tabela principal com o teste Wald Chi-Square
cat("Análise de Variância (Anova) para o Modelo Poisson sem Efeito Aleatório:\n")
Anova(modelo_poisson_sem_random, type = "II")
# Obter os contrastes para comparações pareadas
cat("\nComparação de médias (emmeans) para o Modelo Poisson sem Efeito Aleatório:\n")
emmeans(modelo_poisson_sem_random, specs = pairwise ~ Trajetoria | wave, type = "response")
# Cálculo do AIC e BIC e ICC
#ICC
icc_poisson_sem_random <- performance::icc(modelo_poisson_sem_random)
cat("\nCoeficiente de Correlação Intraclasse (ICC):\n")
summary(icc_poisson_sem_random)
# AIC (Critério de Informação de Akaike)
aic_poisson_sem_random <- AIC(modelo_poisson_sem_random)
cat("\nAIC do Modelo Poisson sem Efeito Aleatório:\n")
print(aic_poisson_sem_random)
# BIC (Critério de Informação Bayesiano)
bic_poisson_sem_random <- BIC(modelo_poisson_sem_random)
cat("\nBIC do Modelo Poisson sem Efeito Aleatório:\n")
print(bic_poisson_sem_random)

# 4) Modelo de Concentração com Distribuição Binomial Negativa tipo 1 (nbinom1) sem Efeito Aleatório (1 | subjectid)
## Ajusta o modelo GLMM com a família nbinom1 e o termo aleatório para 'subjectid'
modelo_nbinom1_conc_sem_random <- glmmTMB(concentracao_real ~ wave + Trajetoria,
                                      data = nanosight_intersect,
                                      family = nbinom1(link = "log"))
# Avaliação do Modelo
# Tabela principal com o teste Wald Chi-Square
cat("Análise de Variância (Anova) para o Modelo nbinom1 sem Efeito Aleatório:\n")
Anova(modelo_nbinom1_conc_sem_random, type = "II")
# Obter os contrastes para comparações pareadas
cat("\nComparação de médias (emmeans) para o Modelo nbinom1 sem Efeito Aleatório:\n")
emmeans(modelo_nbinom1_conc_sem_random, specs = pairwise ~ Trajetoria | wave, type = "response")
# Cálculo de ICC, AIC e BIC
# ICC (Coeficiente de Correlação Intraclasse)
icc_nbinom1_sem_random <- performance::icc(modelo_nbinom1_conc_sem_random)
cat("\nCoeficiente de Correlação Intraclasse (ICC):\n")
summary(icc_nbinom1_sem_random)
# AIC (Critério de Informação de Akaike)
aic_nbinom1_sem_random <- AIC(modelo_nbinom1_conc_sem_random)
cat("\nAIC do Modelo nbinom1 sem Efeito Aleatório:\n")
print(aic_nbinom1_sem_random)
# BIC (Critério de Informação Bayesiano)
bic_nbinom1_sem_random <- BIC(modelo_nbinom1_conc_sem_random)
cat("\nBIC do Modelo nbinom1 sem Efeito Aleatório:\n")
print(bic_nbinom1_sem_random)
#DHARMa residual
png("dharma_residual_nbinom1.png", width = 800, height = 600, res = 100)
simulationOutput_nbinom1 <- simulateResiduals(fittedModel = modelo_nbinom1_conc_sem_random, plot = TRUE)
dev.off()

# 4) Modelo de Concentração com Distribuição Binomial Negativa tipo 2 (nbinom2) sem Efeito Aleatório (1 | subjectid)
## Ajusta o modelo GLMM com a família nbinom2 e o termo aleatório para 'subjectid'
modelo_nbinom2_conc_sem_random <- glmmTMB(concentracao_real ~ wave + Trajetoria,
                                          data = nanosight_intersect,
                                          family = nbinom2(link = "log"))
# Avaliação do Modelo
# Tabela principal com o teste Wald Chi-Square
cat("Análise de Variância (Anova) para o Modelo nbinom2 sem Efeito Aleatório:\n")
Anova(modelo_nbinom2_conc_sem_random, type = "II")
# Obter os contrastes para comparações pareadas
cat("\nComparação de médias (emmeans) para o Modelo nbinom2 sem Efeito Aleatório:\n")
emmeans(modelo_nbinom2_conc_sem_random, specs = pairwise ~ Trajetoria | wave, type = "response")
# Cálculo de ICC, AIC e BIC
# ICC (Coeficiente de Correlação Intraclasse)
icc_nbinom2_sem_random <- performance::icc(modelo_nbinom2_conc_sem_random)
cat("\nCoeficiente de Correlação Intraclasse (ICC):\n")
summary(icc_nbinom2_sem_random)
# AIC (Critério de Informação de Akaike)
aic_nbinom2_sem_random <- AIC(modelo_nbinom2_conc_sem_random)
cat("\nAIC do Modelo nbinom2 sem Efeito Aleatório:\n")
print(aic_nbinom2_sem_random)
# BIC (Critério de Informação Bayesiano)
bic_nbinom2_sem_random <- BIC(modelo_nbinom2_conc_sem_random)
cat("\nBIC do Modelo nbinom2 sem Efeito Aleatório:\n")
print(bic_nbinom2_sem_random)



