#Definir pasta de trabalho
setwd("C:/Users/Belle/Documents/Belle - Nanosight")

#ESTATÍSTICA PARA BATCH
shapiro.test(nanosight_plus_sampleinfo$tamanho_mean_average) #1.66e-05
modelo_mean <- aov(tamanho_mean_average~batch_nan, data = nanosight_plus_sampleinfo)
levene_mean <- leveneTest(modelo_mean) #0.002
resultado_robusto_batch_mean <- t1way(tamanho_mean_average~batch_nan, data = nanosight_plus_sampleinfo)
dunn.test(nanosight_plus_sampleinfo$tamanho_mean_average, nanosight_plus_sampleinfo$batch_nan, kw = TRUE, method = "bonferroni", label = TRUE) #J-I
dunn.test(nanosight_plus_sampleinfo$concentracao_real, nanosight_plus_sampleinfo$batch_nan, kw = TRUE, method = "bonferroni", label = TRUE)

### ANÁLISES ESTATÍSTICAS ###
#Testes de normalidade (premissa para o ANOVA)
shapiro.test(nanosight_w1_sem_outliers_mean$tamanho_mean_average) #0.005
shapiro.test(nanosight_w1_sem_outliers_porcentagem$EV_pequenas_porcentagem) #4.148e-09
shapiro.test(nanosight_w2_sem_outliers_mean$tamanho_mean_average) #0.02
shapiro.test(nanosight_w2_sem_outliers_porcentagem$EV_pequenas_porcentagem) #6.913e-10
#Testes de homogeneidade de variâncias (premissa para o ANOVA)
library(car)
modelo_w1_mean <- aov(tamanho_mean_average~Trajetoria, data = nanosight_w1_sem_outliers_mean)
levene_w1_mean <- leveneTest(modelo_w1_mean) #p 0,002
modelo_w1_porcentagem <- aov(EV_pequenas_porcentagem~Trajetoria, data = nanosight_w1_sem_outliers_porcentagem)
levene_w1_porcentagem <- leveneTest(modelo_w1_porcentagem) #p 9.444e-05
modelo_w2_mean <- aov(tamanho_mean_average~Trajetoria, data = nanosight_w2_sem_outliers_mean)
levene_w2_mean <- leveneTest(modelo_w2_mean) #p 0.06546
modelo_w2_porcentagem <- aov(EV_pequenas_porcentagem~Trajetoria, data = nanosight_w2_sem_outliers_porcentagem)
levene_w2_porcentagem <- leveneTest(modelo_w2_porcentagem) #p 0,1
#ANOVA robusta
library(WRS2)
resultado_robusto_t1way_w1_mean <- t1way(tamanho_mean_average~Trajetoria, data = nanosight_w1_sem_outliers_mean) #p 0,03
resultado_robusto_t1way_w1_porcentagem <- t1way(EV_pequenas_porcentagem~Trajetoria, data = nanosight_w1_sem_outliers_porcentagem) #p 0,003
resultado_robusto_t1way_w2_mean <- t1way(tamanho_mean_average~Trajetoria, data = nanosight_w2_sem_outliers_mean) #p 0,22
resultado_robusto_t1way_w2_porcentagem <- t1way(EV_pequenas_porcentagem~Trajetoria, data = nanosight_w2_sem_outliers_porcentagem) #0,04
#Teste de Dunn (O teste de Dunn é uma alternativa não paramétrica ao teste de Tukey, adequado para dados que não atendem aos pressupostos de normalidade ou homogeneidade de variâncias)
library(dunn.test)
dunn.test(nanosight_w1_sem_outliers_mean$tamanho_mean_average, nanosight_w1_sem_outliers_mean$Trajetoria, kw = TRUE, label = TRUE) #B-A e B-D
dunn.test(nanosight_w1_sem_outliers_porcentagem$EV_pequenas_porcentagem, nanosight_w1_sem_outliers_porcentagem$Trajetoria, kw = TRUE, label = TRUE) #B-A; B-C; B-D
dunn.test(nanosight_w1_sem_outliers_concentracao$concentracao_real, nanosight_w1_sem_outliers_concentracao$Trajetoria, kw = TRUE, label = TRUE) #A-B; B-C; B-D
dunn.test(nanosight_w2_sem_outliers_mean$tamanho_mean_average, nanosight_w2_sem_outliers_mean$Trajetoria, kw = TRUE, label = TRUE) #sem diferença
dunn.test(nanosight_w2_sem_outliers_porcentagem$EV_pequenas_porcentagem, nanosight_w2_sem_outliers_porcentagem$Trajetoria, kw = TRUE, label = TRUE) #B-A; B-D
dunn.test(nanosight_w2_sem_outliers_concentracao$concentracao_real, nanosight_w2_sem_outliers_concentracao$Trajetoria, kw = TRUE, label = TRUE) #A-B; B-C;B-D

#### possibilidade para análise com outras variáveis
#ANOVA robusta de 2 vias
resultado_anova_duas_vias_tam_wave_traj <- aov(tamanho_mean_average ~ wave * Trajetoria, data = nanosight_plus_sampleinfo)
summary(resultado_anova_duas_vias_tam_wave_traj)
resultado_anova_duas_vias_tam_batch_traj <- aov(tamanho_mean_average ~ batch_nan * Trajetoria, data = nanosight_plus_sampleinfo)
summary(resultado_anova_duas_vias_tam_batch_traj)
resultado_anova_duas_vias_conc_batch_traj <- aov(concentracao_real ~ batch_nan * Trajetoria, data = nanosight_plus_sampleinfo)
summary(resultado_anova_duas_vias_conc_batch_traj)

#teste Pearson
cor(nanosight_plus_sampleinfo$concentracao_real, nanosight_plus_sampleinfo$tamanho_mean_average, method = "pearson") #0.27 (correlação media)
