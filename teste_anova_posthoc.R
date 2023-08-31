#Carregar as bibliotecas necessárias
library(tidyverse)
#Tabela Nanosight sem outliers com as infos das amostras
nanosightsampleinformation_tudo <- inner_join(nanosight_sem_outliers, sample_information, by = "id_sample") #3 amostras outliers
anti_join(nanosight_sem_outliers, nanosightsampleinformation_tudo, by = "id_sample") #6 amostras excluídas sem correspondência na tabela geral da Jess
#Teste ANOVA e post hoc de Tukey (diferença estatística entre os grupos?)
teste_anova_mean <- aov(tamanho_mean_average ~ Trajetoria, data = nanosightsampleinformation_tudo)
summary(teste_anova_mean) ##SEM DIFERENÇA ENTRE OS GRUPOS A/B/C/D PARA A MÉDIA
w1 <- nanosightsampleinformation_tudo[nanosightsampleinformation_tudo$wave == "w1",]
teste_anova_mean_w1 <- aov(tamanho_mean_average ~ Trajetoria, data = w1)
summary(teste_anova_mean_w1) ##HÁ DIFERENÇA QUANDO CONTAMOS APENAS W1
teste_tukey_mean_w1 <- TukeyHSD(teste_anova_mean_w1)
print(teste_tukey_mean_w1) ###DIFERENÇA C-B
w2 <- nanosightsampleinformation_tudo[nanosightsampleinformation_tudo$wave == "w2",]
teste_anova_mean_w2 <- aov(tamanho_mean_average ~ Trajetoria, data = w2)
summary(teste_anova_mean_w2) ##SEM DIFERENÇA QUANDO CONTAMOS APENAS W2
teste_anova_mode <- aov(tamanho_mode_average ~ Trajetoria, data = nanosightsampleinformation_tudo)
summary(teste_anova_mode) ##HÁ DIFERENÇA ENTRE OS GRUPOS A/B/C/D PARA A MODA
teste_tukey_mode <- TukeyHSD(teste_anova_mode)
print(teste_tukey_mode) ###DIFERENÇA D-B
teste_anova_mode_w1 <- aov(tamanho_mode_average ~ Trajetoria, data = w1)
summary(teste_anova_mode_w1) ##HÁ DIFERENÇA QUANDO CONTAMOS APENAS W1
teste_tukey_mode_w1 <- TukeyHSD(teste_anova_mode_w1)
print(teste_tukey_mode_w1) ##DIFERENÇA D-B
teste_anova_mode_w2 <- aov(tamanho_mode_average ~ Trajetoria, data = w2)
summary(teste_anova_mode_w2) ##SEM DIFERENÇA QUANDO CONTAMOS APENAS W2

############ PAREI AQUI EM 30/08/23

##Teste T-pareado para a moda (sem diferença estatística entre os grupos)
#Criando tabela somente com as duplicatas (w1 e w2)
nanosight_sem_outliers$número <- as.numeric(gsub("\\D", "", nanosight_sem_outliers$id_sample))
nanosight_sem_outliers$número <- sub("\\d$", "", nanosight_sem_outliers$número)
nanosight_pares <- duplicated(nanosight_sem_outliers$número) | duplicated(nanosight_sem_outliers$número, fromLast = TRUE)
nanosight_pares_tabela <- nanosight_sem_outliers[nanosight_pares, ]
#Juntando nanosight_pares e sample_information
nanosightsampleinformation_pares <- inner_join(nanosight_pares_tabela, sample_information, by = "id_sample")
#Aplicando o teste
nanosight_w1 <- nanosightsampleinformation_pares[grep("w1", nanosightsampleinformation_pares$wave), ]
nanosight_w2 <- nanosightsampleinformation_pares[grep("w2", nanosightsampleinformation_pares$wave), ]
teste_t_pareado <- t.test(nanosight_w1$tamanho_mode_average, nanosight_w2$tamanho_mode_average, paired = TRUE) #não foi possível rejeitar a hipótese nula p/ a moda entre w1 e w2

################################# PAREI AQUI EM 22/08/23

#Separando os grupos A, B, C, D
nanosight_a <- nanosightsampleinformation_tudo[grep("A", nanosightsampleinformation_tudo$Trajetoria), ]
nanosight_b <- nanosightsampleinformation_tudo[grep("B", nanosightsampleinformation_tudo$Trajetoria), ]
nanosight_c <- nanosightsampleinformation_tudo[grep("C", nanosightsampleinformation_tudo$Trajetoria), ]
nanosight_d <- nanosightsampleinformation_tudo[grep("D", nanosightsampleinformation_tudo$Trajetoria), ]
#Separando os grupos w1 e w2
nanosight_w1 <- nanosight_sem_outliers[grep("w1", nanosight_sem_outliers$id_sample), ]
nanosight_w2 <- nanosight_sem_outliers[grep("w2", nanosight_sem_outliers$id_sample), ]
