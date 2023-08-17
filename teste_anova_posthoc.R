#Carregar as bibliotecas necessárias
install.packages("tidyverse")
library(tidyverse)
#Teste T-pareado nos grupos A, B, C, D
nanosightsampleinformation_tudo <- inner_join(nanosight_sem_outliers, sample_information, by = "id_sample")
anti_join(nanosight_sem_outliers, nanosightsampleinformation_tudo, by = "id_sample")

#Criando tabela somente com as duplicatas (w1 e w2)
nanosight_sem_outliers$número <- as.numeric(gsub("\\D", "", nanosight_sem_outliers$id_sample))
nanosight_sem_outliers$número <- sub("\\d$", "", nanosight_sem_outliers$número)
nanosight_pares <- duplicated(nanosight_sem_outliers$número) | duplicated(nanosight_sem_outliers$número, fromLast = TRUE)
nanosight_pares_tabela <- nanosight_sem_outliers[nanosight_pares, ]
#Juntando nanosight_pares e sample_information
nanosightsampleinformation <- inner_join(nanosight_pares_tabela, sample_information, by = "id_sample")
#Separando os grupos w1 e w2
nanosight_w1 <- nanosight_sem_outliers[grep("w1", nanosight_sem_outliers$id_sample), ]
nanosight_w2<- nanosight_sem_outliers[grep("w2", nanosight_sem_outliers$id_sample), ]
#Teste ANOVA e post hoc Tukey
anova_w1w2 <- aov(tamanho_mean_average ~ wave, data = nanosightsampleinformation)
resultado_anova_w1w2 <- summary(anova_w1w2)