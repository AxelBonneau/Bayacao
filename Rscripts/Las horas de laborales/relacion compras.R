pacman::p_load(
  readxl,
  dplyr,
  ggplot2,
  openxlsx,
  plotly,
  reshape2
)

options(encoding = "UTF-8")

###########################################################################
######################## LA SELECCIÓN DE LOS DATOS ########################
###########################################################################

# Extraer los bancos de datos
Compra <- import(here("Documents","Bayacao", "Dashboard", "datasets", "Compra","Compra Cacao Bayacao PROVA BACKUP.xlsx"), 
                                                sheet = "Compra")

Entrada_Cacao <- read_excel("datasets/19Juil/Compra Cacao Bayacao PROVA BACKUP.xlsx", 
                     sheet = "ENTRADA CACAO en FABRICA")

# Solamante recuperar las columnas esenciales y renombrar
Compra <- Compra[c(2,5,18,22:26,29:32)]

colnames(Compra) <- c("Fecha","Productor","Comprador","Condicion","Precio_Quintal","Precio_KG","Peso_KG_Pagado",
                     "Kg_Quintal","Quintales_entregado","Compra_total_pesos","Pagado_real","Incentivo")

Compra$Fecha <- as.Date(Compra$Fecha)

Entrada_Cacao <- Entrada_Cacao[c(1,5,7,8,9)]

colnames(Entrada_Cacao) <- c('Fecha',"Condicion","Saco","Comprador","kg Bruto Recibido Fabrica")

Entrada_Cacao$Fecha <- as.Date(Entrada_Cacao$Fecha)

# Filtrar las informaciones y juntar los pesos pagados segun la fecha, el comprador y la condicion (baba, seco, ...)
Compra_filtra <- Compra %>%
  group_by(Fecha, Comprador, Condicion) %>%
  summarise(total_kg_pagado = sum(Peso_KG_Pagado, na.rm = TRUE),
            productores = paste(unique(Productor), collapse = ", ")) %>%
  ungroup() 

# Juntar las bases de datos entre Compra y Entrada_Cacao y filtrar de nuevo 
df_relacion <- Compra_filtra %>%
  merge(Entrada_Cacao, by = c("Fecha","Condicion","Comprador"))

df_relacion_filtra <- df_relacion %>%
  group_by(Fecha,Condicion,Comprador) %>%
  summarise(total_kg_pagado = last(total_kg_pagado),
            total_kg_Bruto = sum(`kg Bruto Recibido Fabrica`),
            cantidad_saco = sum(Saco),
            productores = first(productores))

# Calcular la differencia entre el total kg pagado y entrado
df_relacion_filtra$delta_kg <- df_relacion_filtra$total_kg_Bruto - df_relacion_filtra$total_kg_pagado 
df_relacion_filtra$proporcion_delta <-  round(df_relacion_filtra$delta_kg/df_relacion_filtra$total_kg_pagado *100,2)

# Conocer el primero cuartil y la ultima cuartil para limpiar los datos extremos
q1 <- quantile(df_relacion_filtra$delta_kg, na.rm = T, probs = 0.05)
q9 <- quantile(df_relacion_filtra$delta_kg, na.rm = T, probs = 0.95)

df_relacion_filtra <- df_relacion_filtra %>%
  filter(Condicion %in% c("baba","seco","fermentado") & delta_kg > q1 & delta_kg < q9)
df_relacion_filtra <- na.omit(df_relacion_filtra)

# Agregar un graficó para ver las faltas segun los compradores y las condiciones
f <- ggplot(df_relacion_filtra, aes(y = delta_kg, x = Comprador, fill = Comprador)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, notch = TRUE) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribution de la différence de kg (acheté et rentré à l'usine) par Acheteur",
    x = "Acheteur",
    y = "Delta Kg",
    fill = "Acheteur"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
f

df_relacion_filtra <- df_relacion_filtra %>%
  filter(proporcion_delta < 300)

# Création d'histogrammes pour chaque variable
truehist(df_relacion_filtra$cantidad_saco, main = "Histogramme de var1", xlab = "Var1")
truehist(df_relacion_filtra$total_kg_Bruto, main = "Histogramme de var2", xlab = "Var2")
truehist(df_relacion_filtra$erreur_absolue, main = "Histogramme de var3", xlab = "Var3")
truehist(df_relacion_filtra$proporcion_delta, main = "Histogramme de var4", xlab = "Var4")

# Création de l'histogramme
ggplot(df_relacion_filtra, aes(x = cantidad_saco)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +  # Histogramme avec couleur de remplissage
  labs(title = "Distribution des quantités de sacs",
       x = "Quantité de sacs",
       y = "Fréquence") +  # Titre et labels des axes
  theme_minimal() +  # Utilisation d'un thème minimal pour un look épuré
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centrer et mettre en gras le titre
    axis.title.x = element_text(face = "bold", size = 12),  # Mettre en gras les titres des axes
    axis.title.y = element_text(face = "bold", size = 12)
  )

df_relacion_filtra_baba <- df_relacion_filtra %>%
  filter(Condicion =="baba")

df_relacion_filtra_seco <- df_relacion_filtra %>%
  filter(Condicion =="seco")

# Calculer la somme des poids 
somme_poids <- sum(df_relacion_filtra$total_kg_pagado, na.rm = TRUE)

# Calculer la moyenne pondérée des proportions
moyenne_ponderee <- sum(df_relacion_filtra$proporcion_delta * df_relacion_filtra$total_kg_pagado, na.rm = TRUE) / somme_poids

print(sum(df_relacion_filtra$total_kg_pagado))
print(sum(df_relacion_filtra$total_kg_Bruto)) 
print(sum(df_relacion_filtra$delta_kg)) 
print(median(df_relacion_filtra$proporcion_delta))

write.xlsx(df_relacion_filtra,"./Compra/compra_filtra.xlsx")

###########################################################################
#########################     FASE DE PRACTICO    #########################
###########################################################################

df_relacion_filtra$proporcion_delta[is.infinite(df_relacion_filtra$proporcion_delta)] <- NA
df_relacion_filtra$erreur_absolue <- abs(df_relacion_filtra$delta_kg)

# Ajuster la loi de Cauchy
fit_cauchy <- fitdistr(df_relacion_filtra$proporcion_delta, "cauchy")

# Vérifier les résultats
fit_cauchy

ggplot(df_relacion_filtra, aes(x = proporcion_delta)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dcauchy, args = list(location = fit_cauchy$estimate["location"], scale = fit_cauchy$estimate["scale"]), color = "red") +
  ggtitle("Histogramme avec Ajustement de la Loi de Cauchy") +
  xlab("proporcion_delta") +
  ylab("Densité")

ks.test(df_relacion_filtra$proporcion_delta, "pcauchy", location = fit_cauchy$estimate["location"], scale = fit_cauchy$estimate["scale"])
# Paramètres de la loi de Cauchy ajustée
location <- fit_cauchy$estimate["location"]
scale <- fit_cauchy$estimate["scale"]

# Calculer les quantiles à 2.5% et 97.5%
quantile_2_5 <- qcauchy(0.05, location = location, scale = scale)
quantile_97_5 <- qcauchy(0.95, location = location, scale = scale)

quantile_2_5
quantile_97_5

# Créer un histogramme des données avec la densité Cauchy ajustée
ggplot(df_relacion_filtra, aes(x = proporcion_delta)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dcauchy, args = list(location = location, scale = scale), color = "red") +
  geom_vline(xintercept = quantile_2_5, linetype = "dashed", color = "green") +
  geom_vline(xintercept = quantile_97_5, linetype = "dashed", color = "purple") +
  ggtitle("Histogramme avec Ajustement de la Loi de Cauchy et Seuils des 5%") +
  xlab("delta_kg") +
  ylab("Densité")

df_relacion_filtra$abs_delta_kg <- abs(df_relacion_filtra$delta_kg)
# Ajouter une petite constante pour éviter les zéros
epsilon <- 1e-6
df_relacion_filtra$abs_delta_kg_adjusted <- df_relacion_filtra$abs_delta_kg + epsilon

# Ajuster la loi log-normale
fit_lognormal <- fitdistr(df_relacion_filtra$abs_delta_kg_adjusted, "lognormal")
fit_lognormal

meanlog <- fit_lognormal$estimate["meanlog"]
sdlog <- fit_lognormal$estimate["sdlog"]

# Créer un histogramme des données avec la densité log-normale ajustée
ggplot(df_relacion_filtra, aes(x = abs_delta_kg)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog), color = "red") +
  ggtitle("Histogramme avec Ajustement de la Loi Log-Normale") +
  xlab("Valeur Absolue de delta_kg") +
  ylab("Densité")

# Exclure les valeurs égales à zéro pour le test
data_non_zero <- df_relacion_filtra$abs_delta_kg[df_relacion_filtra$abs_delta_kg > 0]

ks_test <- ks.test(data_non_zero, "plnorm", meanlog = meanlog, sdlog = sdlog, exact = T)
ks_test

quantile_2_5 <- qlnorm(0.025, meanlog = meanlog, sdlog = sdlog)
quantile_97_5 <- qlnorm(0.975, meanlog = meanlog, sdlog = sdlog)

quantile_2_5
quantile_97_5

hist(data_non_zero, breaks = 30, main = "Histogram of Data", xlab = "Value", ylab = "Frequency")

# Ajuster une loi Gamma
fit_gamma <- fitdistr(data_non_zero, "gamma")
fit_gamma

# Paramètres ajustés pour la loi Gamma
shape <- fit_gamma$estimate["shape"]
rate <- fit_gamma$estimate["rate"]

# Créer un histogramme des données avec la densité Gamma ajustée
ggplot(df_relacion_filtra, aes(x = abs_delta_kg)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dgamma, args = list(shape = shape, rate = rate), color = "red") +
  ggtitle("Histogramme avec Ajustement de la Loi Gamma") +
  xlab("Valeur Absolue de delta_kg") +
  ylab("Densité")


quantile_95 <- qgamma(0.95, shape = shape, rate = rate)
quantile_95

# Créer un histogramme des données avec la densité Gamma ajustée
ggplot(df_relacion_filtra, aes(x = abs_delta_kg)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dgamma, args = list(shape = shape, rate = rate), color = "red") +
  geom_vline(xintercept = quantile_2_5, linetype = "dashed", color = "green") +
  geom_vline(xintercept = quantile_95, linetype = "dashed", color = "purple") +
  ggtitle("Histogramme avec Ajustement de la Loi Gamma et Seuils des 5%") +
  xlab("Valeur Absolue de delta_kg") +
  ylab("Densité")


# Test de Kolmogorov-Smirnov
ks_test <- ks.test(data_non_zero, "pgamma", shape = shape, rate = rate)
ks_test

# Ajuster une loi exponentielle
fit_exponential <- fitdistr(df_relacion_filtra$abs_delta_kg[df_relacion_filtra$abs_delta_kg > 0], "exponential")

# Afficher les paramètres estimés
fit_exponential

# Paramètre ajusté pour la loi exponentielle (taux)
rate <- fit_exponential$estimate["rate"]

# Calculer les quantiles à 2.5% et 97.5%
quantile_2_5 <- qexp(0.025, rate = rate)
quantile_97_5 <- qexp(0.975, rate = rate)

quantile_2_5
quantile_97_5

rate <- fit_exponential$estimate["rate"]

# Créer un histogramme des données avec la densité exponentielle ajustée
ggplot(df_relacion_filtra, aes(x = abs_delta_kg)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dexp, args = list(rate = rate), color = "red") +
  ggtitle("Histogramme avec Ajustement de la Loi Exponentielle") +
  xlab("Valeur Absolue de delta_kg") +
  ylab("Densité")

# Test de Kolmogorov-Smirnov
ks_test <- ks.test(df_relacion_filtra$abs_delta_kg[df_relacion_filtra$abs_delta_kg > 0], "pexp", rate = rate)
ks_test

# Calcul de la moyenne et de l'écart type
mean_proporcion_delta <- mean(df_relacion_filtra$proporcion_delta)
sd_proporcion_delta <- sd(df_relacion_filtra$proporcion_delta)

# Création de l'histogramme avec ajustement de la loi normale
ggplot(df_relacion_filtra, aes(x = proporcion_delta)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +  # Histogramme avec densité
  stat_function(fun = dnorm, args = list(mean = mean_proporcion_delta, sd = sd_proporcion_delta), color = "red") +  # Ajustement de la loi normale
  ggtitle("Histogramme avec Ajustement de la Loi Normale") +
  xlab("Proporcion Delta") +
  ylab("Densité") +
  theme_minimal() +  # Thème épuré
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centrer et mettre en gras le titre
    axis.title.x = element_text(face = "bold", size = 12),  # Mettre en gras les titres des axes
    axis.title.y = element_text(face = "bold", size = 12)
  )

# Création de l'histogramme avec ajustement de la loi normale
ggplot(df_relacion_filtra, aes(x = Moyenne_Relative)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +  # Histogramme avec densité
  stat_function(fun = dnorm, args = list(mean = moyenne_relative, sd = sd_relative), color = "red") +  # Ajustement de la loi normale
  ggtitle("Histogramme avec Ajustement de la Loi Normale") +
  xlab("Proporcion Delta") +
  ylab("Densité") +
  theme_minimal() +  # Thème épuré
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centrer et mettre en gras le titre
    axis.title.x = element_text(face = "bold", size = 12),  # Mettre en gras les titres des axes
    axis.title.y = element_text(face = "bold", size = 12)
  )

mean(df_relacion_filtra_baba$total_kg_pagado)
