pacman::p_load(
  readxl,
  dplyr,
  openxlsx,
  rio,
  here
)

# Chemins des fichiers
fichier_source <- here("datasets", "Compra","Compra Cacao Bayacao PROVA BACKUP.xlsx")
fichier_dest <- here("datasets","Compra","compra_filtra.xlsx")

# Charger les données existantes
if (file.exists(fichier_dest)) {
  df_existante <- import(here(fichier_dest))
} else {
  df_existante <- data.frame()  # Créer un dataframe vide si le fichier n'existe pas
}

# Charger les nouvelles données
Compra <- import(fichier_source, sheet = "Compra")
Entrada_Cacao <- import(fichier_source, sheet = "ENTRADA CACAO en FABRICA")

# Traitement des données
Compra <- Compra[c(2,5,18,22:26,29:32)]
colnames(Compra) <- c("Fecha","Productor","Comprador","Condicion","Precio_Quintal","Precio_KG","Peso_KG_Pagado",
                      "Kg_Quintal","Quintales_entregado","Compra_total_pesos","Pagado_real","Incentivo")
Compra$Fecha <- as.Date(Compra$Fecha)

Entrada_Cacao <- Entrada_Cacao[c(1,5,7,8,9)]
colnames(Entrada_Cacao) <- c('Fecha',"Condicion","Saco","Comprador","kg Bruto Recibido Fabrica")
Entrada_Cacao$Fecha <- as.Date(Entrada_Cacao$Fecha)

# Filtrer les informations et les regrouper
Compra_filtra <- Compra %>%
  group_by(Fecha, Comprador, Condicion) %>%
  summarise(total_kg_pagado = sum(Peso_KG_Pagado, na.rm = TRUE)) %>%
  ungroup()

df_relacion <- Compra_filtra %>%
  merge(Entrada_Cacao, by = c("Fecha","Condicion","Comprador"))

df_relacion_filtra <- df_relacion %>%
  group_by(Fecha,Condicion,Comprador) %>%
  summarise(total_kg_pagado = last(total_kg_pagado),
            total_kg_Bruto = sum(`kg Bruto Recibido Fabrica`),
            cantidad_saco = sum(Saco))

df_relacion_filtra$delta_kg <- df_relacion_filtra$total_kg_Bruto - df_relacion_filtra$total_kg_pagado 
df_relacion_filtra$proporcion_delta <-  round(df_relacion_filtra$delta_kg/df_relacion_filtra$total_kg_pagado *100,2)

q1 <- quantile(df_relacion_filtra$delta_kg, na.rm = TRUE, probs = 0.05)
q9 <- quantile(df_relacion_filtra$delta_kg, na.rm = TRUE, probs = 0.95)

df_relacion_filtra <- df_relacion_filtra %>%
  filter(Condicion %in% c("baba","seco","fermentado") & delta_kg > q1 & delta_kg < q9) %>%
  na.omit() %>%
  filter(proporcion_delta < 300)

df_relacion_filtra <- df_relacion_filtra %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

df_existante <- df_existante %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

if (nrow(df_existante) > 0) {
  df_ajout <- anti_join(df_relacion_filtra, df_existante)
} else {
  df_ajout <- df_relacion_filtra
}

if (nrow(df_ajout) > 0) {
  # Écrire les nouvelles lignes dans le fichier Excel de destination
  export(df_ajout, fichier_dest, append = TRUE)
  print("Les nouvelles données ont été ajoutées.")
} else {
  print("Aucune nouvelle donnée à ajouter.")
}

