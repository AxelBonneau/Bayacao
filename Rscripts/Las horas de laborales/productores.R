# Charger les bibliothèques nécessaires
pacman::p_load(
  here, 
  rio, 
  readxl,
  writexl,
  openxlsx,
  dplyr
)

# Fonction pour normaliser les noms des municipalités
normalizar_municipalidades <- function(df, mapa_normalizacion) {
  df %>%
    mutate(Ubicacion = recode(Municipalidad, !!!mapa_normalizacion))
}

# Importer les données
importar_datos <- function() {
  df_productores <- import(here("documents", "bayacao", "dashboard", "datasets", "19Juil", "Compra Cacao Bayacao PROVA BACKUP.xlsx"), sheet = "Compra") %>%
    select(Fecha = 2, Productor = 5, Municipalidad = 6, Comprador = 18, Condicion = 22, Peso_pagado = 25) %>%
    mutate(Fecha = as.Date(Fecha))
  
  df_compra <- import(here("Documents","Bayacao", "Dashboard", "datasets","Compra", "compra_filtra.xlsx"))
  
  list(df_productores = df_productores, df_compra = df_compra)
}

# Définir le mapping pour normaliser les noms
mapa_normalizacion <- c(
  "esperalvillo" = "Peralvillo",
  "el dean" = "Cotuí",
  "sanchez ramirez" = "Cotuí",
  "San franc. De macoris" = "San Francisco de Macorís",
  "La victoria" = "Yamasá",
  "monte plata" = "Monte Plata",
  "Monte Plata" = "Monte Plata",
  "MONTE PLATA" = "Monte Plata",
  "santo domingo" = "Santo Domingo Norte",
  "Santo Domingo" = "Santo Domingo Norte",
  "Santo Domingo Norte" = "Santo Domingo Norte",
  "yamasa" = "Yamasá",
  "Yamasa" = "Yamasá",
  "YAMASA" = "Yamasá",
  "no registrado" = "No Registrado",
  "sabana grande de boya" = "Sabana Grande de Boyá",
  "ramirez sanchez" = "Cotuí",
  "Cotui" = "Cotuí"
)

# Prétraitement des données
pretraiter_datos <- function(df_productores, df_compra) {
  df_productores_normalizado <- normalizar_municipalidades(df_productores, mapa_normalizacion)
  
  df_mal_dia <- df_productores_normalizado %>%
    left_join(df_compra, by = c("Fecha", "Comprador", "Condicion")) %>%
    na.omit()
  
  df_mal_dia_filtrado <- df_mal_dia %>%
    group_by(Fecha, Productor, Condicion) %>%
    summarise(
      Ubicacion = first(Ubicacion),
      Comprador = first(Comprador),
      Peso_pagado = sum(Peso_pagado),
      total_kg_pagado = first(total_kg_pagado),
      total_kg_Bruto = first(total_kg_Bruto),
      delta_kg = first(delta_kg)
    )
  
  datos_contados <- df_mal_dia_filtrado %>%
    group_by(Productor) %>%
    tally(name = "count")
  
  df_mal_dia_filtrado %>%
    inner_join(datos_contados, by = "Productor") %>%
    filter(count >= 10) %>%
    mutate(
      pct_peso = round(Peso_pagado / total_kg_pagado, 2),
      reparticion = pct_peso * delta_kg
    )
}

# Calculer les différences par producteur
calculer_diff_prod <- function(df_mal_dia_filtrado) {
  df_mal_dia_filtrado %>%
    group_by(Productor, Condicion) %>%
    summarise(
      delta_kg = sum(reparticion),
      nbr_kg_vendido = sum(Peso_pagado)
    ) %>%
    arrange(delta_kg) %>%
    mutate(pct = round(delta_kg / nbr_kg_vendido, 2))
}

# Exécuter le processus
datos <- importar_datos()
df_mal_dia_filtrado <- pretraiter_datos(datos$df_productores, datos$df_compra)
df_diff_prod <- calculer_diff_prod(df_mal_dia_filtrado)


