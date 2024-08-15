# Charger les bibliothèques nécessaires
pacman::p_load(
  here, 
  rio, 
  readxl,
  writexl,
  openxlsx,
  dplyr
)

output_file <- here("datasets","Compra","desempeno_del_productor.xlsx")

# Fonction pour normaliser les noms des municipalités
normalizar_municipalidades <- function(df, mapa_normalizacion) {
  df %>%
    mutate(Ubicacion = recode(Municipalidad, !!!mapa_normalizacion))
}

# Importer les données
importar_datos <- function() {
  df_productores <- import(here("datasets", "Compra", "Compra Cacao Bayacao PROVA BACKUP.xlsx"), sheet = "Compra") %>%
    select(Fecha = 2, Productor = 5, Municipalidad = 6, Comprador = 18, Condicion = 22, Peso_pagado = 25) %>%
    mutate(Fecha = as.Date(Fecha))
  
  df_compra <- import(here("datasets","Compra", "compra_filtra.xlsx"))
  
  list(df_productores = df_productores, df_compra = df_compra)
}

# Prétraitement des données
pretraiter_datos <- function(df_productores, df_compra) {
  df_mal_dia <- df_productores %>%
    left_join(df_compra, by = c("Fecha", "Comprador", "Condicion")) %>%
    na.omit()
  
  df_productores_dia <- df_mal_dia %>%
    group_by(Fecha, Productor, Condicion) %>%
    summarise(
      Ubicacion = first(Municipalidad),
      Comprador = first(Comprador),
      Peso_pagado = sum(Peso_pagado),
      total_kg_pagado = first(total_kg_pagado),
      total_kg_Bruto = first(total_kg_Bruto),
      delta_kg = first(delta_kg)
    )
  
  datos_contados <- df_productores_dia %>%
    group_by(Productor) %>%
    tally(name = "count")
  
  df_productores_dia %>%
    inner_join(datos_contados, by = "Productor") %>%
    filter(count >= 10) %>%
    mutate(
      pct_peso = round(Peso_pagado / total_kg_pagado, 2),
      reparticion = pct_peso * delta_kg
    )  %>%
    select(Fecha, Productor, Condicion, Ubicacion, Comprador, pct_peso, reparticion)
}

# Exécuter le processus
datos <- importar_datos()
df_productores_dia <- pretraiter_datos(datos$df_productores, datos$df_compra)

wb <- createWorkbook()
addWorksheet(wb, "desempeno del productor")
writeData(wb, "desempeno del productor", df_productores_dia)
saveWorkbook(wb, output_file, overwrite = T)
