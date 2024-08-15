pacman::p_load(
  readxl,
  dplyr,
  ggplot2,
  openxlsx,
  rio,
  here,
  reshape2
)

df <- import(here("datasets","meteo","BAYACAO_FACTORIA.xlsx"))

df_f <- df %>%
  mutate(hour = hour(`Date Heure, GMT-04:00`)) %>%  # Extraire l'heure de la colonne Date
  group_by(Date) %>%
  summarise(
    prcp = sum(`Pluie, mm`),
    temp = mean(`Temp., °C`),
    hr = mean(if_else(hour >= 18 | hour < 6, NA_real_, `HR, %`), na.rm = TRUE),
    rs = mean(if_else(hour >= 18 | hour < 6, NA_real_, `Rayonnement solaire, W/m²`), na.rm = TRUE),
    wind = sum(`Vitesse du vent, m/s`) * 60 ,
    gust = sum(`Vitesse des rafales, m/s`) * 60 ,
    dir_wind = mean(`Direction du vent, ø`)
  ) %>%
  rename(date = Date)

wb <- createWorkbook()
addWorksheet(wb,"Pluviometrie")
writeData(wb, "Pluviometrie", df_f)
saveWorkbook(wb, here("datasets","meteo","datos_meteo.xlsx"), overwrite = T)






