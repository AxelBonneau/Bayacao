pacman::p_load(
  readxl,
  dplyr,
  lubridate,
  tmap,
  sf,
  openxlsx
)

# Chemin du fichier Excel
file_path <- import(here("Documents","Bayacao", "Dashboard","datasets","Las horas de laborales","Work hours all units BAYACAO TRUCKS.xlsx"))

# Lire les noms de toutes les feuilles
sheet_names <- excel_sheets(file_path)

# Lire toutes les feuilles et les stocker dans une liste
all_sheets <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet, 
             col_types = c("numeric", "text", "text", 
                           "text", "text", "numeric", "text", 
                           "numeric", "numeric", "text"))
})

# Fonction pour appliquer les modifications à un dataframe
process_sheet <- function(df) {
  df <- df[2:9]
  df <- na.omit(df)
  
  df$Worksdate <- as.Date(substr(df$`Hora de inicio`,1,10), format = "%d/%m/%Y")
  df$`Hora de inicio` <- strptime(df$`Hora de inicio`, format = "%d/%m/%Y %H:%M:%S", tz = "EST")
  df$`Hora de finalización` <- strptime(df$`Hora de finalización`, format = "%d/%m/%Y %H:%M:%S", tz = "EST")
  df$`Tiempo de trabajo` <- df$`Hora de finalización` - df$`Hora de inicio`
  
  df$local_name <- gsub("\\(.*\\)","",df$Ubicación)
  coords <- gsub(".*\\(|\\)","",df$Ubicación)
  
  coordsplit <- strsplit(coords,",")
  df$x <- sapply(coordsplit, function(x) as.numeric(x[1]))
  df$y <- sapply(coordsplit, function(x) as.numeric(x[2]))
  
  df$`Menos de 50 m` <- df$`Kilometraje (Km.)` < 0.050
  
  # Créer un groupe incrémenté à chaque FALSE
  df$group <- cumsum(!df$`Menos de 50 m` | (df$`Menos de 50 m` & lag(df$`Menos de 50 m`, default = FALSE) == FALSE))
  df$adjusted_group <- df$group
  
  # Itérer pour ajuster les groupes correctement
  for (i in 2:nrow(df)) {
    if (df$`Menos de 50 m`[i] == TRUE && df$group[i] == df$group[i - 1] && df$Worksdate[i] != df$Worksdate[i - 1]) {
      df$adjusted_group[i:nrow(df)] <- df$adjusted_group[i:nrow(df)] + 1
    }
  }
  
  df_grouped <- df %>%
    group_by(adjusted_group) %>%
    summarise(total_km = sum(`Kilometraje (Km.)`),
              total_trabajo = sum(`Tiempo de trabajo`),
              lugar = first(local_name),
              primera_fecha = first(`Hora de inicio`),
              ultima_fecha = last(`Hora de finalización`),
              coord_x = first(x),
              coord_y = first(y))
  
  df_grouped$coord_x <- round(df_grouped$coord_x,3)
  df_grouped$coord_y <- round(df_grouped$coord_y,3)
  df_grouped$total_trabajo <- as.numeric(df_grouped$total_trabajo)  # Assurez-vous que c'est en nombre
  
  # Choisir une date de départ
  start_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  
  df_grouped$total_trabajo <- start_date + df_grouped$total_trabajo
  
  return(df_grouped)
}

# Appliquer les modifications à toutes les feuilles
processed_sheets <- lapply(all_sheets, process_sheet)

# Chemin du nouveau fichier Excel
output_file_path <- "./WorksHours/Work_hours.xlsx"

# Créer un nouveau workbook
wb <- createWorkbook()

# Ajouter chaque dataframe modifié comme une nouvelle feuille
for (i in seq_along(processed_sheets)) {
  addWorksheet(wb, sheetName = sheet_names[i])
  writeData(wb, sheet = sheet_names[i], processed_sheets[[i]])
}

# Sauvegarder le workbook
saveWorkbook(wb, file = output_file_path, overwrite = TRUE)


