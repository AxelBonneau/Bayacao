# Charger les bibliothèques nécessaires
pacman::p_load(
  here,
  purrr,
  stringr,
  stringdist,
  readxl,
  writexl, 
  dplyr,
  rio, 
  openxlsx,
  dbscan,
  data.table
)

file_path <- here("datasets","Las horas de laborales","Historial.xlsx")

# Lire toutes les feuilles du fichier Excel dans une liste
sheets <- excel_sheets(file_path)  # Obtenir les noms des feuilles

# Lire toutes les feuilles dans une liste
sheets_data <- lapply(sheets, function(sheet_name) {
  import(file_path, which = sheet_name)
})

names(sheets_data) <- sheets

output_path <- here("datasets","Las horas de laborales","Integral_Historial_filtered.xlsx")

process_sheets_in_chunks <- function(sheets_data, sheets, output_path, chunk_size = 10000, stop_threshold = 60) {
  
  # Fonction pour traiter une seule feuille en morceaux
  process_sheet_chunk <- function(data, chunk_start, chunk_end) {
    chunk <- data[chunk_start:chunk_end, ]
    
    # Supprimer les lignes avec des valeurs manquantes
    chunk <- na.omit(chunk)
    
    # Convertir la colonne de temps en format datetime
    chunk$Tiempo <- as.POSIXct(chunk$Tiempo, format="%Y-%m-%d %H:%M:%S")
    
    # Extraire les coordonnées et nettoyer les noms locaux
    chunk$local_name <- gsub("\\(.*\\)", "", chunk$Dirección)
    coords <- gsub(".*\\(|\\)", "", chunk$Dirección)
    
    coordsplit <- strsplit(coords, ",")
    chunk$longitude <- sapply(coordsplit, function(x) as.numeric(x[1]))
    chunk$latitude <- sapply(coordsplit, function(x) as.numeric(x[2]))
    
    # Calculer la distance parcourue entre les points successifs
    chunk <- chunk %>%
      arrange(Tiempo) %>%
      mutate(
        time_diff = as.numeric(difftime(Tiempo, lag(Tiempo, default = first(Tiempo)), units = "secs")),
        distance = as.numeric(`Velocidad (Kph)` * time_diff / 3600)
      )
    
    chunk$distance <- ifelse(chunk$distance > 300, 0, chunk$distance)
    
    # Identifier les arrêts
    chunk <- chunk %>%
      mutate(is_stop = ifelse(time_diff >= stop_threshold & `Velocidad (Kph)` < 1, TRUE, FALSE)) %>%
      mutate(time_diff = ifelse(time_diff > 50000 & !is_stop, 0, time_diff))
    
    chunk <- chunk %>%
      select(Tiempo, Motor, local_name, longitude, latitude, distance, time_diff, speed = `Velocidad (Kph)`, is_stop)
    
    return(chunk)
  }
  
  processed_sheets <- lapply(sheets, function(sheet_name) {
    tryCatch({
      data <- sheets_data[[sheet_name]]
      if (is.null(data) || nrow(data) == 0) {
        stop("Sheet is empty or does not exist")
      }
      
      # Ajout de messages de diagnostic
      message("Processing sheet: ", sheet_name)
      
      # Assurer que les colonnes nécessaires sont présentes
      required_columns <- c("Tiempo", "Dirección", "Velocidad (Kph)")
      missing_columns <- setdiff(required_columns, names(data))
      if (length(missing_columns) > 0) {
        stop("Missing columns: ", paste(missing_columns, collapse = ", "))
      }
      
      data$Tiempo <- as.POSIXct(data$Tiempo, format="%Y-%m-%d %H:%M:%S")
      
      total_rows <- nrow(data)
      
      results <- list()
      for (start_row in seq(1, total_rows, by = chunk_size)) {
        end_row <- min(start_row + chunk_size - 1, total_rows)
        
        message(sprintf("Processing rows %d to %d in sheet %s", start_row, end_row, sheet_name))
        
        chunk_result <- tryCatch({
          process_sheet_chunk(data, start_row, end_row)
        }, error = function(e) {
          message(sprintf("Error processing chunk from rows %d to %d in sheet %s: %s", 
                          start_row, end_row, sheet_name, conditionMessage(e)))
          return(NULL)
        })
        
        if (!is.null(chunk_result)) {
          results <- append(results, list(chunk_result))
        } else {
          message(sprintf("Skipping chunk from rows %d to %d due to error", start_row, end_row))
        }
      }
      combined_result <- rbindlist(results)
      return(list(sheet_name = sheet_name, data = combined_result))
      
    }, error = function(e) {
      message(sprintf("Error processing sheet %s: %s", sheet_name, e$message))
      return(NULL)
    })
  })
  
  # Filtrer les résultats NULL
  processed_sheets <- Filter(Negate(is.null), processed_sheets)
  
  # Sauvegarder les données traitées dans un fichier Excel
  wb <- createWorkbook()
  for (sheet in processed_sheets) {
    addWorksheet(wb, sheet$sheet_name)
    writeData(wb, sheet$sheet_name, sheet$data)
  }
  saveWorkbook(wb, output_path, overwrite = TRUE)
  
  return(processed_sheets)
}

process_sheets_in_chunks(sheets_data, sheets, output_path)

