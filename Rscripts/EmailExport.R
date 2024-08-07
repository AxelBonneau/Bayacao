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

download_folder <- here("..","..","downloads")  
extract_folder <- here("..","..","extracted")
file_path <- here("datasets", "Las horas de laborales", "historial.xlsx")

# Liste des motifs de fichiers ZIP
patterns <- c("^Historial_Isuzu_Factoria_1_Grande_PP.*\\.zip$",
              "^Historial_Isuzu_Factoria_2_L-464339.*\\.zip$",
              "^Historial_Isuzu_Factoria_3_L-464324.*\\.zip$",
              "^Historial_Land_Cruiser_L-466767.*\\.zip$",
              "^Historial_Land_Cruiser_L-473796.*\\.zip$")

# Fonction pour lister les fichiers correspondant à un motif
list_zip_files <- function(patterns, folder) {
  zip_files <- unlist(lapply(patterns, function(pattern) {
    list.files(folder, pattern = pattern, full.names = TRUE)
  }))
  return(zip_files)
}

# Listez tous les fichiers ZIP correspondant aux motifs
zip_files <- list_zip_files(patterns, download_folder)
zip_files

# Décompressez les fichiers ZIP et capturez les fichiers extraits
files <- unlist(lapply(zip_files, function(zip_file) {
  if (file.exists(zip_file)) {
    unzip(zip_file, exdir = extract_folder)
  } else {
    print(paste("Fichier ZIP non trouvé :", zip_file))
  }
}))

# Lire toutes les feuilles du fichier Excel dans une liste
sheets <- excel_sheets(file_path)  # Obtenir les noms des feuilles

# Lire toutes les feuilles dans une liste
sheets_data <- lapply(sheets, function(sheet_name) {
  import(file_path, which = sheet_name)
})

names(sheets_data) <- sheets

# Lister tous les fichiers CSV dans le dossier d'extraction
csv_files <- list.files(extract_folder, pattern = "\\.csv$", full.names = TRUE)

# Fonction pour nettoyer et normaliser les noms
normalize_name <- function(name) {
  name <- str_to_lower(name) # Convertir en minuscules
  name <- str_squish(name)   # Supprimer les espaces multiples
  return(name)
}

# Fonction pour trouver la meilleure correspondance entre le nom du fichier et les noms des feuilles
get_best_match <- function(file_name, sheet_names) {
  normalized_file_name <- normalize_name(basename(file_name))
  normalized_sheet_names <- normalize_name(sheet_names)
  
  distances <- stringdist::stringdist(normalized_file_name, normalized_sheet_names, method = "lv")  # Jaro-Winkler distance
  best_match_index <- which.min(distances)
  print(distances[best_match_index])
  print(sheet_names[best_match_index])
  if (distances[best_match_index] > 25) {  # Un seuil pour la similarité
    return(sheet_names[best_match_index])
  } else {
    return(NULL)  # Aucun match trouvé
  }
}

# Lire et remplir les données CSV dans les feuilles Excel correspondantes
if (length(csv_files) > 0) {
  for (csv_file in csv_files) {
    # Lire les données CSV
    data <- read.csv(csv_file, fileEncoding = "ISO-8859-1", skipNul = TRUE, row.names = NULL, sep = "\t", header = TRUE, skip = 3)
    data <- data %>%
      select(Tiempo, Razón, Motor, `Velocidad..Kph.`, `Kilometraje..Km..`,Dirección)
    
    # Convertir les dates
    data$Tiempo <- as.POSIXct(data$Tiempo, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
    
    # Identifier la feuille Excel correspondante
    sheet_name <- get_best_match(csv_file, sheets)
    
    if (!is.null(sheet_name)) {
      # Harmoniser les colonnes
      names(data) <- names(sheets_data[[sheet_name]])
      
      # Ajouter les données à la feuille correspondante
      sheets_data[[sheet_name]] <- bind_rows(sheets_data[[sheet_name]], data)
    } else {
      print(paste("Aucune feuille correspondant à", basename(csv_file)))
    }
  }
  # Écrire les données mises à jour dans le fichier Excel
  wb <- loadWorkbook(file_path)
  for (sheet_name in names(sheets_data)) {
    if (!sheet_name %in% names(wb)) {
      addWorksheet(wb, sheet_name)
    }
    writeData(wb, sheet = sheet_name, sheets_data[[sheet_name]])
  }
  saveWorkbook(wb, file_path, overwrite = TRUE)
} else {
  print("Aucun fichier CSV trouvé.")
}

# Supprimer les fichiers ZIP dans le dossier de téléchargement
file.remove(zip_files)

# Supprimer les fichiers CSV dans le dossier d'extraction
csv_files_extracted <- list.files(extract_folder, pattern = "\\.csv$", full.names = TRUE)
file.remove(csv_files_extracted)

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
    
    # Fonction pour regrouper les points GPS proches avec HDBSCAN
    group_points_hdbscan <- function(chunk, minPts = 20) {
      coords <- as.matrix(chunk[, c("longitude", "latitude")])
      
      # Appliquer HDBSCAN
      clustering <- hdbscan(coords, minPts = minPts)
      chunk$group <- as.factor(clustering$cluster)
      
      return(chunk)
    }
    
    # Regrouper les points GPS proches
    chunk <- group_points_hdbscan(chunk)
    
    chunk <- chunk %>%
      select(Tiempo, Motor, local_name, longitude, latitude, distance, time_diff, speed = `Velocidad (Kph)`, is_stop, group)
    
    return(chunk)
  }
  
  # Traiter chaque feuille individuellement
  processed_sheets <- lapply(sheets, function(sheet_name) {
    tryCatch({
      data <- sheets_data[[sheet_name]]
      
      # Convertir la colonne de temps en format datetime
      data$Tiempo <- as.POSIXct(data$Tiempo, format="%Y-%m-%d %H:%M:%S")
      
      # Filtrer les données pour les 15 derniers jours avant le traitement
      cutoff_date <- as.POSIXct(Sys.Date() - 15, format="%Y-%m-%d")
      data <- filter(data, Tiempo >= cutoff_date)
      
      total_rows <- nrow(data)
      
      # Processer les données en morceaux
      results <- list()
      for (start_row in seq(1, total_rows, by = chunk_size)) {
        end_row <- min(start_row + chunk_size - 1, total_rows)
        chunk_result <- process_sheet_chunk(data, start_row, end_row)
        results <- append(results, list(chunk_result))
      }
      
      # Combiner les résultats des morceaux
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

output_path <- here("datasets", "Las horas de laborales", "Processed_Historial.xlsx")
processed_data <- process_sheets_in_chunks(sheets_data,sheets, output_path)


    
  