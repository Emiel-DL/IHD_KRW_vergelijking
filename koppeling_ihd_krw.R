library(sf)
library(tidyverse)
library(here)
library(mapview)

# inlezen habitat 3260
ht3260_lijnen <- st_read(here("data", "habitat_3260", "habitat_3260.shp"))

ht3260_header <- read.table(file = here("data", "LSVI", "HT3260_header.tsv"), sep = '\t', header = TRUE) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  mutate(
    latitude = case_when(
      latitude < 10 ~ latitude * 10,
      latitude > 60 ~ latitude / 1e+13,
      TRUE ~ latitude
  ),
    longitude = case_when(
      longitude > 10 ~ longitude / 1e14,
      TRUE ~ longitude
    ),
    longitude = case_when(
      (longitude > 0.1) & (longitude < 1) ~ longitude * 10,
      longitude < 0.1 ~ longitude * 100,
      TRUE ~ longitude
    )) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
  st_transform(., crs = 31370) %>%
  filter(measured == 1)

# inlezen krw
mi_meetpunten <- st_read(here("data", "krw", "mi_meetpunten_datum.gpkg"))
mafy_meetpunten <- st_read(here("data", "krw", "mafy_meetpunten_datum.gpkg"))

# exploratie
summary(ht3260_header$date)

mi_meetpunten_2015_2023 <- mi_meetpunten %>%
  filter(monsternamedatum > '2014-12-31') %>%
  select(meetplaats) %>%
  unique()

mafy_meetpunten_2015_2023 <- mafy_meetpunten %>%
  filter(monsternamedatum > '2014-12-31') %>%
  select(meetplaats) %>%
  unique()

label_nabijheid <- function(punten_sf, lijnen_sf, d_op_lijn = 10, d_in_buurt = 100) {

  # Assertie: Zorg ervoor dat de CRS overeenkomt
  if (st_crs(punten_sf) != st_crs(lijnen_sf)) {
    stop("CRS-mismatch: Beide sf-objecten moeten dezelfde CRS hebben (bij voorkeur in meters).")
  }

  # --- 1. Bepaal welke punten OP de lijn liggen (binnen d_op_lijn meters) ---

  indices_op_lijn <- st_is_within_distance(punten_sf, lijnen_sf, dist = d_op_lijn)
  logisch_op_lijn <- sapply(indices_op_lijn, function(x) length(x) > 0)

  # --- 2. Bepaal welke punten IN DE BUURT liggen (binnen d_in_buurt meters) ---

  indices_in_buurt <- st_is_within_distance(punten_sf, lijnen_sf, dist = d_in_buurt)
  logisch_in_buurt <- sapply(indices_in_buurt, function(x) length(x) > 0)

  # --- 3. Creëer de definitieve hiërarchische statuskolom ---

  punten_gelabeld <- punten_sf %>%
    mutate(
      status = case_when(
        # 1. Hoogste prioriteit: Op de waterloop (binnen 25m)
        logisch_op_lijn == TRUE          ~ "op_waterloop",

        # 2. Tweede prioriteit: In de buurt (binnen 100m, maar niet binnen 25m)
        (logisch_op_lijn == FALSE) & (logisch_in_buurt == TRUE) ~ "in_de_buurt",

        # 3. Laagste prioriteit: Verder weg
        TRUE                           ~ "verder_weg"
      )
    )

  return(punten_gelabeld)
}

mi_meetpunten_label <- label_nabijheid(mi_meetpunten_2015_2023, ht3260_lijnen)
mafy_meetpunten_label <- label_nabijheid(mafy_meetpunten_2015_2023, ht3260_lijnen)

# visualisatie
mi_punten_op_dicht <- mi_meetpunten_label %>%
  filter(status %in% c("op_waterloop", "in_de_buurt"))
mafy_punten_op_dicht <- mafy_meetpunten_label %>%
  filter(status %in% c("op_waterloop", "in_de_buurt"))
plot <- mapview(ht3260_lijnen) + mapview(mi_punten_op_dicht, zcol = "status", col.regions = c("red", "darkred")) +
  mapview(mafy_punten_op_dicht, zcol = "status", col.regions = c("darkgreen", "green"))

plot %>% mapshot2(url = here("output", "kaartje_mi_mafy_ht3260.html"))
#####
# kaartjes per jaar met ht3260 punten, mi en mafy punten
#####

library(sf)
library(dplyr)
# library(mapview) # Mapview is al geladen

# --- 1. Data Voorbereiding (Herhaald uit eerdere context voor volledigheid) ---

# Zorg ervoor dat de datumvelden correct zijn geconverteerd
mi_meetpunten$jaar <- as.integer(format(mi_meetpunten$monsternamedatum, "%Y"))
mafy_meetpunten$jaar <- as.integer(format(mafy_meetpunten$monsternamedatum, "%Y"))
ht3260_header$jaar <- as.integer(format(ht3260_header$date, "%Y"))

unieke_jaren <- sort(unique(ht3260_header$jaar))

# Kleuren definiëren voor de drie puntentypes
kleuren_lijst <- list(
  ht3260_header = "purple",  # HT3260 punten
  mi = "red",                # MI punten
  mafy = "green"              # MAFY punten
)

# De label_nabijheid functie moet beschikbaar zijn in de omgeving
# (Aangenomen dat deze functie uit de vorige stappen nog actief is)

# --- 2. Functie voor Jaarlijkse Kaartgeneratie (Zonder mapshot) ---

maak_jaarlijkse_kaart <- function(jaar_i, lijnen_sf) {

  # --- Filter de data voor het huidige jaar ---

  # 1. HT3260 Header Punten
  punten_ht3260_jaar <- ht3260_header %>% filter(jaar == jaar_i)

  # 2. MI Punten (filteren en nabijheid labelen)
  punten_mi_jaar <- mi_meetpunten %>%
    filter(jaar == jaar_i) %>%
    label_nabijheid(lijnen_sf, d_op_lijn = 10, d_in_buurt = 100) %>%
    filter(status %in% c("op_waterloop", "in_de_buurt"))

  # 3. MAFY Punten
  punten_mafy_jaar <- mafy_meetpunten %>%
    filter(jaar == jaar_i) %>%
    label_nabijheid(lijnen_sf, d_op_lijn = 10, d_in_buurt = 100) %>%
    filter(status %in% c("op_waterloop", "in_de_buurt"))


  # --- Creëer de mapview kaart ---

  # Start met de basislaag: HT3260 Lijnen
  kaart <- mapview(lijnen_sf,
                   color = "black",
                   lwd = 1,
                   layer.name = paste("HT3260 Lijnen"))

  # Voeg HT3260 Header Punten toe
  if (nrow(punten_ht3260_jaar) > 0) {
    kaart <- kaart + mapview(punten_ht3260_jaar,
                             col.regions = kleuren_lijst$ht3260_header,
                             # cex = 3,
                             legend = TRUE,
                             layer.name = paste("HT3260 Punten", jaar_i))
  }

  # Voeg MI Punten toe
  if (nrow(punten_mi_jaar) > 0) {
    kaart <- kaart + mapview(punten_mi_jaar,
                             col.regions = kleuren_lijst$mi,
                             legend = TRUE,
                             layer.name = paste("MI Punten", jaar_i))
  }

  # Voeg MAFY Punten toe
  if (nrow(punten_mafy_jaar) > 0) {
    kaart <- kaart + mapview(punten_mafy_jaar,
                             col.regions = kleuren_lijst$mafy,
                             legend = TRUE,
                             layer.name = paste("MAFY Punten", jaar_i))
  }

  # Voeg een titel toe (wordt zichtbaar in de kaartlaagkiezer)
  kaart@map$options$title <- htmltools::tags$h3(paste("Jaar:", jaar_i))

  return(kaart) # Retourneert het mapview object
}


# --- 3. Itereren over alle unieke jaren en de lijst creëren ---

# We gebruiken een anonieme functie in lapply om 'ht3260_lijnen' door te geven
alle_kaarten <- lapply(unieke_jaren, function(j) {
  maak_jaarlijkse_kaart(j, ht3260_lijnen)
})

# De lijst 'alle_kaarten' bevat nu alle mapview objecten.
print(paste("Lijst van kaarten gecreëerd:", length(alle_kaarten), "kaarten."))

# Als je een van de kaarten wilt bekijken, roep je deze aan via de index:
print(alle_kaarten[[1]]) # Toon de kaart voor het eerste jaar
print(alle_kaarten[[2]])

#####
# opslaan
######

# --- 1. Definieer de output map en controleer op bestaan ---
output_dir <- here("output", "kaartjes_jaren")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat("Output map 'output' is aangemaakt.\n")
}

# --- 2. Itereer over de kaarten en sla elke kaart op ---

# Gebruik een for-lus om de index (i) en het jaar te koppelen
for (i in seq_along(alle_kaarten)) {

  kaart_object <- alle_kaarten[[i]]
  jaar <- unieke_jaren[i]

  # Genereer de bestandsnaam
  bestandsnaam <- here(output_dir, paste0("kaartje_", jaar, ".html"))

  cat(paste("Bezig met opslaan:", bestandsnaam, "\n"))

  # Sla de kaart op. remove_controls = FALSE zorgt voor een schone export.
  mapshot2(kaart_object,
           url = bestandsnaam)
}

cat("\nAlle jaarlijkse kaarten zijn succesvol opgeslagen in de 'output' map.\n")
