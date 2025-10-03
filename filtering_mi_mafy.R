library(tidyverse)
library(sf)
library(here)
library(mapview)

load("data/krw/mi_data.rdata")

wlas <- st_read("data/wlas/Wlas.shp")
ht3260_lijnen <- st_read(here("data", "habitat_3260", "habitat_3260.shp")) %>%
  st_transform(crs = st_crs(wlas))
st_crs(wlas) == st_crs(ht3260_lijnen)

ht3260_lijnen_vhag <- st_join(
  x = ht3260_lijnen,
  y = wlas %>% select(VHAG, VHAS),
  join = st_nearest_feature, # Koppelt aan de dichtstbijzijnde feature
  left = TRUE
)

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
  st_transform(., crs = st_crs(wlas)) %>%
  filter(measured == 1)

ht3260_header_vhag <- st_join(
  x = ht3260_header,
  y = wlas %>% select(VHAG),
  join = st_nearest_feature, # Koppelt aan de dichtstbijzijnde feature
  left = TRUE
)

mi_data1 <- mi_data %>%
  filter(monsternamedatum > '2014-12-31') %>%
  select(meetplaats, vhag) %>%
  unique() %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(wlas))

# Hernoem de VHAG-kolom in mi_data1 om conflicten te vermijden en consistentie te waarborgen
mi_data1 <- mi_data1 %>% rename(VHAG_MI = vhag)

# --- CRITERIUM 1: Nabij een HT3260_header punt (200m) met dezelfde VHAG ---

# 1a: Spatial Join met afstand 200m
mi_data_met_nabije_header_vhag <- st_join(
  x = mi_data1,
  y = ht3260_header_vhag %>% select(VHAG) %>% rename(VHAG_HT_PUNT = VHAG),
  join = st_is_within_distance,
  dist = 200,
  left = TRUE
)

# 1b: Filter de resultaten
criteria_1_resultaat <- mi_data_met_nabije_header_vhag %>%
  filter(!is.na(VHAG_HT_PUNT) & VHAG_MI == VHAG_HT_PUNT) %>%
  select(meetplaats, VHAG_MI) %>%
  unique()

# --------------------------------------------------------------------------------

# --- CRITERIUM 2: Op een HT3260_lijnen_vhag traject (10m) met dezelfde VHAG ---

# 2a: Spatial Join met afstand 10m
mi_data_met_nabije_lijn_10m_vhag <- st_join(
  x = mi_data1,
  y = ht3260_lijnen_vhag %>% select(VHAG) %>% rename(VHAG_HT_LIJN = VHAG),
  join = st_is_within_distance,
  dist = 10,
  left = TRUE
)

# 2b: Filter de resultaten
criteria_2_resultaat <- mi_data_met_nabije_lijn_10m_vhag %>%
  filter(!is.na(VHAG_HT_LIJN) & VHAG_MI == VHAG_HT_LIJN) %>%
  select(meetplaats, VHAG_MI) %>%
  unique()

# --------------------------------------------------------------------------------

## Nieuw Criterium 3: Nabij een HT3260_lijnen_vhag traject (200m) met dezelfde VHAG

# 3a: Spatial Join met afstand 200m
mi_data_met_nabije_lijn_200m_vhag <- st_join(
  x = mi_data1,
  y = ht3260_lijnen_vhag %>% select(VHAG) %>% rename(VHAG_HT_LIJN_200M = VHAG),
  join = st_is_within_distance,
  dist = 200,
  left = TRUE
)

# 3b: Filter de resultaten.
# Selecteer punten binnen 200m van een lijn EN met dezelfde VHAG.
criteria_3_resultaat <- mi_data_met_nabije_lijn_200m_vhag %>%
  filter(!is.na(VHAG_HT_LIJN_200M) & VHAG_MI == VHAG_HT_LIJN_200M) %>%
  select(meetplaats, VHAG_MI) %>%
  unique()

# --------------------------------------------------------------------------------

## Finale Stap: De Drie Resultaten Combineren

# Gebruik rbind om de unieke punten uit de drie criteria samen te voegen en unieke geometrieën te behouden.
mi_data_finaal_gefilterd <- rbind(
  criteria_1_resultaat,
  criteria_2_resultaat,
  criteria_3_resultaat
) %>%
  unique()

mapview(mi_data_finaal_gefilterd, col.region = "red") + mapview(ht3260_lijnen_vhag) + mapview(ht3260_header_vhag)


###########################################################
# macrofyten
############################################################

load("data/krw/mafy_data.rdata")

mafy_data0 <- mafy_data %>%
  filter(monsternamedatum > '2014-12-31') %>%
  select(meetplaats) %>%
  unique() %>%
  st_as_sf() %>%
  st_transform(., crs = st_crs(wlas))

mafy_data1 <- st_join(
  x = mafy_data0,
  y = wlas %>% select(VHAG),
  join = st_nearest_feature, # Koppelt aan de dichtstbijzijnde feature
  left = TRUE
)


# Hernoem de VHAG-kolom in mafy_data1 om conflicten te vermijden en consistentie te waarborgen
mafy_data1 <- mafy_data1 %>% rename(VHAG_MAFY = VHAG)

# --- CRITERIUM 1: Nabij een HT3260_header punt (200m) met dezelfde VHAG ---

# 1a: Spatial Join met afstand 200m
mafy_data_met_nabije_header_vhag <- st_join(
  x = mafy_data1,
  y = ht3260_header_vhag %>% select(VHAG) %>% rename(VHAG_HT_PUNT = VHAG),
  join = st_is_within_distance,
  dist = 200,
  left = TRUE
)

# 1b: Filter de resultaten
criteria_1_resultaat <- mafy_data_met_nabije_header_vhag %>%
  filter(!is.na(VHAG_HT_PUNT) & VHAG_MAFY == VHAG_HT_PUNT) %>%
  select(meetplaats, VHAG_MAFY) %>%
  unique()

# --------------------------------------------------------------------------------

# --- CRITERIUM 2: Op een HT3260_lijnen_vhag traject (10m) met dezelfde VHAG ---

# 2a: Spatial Join met afstand 10m
mafy_data_met_nabije_lijn_10m_vhag <- st_join(
  x = mafy_data1,
  y = ht3260_lijnen_vhag %>% select(VHAG) %>% rename(VHAG_HT_LIJN = VHAG),
  join = st_is_within_distance,
  dist = 10,
  left = TRUE
)

# 2b: Filter de resultaten
criteria_2_resultaat <- mafy_data_met_nabije_lijn_10m_vhag %>%
  filter(!is.na(VHAG_HT_LIJN) & VHAG_MAFY == VHAG_HT_LIJN) %>%
  select(meetplaats, VHAG_MAFY) %>%
  unique()

# --------------------------------------------------------------------------------

## Nieuw Criterium 3: Nabij een HT3260_lijnen_vhag traject (200m) met dezelfde VHAG

# 3a: Spatial Join met afstand 200m
mafy_data_met_nabije_lijn_200m_vhag <- st_join(
  x = mafy_data1,
  y = ht3260_lijnen_vhag %>% select(VHAG) %>% rename(VHAG_HT_LIJN_200M = VHAG),
  join = st_is_within_distance,
  dist = 200,
  left = TRUE
)

# 3b: Filter de resultaten.
# Selecteer punten binnen 200m van een lijn EN met dezelfde VHAG.
criteria_3_resultaat <- mafy_data_met_nabije_lijn_200m_vhag %>%
  filter(!is.na(VHAG_HT_LIJN_200M) & VHAG_MAFY == VHAG_HT_LIJN_200M) %>%
  select(meetplaats, VHAG_MAFY) %>%
  unique()

# --------------------------------------------------------------------------------

## Finale Stap: De Drie Resultaten Combineren

# Gebruik rbind om de unieke punten uit de drie criteria samen te voegen en unieke geometrieën te behouden.
mafy_data_finaal_gefilterd <- rbind(
  criteria_1_resultaat,
  criteria_2_resultaat,
  criteria_3_resultaat
) %>%
  unique()

mapview(mafy_data_finaal_gefilterd, col.region = "red") + mapview(ht3260_lijnen_vhag) + mapview(ht3260_header_vhag)


