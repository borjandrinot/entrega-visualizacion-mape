
library(tidyverse)
library(readxl)
library(janitor)
library(jsonlite)

data_raw <- read_excel("data_raw/04_201905_1.xlsx")
readxl::excel_sheets("data_raw/04_201905_1.xlsx")

x <- read_excel("data_raw/04_201905_1.xlsx",
                "Municipios(250)")

data <- 
  data_raw %>% 
  clean_names()


names_cols <- 
  data %>% 
  slice(3)

partidos <- 
  names_cols %>% 
  select(-1:-14) %>% 
  pivot_longer(everything(), 
               names_to = "partido_id", 
               values_to = "partido") 

data_2 <- 
  data %>% 
  slice(-1:-3) %>% 
  rename(especial = 1, ccaa = 2, codigo_prov = 3, prov = 4, codigo_mun = 5,
         mun = 6, pob = 7, mesas = 8, censo_total = 9, votantes = 10, 
         validos = 11, candidaturas = 12, blanco = 13, nulos = 14)

data_3 <- 
  data_2 %>% 
  mutate(especial = !is.na(especial), 
         across(c(pob:nulos), as.numeric)) %>% 
  pivot_longer(-especial:-nulos, 
               names_to = "partido_id", 
               values_to = "votos") %>% 
  mutate(votos = as.numeric(votos)) %>% 
  left_join(partidos) %>% 
  mutate(votos_pc = votos / validos) %>%
  filter(votos_pc > 0.1)

partidos_dict <- 
  data_3 %>%
  group_by(partido) %>% 
  summarise(votos = sum(votos), 
            .groups = "drop") %>% 
  mutate(partido = str_remove_all(partido, "\\\""),
         partido = fct_reorder(partido, votos, .desc = T), 
         id = as.numeric(partido)) %>% 
  select(partido, id)

partidos_to_json <- 
  partidos_dict %>% 
  arrange(partido) %>% 
  pull(partido)

datos_to_json <- 
  data_3 %>% 
  mutate(codigo_mun = str_glue("{str_pad(codigo_prov, 2, 'left', '0')}{str_pad(codigo_mun, 3, 'left', '0')}")) %>% 
  left_join(partidos_dict) %>% 
  mutate(votos_pc = round(100 * votos_pc, 1)) %>% 
  group_by(codigo_mun) %>% 
  arrange(desc(votos)) %>% 
  summarise(p = list(id), 
            v = list(votos), 
            vc = list(votos_pc)) %>% 
  nest(-codigo_mun)

json <- list(partidos = partidos_to_json, 
             `2019` = setNames(as.list(datos_to_json$data), datos_to_json$codigo_mun))

json$`2019` <- map(json$`2019`, unbox)

# json <- map(json, unbox)
json <- 
  json %>% 
  toJSON(pretty = F, auto_unbox = F)

write_json(json, "data/map_data_raw.json")

system(paste("sed 's/\\\\//g'",
             "data/map_data_raw.json",
             "| sed 's/\\[\"{/\\[{/g' | sed 's/}\"\\]/}\\]/g' | sed 's/^\\[\"/\\[/g' | sed 's/\"\\]$/\\]/g' >",
             "data/map_data.json"))
