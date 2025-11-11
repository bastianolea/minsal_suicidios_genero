library(dplyr)
library(tidyr)
library(arrow)
library(readxl)
library(janitor)
library(stringr)
source("funciones.R")

# códigos de egreso ----
codigos_0 <- read_xlsx("datos/minsal_egresos/Diccionario BD egresos hospitalario.xlsx", sheet = 2, skip = 8) |> 
  clean_names()

codigos_1 <- codigos_0 |> 
  mutate(codigo_subcategoria_letra = str_extract(codigo_subcategoria, "."),
         codigo_subcategoria_numero = str_extract(codigo_subcategoria, "\\d+") |> as.numeric())

codigos_suicidio <- codigos_1 |> 
  filter(codigo_subcategoria_letra == "X",
         codigo_subcategoria_numero >= 600 & codigo_subcategoria_numero <= 849)
         
codigos_suicidio$codigo_subcategoria


# egresos ----
# https://deis.minsal.cl/#datosabiertos

# cargar
egresos_0 <- read_delim_arrow("datos/minsal_egresos/EGRESOS_2023.csv",
                            delim = ";") |> 
  clean_names()

# limpiar
egresos_1 <- egresos_0 |> 
  rename(codigo_comuna = comuna_residencia,
         año = ano_egreso) |> 
  # sacar datos perdidos
  filter(!is.na(codigo_comuna)) |> 
  filter(!is.na(sexo)) |> 
  mutate(genero = case_match(sexo,
                             "HOMBRE" ~ "masculino",
                             "MUJER" ~ "femenino")) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  mutate(condicion_egreso = case_match(condicion_egreso,
                                       1 ~ "intento",
                                       2 ~ "consumado"))
  
egresos_1 |> glimpse()

# filtrar suicidios
egresos_suicidio <- egresos_1 |> 
  filter(año == 2023) |> 
  # filter(condicion_egreso == 1) |>
  filter(diag2 %in% codigos_suicidio$codigo_subcategoria)
  
# conteo ----
# egresos_suicidio |> 
#   group_by(codigo_comuna, genero) |> 
#   count()

egresos_suicidio_2 <- egresos_suicidio |>
  group_by(codigo_comuna, genero, condicion_egreso) |>
  count(name = "valor") |> 
  ungroup()

# comunas ----
comunas <- cargar_comunas()

egresos_suicidio_3 <- egresos_suicidio_2 |> 
  left_join(comunas, join_by(codigo_comuna)) |> 
  ordenar_comunas() |> 
  filter(!is.na(comuna))

# egresos_suicidio_3 |> 
#   group_by(condicion_egreso) |> 
#   summarize(mean(valor))

egresos_suicidio_3 |>
  filter(condicion_egreso == "consumado") |> 
  summarize(n_distinct(comuna))


# pivotar a ancho por género
egresos_suicidio_4 <- egresos_suicidio_3 |>
  pivot_wider(names_from = genero, values_from = valor, values_fill = 0) |>
  mutate(variable = paste0("minsal_suicidios_", condicion_egreso)) |> 
  select(-condicion_egreso)

# egresos_suicidio_4 |> 
#   filter(variable != "minsal_suicidios_consumado")


# guardar ----
guardar_pieza(egresos_suicidio_4, "minsal_suicidios.csv")
