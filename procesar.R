library(dplyr)
library(tidyr)
library(arrow)
library(readxl)
library(janitor)
library(stringr)

# códigos de egreso ----
# a partir del diccionario de egresos, obtener códigos de egreso 
# que corresponden a casos de suicidio; en general lesiones autoinfligidas

codigos_0 <- read_xlsx("datos/Diccionario BD egresos hospitalario.xlsx", sheet = 2, skip = 8) |> 
  clean_names()

# extraer letra y número de los códigos
codigos_1 <- codigos_0 |> 
  mutate(codigo_subcategoria_letra = str_extract(codigo_subcategoria, "."),
         codigo_subcategoria_numero = str_extract(codigo_subcategoria, "\\d+") |> as.numeric())

# filtrar códigos de egreso en base a letra y números
codigos_suicidio <- codigos_1 |> 
  filter(codigo_subcategoria_letra == "X",
         codigo_subcategoria_numero >= 600 & codigo_subcategoria_numero <= 849)

message(nrow(codigos_suicidio), " códigos de suicidio obtenidos")


# cargar datos ----
# datos descargados desde https://deis.minsal.cl/#datosabiertos
# los archivos csv se cargan con {arrow} para mayor velocidad

## 2024 ----
egresos_2024 <- read_delim_arrow("datos/EGRESOS_2024.csv", delim = ";")

egresos_2024 <- egresos_2024 |> 
  clean_names() |> 
  select(codigo_comuna = comuna_residencia, año = ano_egreso, sexo, condicion_egreso, diag2) |> 
  mutate(across(c(codigo_comuna, año), as.numeric))

## 2023 ----
egresos_2023 <- read_delim_arrow("datos/EGRESOS_2023.csv",
                                 delim = ";")

egresos_2023 <- egresos_2023 |> 
  clean_names() |> 
  select(codigo_comuna = comuna_residencia, año = ano_egreso, sexo, condicion_egreso, diag2) |> 
  mutate(across(c(codigo_comuna, año), as.numeric))


## 2022 ----
# egresos_2022 <- read_delim_arrow("datos/EGRE_DATOS_ABIERTOS_2022.csv",
#                                  delim = ";",
#                                  col_select = c(COMUNA_RESIDENCIA, ANO_EGRESO, SEXO, CONDICION_EGRESO, DIAG2))
# este archivo da problemas, así que se carga distinto
egresos_2022 <- read.csv2("datos/EGRE_DATOS_ABIERTOS_2022.csv")

egresos_2022 <- egresos_2022 |> 
  clean_names() |> 
  select(comuna_residencia, ano_egreso, sexo, condicion_egreso, diag2) |> 
  mutate(comuna_residencia = as.numeric(comuna_residencia),
         ano_egreso = as.numeric(ano_egreso)) |> 
  rename(año = ano_egreso,
         codigo_comuna = comuna_residencia)

## 2021 ----
egresos_2021 <- read_delim_arrow("datos/EGR_DATOS_ABIERTO_2021.csv",
                                 delim = ";")

egresos_2021 <- egresos_2021 |> 
  clean_names() |> 
  select(codigo_comuna = comuna_residencia, año = ano_egreso, sexo, condicion_egreso, diag2) |> 
  mutate(across(c(codigo_comuna, año), as.numeric)) |> 
  mutate(sexo = as.character(sexo))



# unir datos ----
egresos_0 <- bind_rows(egresos_2024,
                       egresos_2023,
                       egresos_2022,
                       egresos_2021)

# filtrar suicidios ----
# priorizar acciones que reduzcan cantidad de filas
egresos_1 <- egresos_0 |> 
  filter(diag2 %in% codigos_suicidio$codigo_subcategoria)

# limpiar datos ----
egresos_1a <- egresos_1 |> 
  # sacar datos perdidos
  filter(!is.na(codigo_comuna)) |> 
  filter(!is.na(sexo))

egresos_1b <- egresos_1a |> 
  # recodificar sexo
  mutate(genero = case_match(sexo,
                             "HOMBRE" ~ "masculino",
                             "MUJER" ~ "femenino",
                             "2" ~ "femenino",
                             "1" ~ "masculino"))

egresos_1c <- egresos_1b |> 
  mutate(condicion_egreso = case_match(condicion_egreso,
                                       1 ~ "intento",
                                       2 ~ "consumado"))

egresos_1d <- egresos_1c |> 
  filter(!is.na(genero)) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna))

# revisiones
# egresos_1 |> distinct(sexo, genero)

# # revisar que hayan en todos los años
# egresos_suicidio |>
#   count(año, genero)

egresos_2 <- egresos_1d

# conteo ----
# egresos_suicidio |> 
#   group_by(codigo_comuna, genero) |> 
#   count()

egresos_3 <- egresos_2 |>
  group_by(codigo_comuna, año, genero, condicion_egreso) |>
  count(name = "valor") |> 
  ungroup()

# comunas ----
comunas <- readRDS("datos/cut_comunas.rds")

# # pivotar a ancho por género
# egresos_4 <- egresos_3 |>
#   pivot_wider(names_from = genero, values_from = valor, values_fill = 0) |>
#   mutate(variable = paste0("minsal_suicidios_", condicion_egreso)) |> 
#   select(-condicion_egreso)
# 
# # crear dataframe con todas las comunas por cada variable
# comunas_2 <- bind_rows(comunas |> mutate(variable = "minsal_suicidios_consumado"),
#                        comunas |> mutate(variable = "minsal_suicidios_intento"))
# 
# # agregar datos a las comunas
# egresos_5 <- comunas_2 |> 
#   left_join(egresos_4,
#             join_by(codigo_comuna, variable)) |> 
#   ordenar_comunas()

egresos_4 <- egresos_3 |> 
  left_join(comunas, join_by(codigo_comuna)) |> 
  filter(!is.na(comuna)) |> 
  relocate(codigo_comuna, .before = comuna)

egresos_5 <- egresos_4 |> 
  arrange(año, genero, codigo_comuna, condicion_egreso) |> 
  rename(condicion = condicion_egreso)

# # rellenar con ceros
# egresos_6 <- egresos_5 |> 
#   mutate(across(c(femenino, masculino), ~replace_na(.x, 0)))

# guardar ----
write_parquet(egresos_5, "datos/minsal_suicidios.parquet")
              