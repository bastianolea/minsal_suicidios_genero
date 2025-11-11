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

# codigos_suicidio$codigo_subcategoria

# codigos_suicidio |> distinct(glosa_categoria) |> print(n=Inf)



# cargar ----
egresos_2023 <- read_delim_arrow("datos/minsal_egresos/EGRESOS_2023.csv",
                                 delim = ";") |> 
  clean_names() |> 
  select(codigo_comuna = comuna_residencia, año = ano_egreso, sexo, condicion_egreso, diag2) |> 
  mutate(across(c(codigo_comuna, año), as.numeric))

egresos_2024 <- read_delim_arrow("datos/minsal_egresos/EGRESOS_2024.csv",
                                 delim = ";") |> 
  clean_names() |> 
  select(codigo_comuna = comuna_residencia, año = ano_egreso, sexo, condicion_egreso, diag2) |> 
  mutate(across(c(codigo_comuna, año), as.numeric))

egresos_2022 <- read_delim_arrow("datos/minsal_egresos/EGRE_DATOS_ABIERTOS_2022.csv",
                                 delim = ";",
                                 col_select = c(COMUNA_RESIDENCIA, ANO_EGRESO, SEXO, CONDICION_EGRESO, DIAG2)) |>
  clean_names()
  
  # rename(codigo_comuna = comuna_residencia, año = ano_egreso) |> 
  # mutate(across(c(codigo_comuna, año), as.numeric))
  # mutate(across(c(comuna_residencia, ano_egreso), as.numeric))
egresos_2022 <- egresos_2022 |> 
  mutate(comuna_residencia = as.numeric(comuna_residencia),
         ano_egreso = as.numeric(ano_egreso)) |> 
  rename(año = ano_egreso,
         codigo_comuna = comuna_residencia)

egresos_2021 <- read_delim_arrow("datos/minsal_egresos/EGR_DATOS_ABIERTO_2021.csv",
                                 delim = ";") |> 
  clean_names() |> 
  select(codigo_comuna = comuna_residencia, año = ano_egreso, sexo, condicion_egreso, diag2) |> 
  mutate(across(c(codigo_comuna, año), as.numeric)) |> 
  mutate(sexo = as.character(sexo))

# unir ----
egresos_0 <- bind_rows(egresos_2024,
                       egresos_2023,
                       egresos_2022,
                       egresos_2021)

# limpiar ----
egresos_1 <- egresos_0 |> 
  # sacar datos perdidos
  filter(!is.na(codigo_comuna)) |> 
  filter(!is.na(sexo)) |> 
  mutate(genero = case_match(sexo,
                             "HOMBRE" ~ "masculino",
                             "MUJER" ~ "femenino",
                             "2" ~ "femenino",
                             "1" ~ "masculino")) |> 
  filter(!is.na(genero)) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  mutate(condicion_egreso = case_match(condicion_egreso,
                                       1 ~ "intento",
                                       2 ~ "consumado"))

# egresos_1 |> distinct(sexo, genero)

# filtrar suicidios
egresos_suicidio <- egresos_1 |> 
  # filter(condicion_egreso == 1) |>
  filter(diag2 %in% codigos_suicidio$codigo_subcategoria)

# # revisar que hayan en todos los años
# egresos_suicidio |>
#   count(año, genero)

# de aquí para abajo es copiado del otro script


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
# 
# egresos_suicidio_3 <- egresos_suicidio_2 |> 
#   left_join(comunas, join_by(codigo_comuna)) |> 
#   ordenar_comunas() |> 
#   filter(!is.na(comuna))
# 


# egresos_suicidio_3 |>
#   group_by(condicion_egreso) |>
#   summarize(mean(valor))
# 
# egresos_suicidio_3 |>
#   filter(condicion_egreso == "consumado") |> 
#   summarize(n_distinct(comuna))

# pivotar a ancho por género
egresos_suicidio_3 <- egresos_suicidio_2 |>
  pivot_wider(names_from = genero, values_from = valor, values_fill = 0) |>
  mutate(variable = paste0("minsal_suicidios_", condicion_egreso)) |> 
  select(-condicion_egreso)

# crear dataframe con todas las comunas por cada variable
comunas_2 <- bind_rows(comunas |> mutate(variable = "minsal_suicidios_consumado"),
                       comunas |> mutate(variable = "minsal_suicidios_intento"))

# agregar datos a las comunas
egresos_suicidio_4 <- comunas_2 |> 
  left_join(egresos_suicidio_3,
            join_by(codigo_comuna, variable)) |> 
  ordenar_comunas()

# rellenar con ceros
egresos_suicidio_5 <- egresos_suicidio_4 |> 
  mutate(across(c(femenino, masculino), ~replace_na(.x, 0)))

# egresos_suicidio_4 |> 
#   filter(variable != "minsal_suicidios_consumado")

# revisar ----
# # casos totales
# egresos_suicidio_5 |> 
#   group_by(variable) |> 
#   summarize(femenino = sum(femenino),
#             masculino = sum(masculino))
# 
# # comunas con mayoría
# egresos_suicidio_5 |> 
#   # mutate(mayor = ifelse(femenino > masculino, "mayoría_femenino", "mayoría_masculino")) |> 
#   mutate(mayor = case_when(femenino > masculino ~ "mayoría_femenino", 
#                            femenino < masculino ~ "mayoría_masculino",
#                            .default = "igualdad")) |> 
#   count(variable, mayor) |> 
#   pivot_wider(names_from = mayor, values_from = n, names_prefix = "comunas_")

# n_distinct(egresos_suicidio_4$comuna)

# mínimo de observaciones
# egresos_suicidio_6 <- egresos_suicidio_5 |> 
  # filter(femenino + masculino >= 3)

# cargar censo
censo <- cargar_censo() |> 
  select(codigo_comuna, poblacion)

# sólo considerar comunas donde el total de personas
# sea igual o mayor a
egresos_suicidio_6 <- egresos_suicidio_5 |> 
  left_join(censo) |> 
  mutate(total = femenino + masculino,
         tasa = (total / poblacion) * 100000) |> 
  arrange(total) |> 
  filter(variable == "minsal_suicidios_intento" & tasa >= 10 |
           variable == "minsal_suicidios_consumado" & tasa >= 1) |> 
  # filter(variable == "minsal_suicidios_intento" & total >= 3 |
           # variable == "minsal_suicidios_consumado" & total >= 2) |> 
  filter(total > 1) |> 
  # limpiar
  select(-poblacion, -total, -tasa)

egresos_suicidio_6 |> count(variable)

egresos_suicidio_6 |> 
  filter(variable == "minsal_suicidios_consumado") |> 
  arrange(masculino + femenino)


# guardar ----
guardar_pieza(egresos_suicidio_6, "minsal_suicidios.csv")

