library(dplyr)
library(ggplot2)
library(ggforce)
library(ggblend)

datos <- arrow::read_parquet("datos/minsal_suicidios.parquet")

conteo <- datos |> 
  summarize(valor = sum(valor), 
            .by = c(genero, condicion)) |> 
  mutate(porcentaje = valor / sum(valor),
         .by = genero) |> 
  mutate(maximo = max(valor),
         .by = genero)


# unidos ----
conteo |> 
  mutate(horizontal = case_when(
    condicion == "intento" & genero == "femenino" ~ 1-max(valor)-200,
    condicion == "intento" & genero == "masculino" ~ 1+maximo+200,
    condicion == "consumado" & genero == "femenino" ~ 1-200,
    condicion == "consumado" & genero == "masculino" ~ 1+valor+10)) |>
  mutate(vertical = case_match(
    genero,
    "femenino" ~ 1,
    "masculino" ~ 1)) |> 
  ggplot() +
  # círculos
  geom_circle(
    aes(x0 = horizontal, y0 = vertical, r = valor,
        fill = genero, color = genero),
    linewidth = .3
  ) |> blend("multiply") +
  # líneas horizontales
  geom_hline(
    aes(yintercept = valor, color = genero),
    alpha = 0.6) +
  geom_hline(
    aes(yintercept = -valor, color = genero),
    alpha = 0.6) +
  # recorte
  coord_fixed(xlim = c(-4000, 4000),
              ylim = c(-3000, 3000))

# intentos ----
conteo |> 
  filter(condicion == "intento") |>
  mutate(horizontal = case_match(genero,
                                 "femenino" ~ 1,
                                 "masculino" ~ max(valor))) |>
  mutate(vertical = case_match(genero,
                               "femenino" ~ 1,
                               "masculino" ~ 1)) |> 
  ggplot() +
  geom_circle(
    aes(x0 = horizontal, y0 = vertical, 
        r = valor,
        fill = genero, color = genero),
    linewidth = .3, alpha = 0.9
  ) |> blend("multiply") +
  coord_fixed()


# consumados ----
conteo |> 
  filter(condicion == "consumado") |>
  mutate(horizontal = case_match(genero,
                                 "femenino" ~ 1,
                                 "masculino" ~ max(valor))) |>
  mutate(vertical = case_match(genero,
                               "femenino" ~ 1,
                               "masculino" ~ 1)) |> 
  ggplot() +
  geom_circle(
    aes(x0 = horizontal, y0 = vertical, 
        r = valor,
        fill = genero, color = genero),
    linewidth = .3, alpha = 0.9
  ) |> blend("multiply") +
  coord_fixed()

