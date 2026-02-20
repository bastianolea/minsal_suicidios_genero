library(dplyr)
library(ggplot2)
library(ggforce)
library(ggblend)
library(scales)

# datos ----
datos <- arrow::read_parquet("datos/minsal_suicidios.parquet")

conteo <- datos |> 
  summarize(valor = sum(valor), 
            .by = c(genero, condicion)) |> 
  mutate(porcentaje = valor / sum(valor),
         .by = genero) |> 
  mutate(maximo = max(valor),
         .by = genero)


# colores ----
color_base <- "#CA1F7B"

color <- list(femenino = color_base,
              masculino = color_base |> col_darker(5) |> col_saturate(-30) |> col_shift(-30),
              fondo = color_base |> col_darker(38) |> col_saturate(-45),
              texto = color_base |> col_lighter(35) |> col_saturate(-50),
              detalle = color_base |> col_darker(20) |> col_saturate(-40),
              tenue = color_base |> col_darker(30) |> col_saturate(-45))

# previsualizar paleta
# show_col(unlist(color))


# temas ----

# tema
theme_set(
  theme_minimal(ink = color$texto,
                paper = color$fondo,
                base_family = "Rubik") +
    theme(legend.position = "none") +
    theme(panel.grid = element_line(linetype = "dashed", color = color$tenue),
          panel.grid.minor = element_blank())
)

# tipografía desde google fonts
library(showtext)
font_add_google("Rubik", "Rubik")
showtext_auto()
showtext_opts(dpi = 200)

# opciones de formato de números
number_options(big.mark = ".",
               decimal.mark = ",")

# funciones de temas
escala_genero <- function() {
  scale_color_manual(values = c("femenino" = color$femenino, 
                                "masculino" = color$masculino),
                     aesthetics = c("fill", "color"))
}

tema_sin_fondo <- function() {
  theme(panel.grid = element_blank())
}

tema_sin_ejes <- \() {
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
}

# gráficos ----

## intentos ----
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
  coord_fixed() +
  escala_genero() +
  tema_sin_fondo() +
  tema_sin_ejes()


## consumados ----
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
  coord_fixed() +
  escala_genero() +
  tema_sin_fondo() +
  tema_sin_ejes()


## unidos ----
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
              ylim = c(-3000, 3000)) +
  escala_genero() +
  tema_sin_fondo() +
  tema_sin_ejes()


## años ----
# devtools::install_github("hrbrmstr/ggalt")
# library(ggalt)

datos_año <- datos |> 
  group_by(año, genero, condicion) |> 
  summarize(valor = sum(valor), .groups = "drop") |> 
  mutate(condicion = recode_values(condicion,
                                   "intento" ~ "Intentos de suicidio",
                                   "consumado" ~ "Suicidios consumados"))


grafico_lineas <- function(datos) {
  datos |> 
    ggplot() +
    aes(x = año, y = valor, color = genero) +
    scale_x_continuous(breaks = seq(min(datos$año), max(datos$año), 1),
                       expand = expansion(c(0.02, 0.1))) +
    scale_y_continuous(labels = number,
                       expand = expansion(c(0.07, 0.07))) +
    # facet_wrap(~condicion, scales = "free", ncol = 1) +
    geom_smooth(se = FALSE, linetype = "solid", linewidth = 1.5, lineend = "round") +
    geom_point(data = ~filter(.x, año == max(año)), size = 6, color = color$fondo) +
    geom_point(data = ~filter(.x, año == max(año)), size = 4) +
    geom_text(data = ~filter(.x, año == max(año)),
              aes(label = number(valor)),
              nudge_x = 0.06,
              size = 4, fontface = "bold", hjust = 0) |> 
    ggblend::copy_over(color = color$texto, alpha = 0.4) +
    theme(legend.position = "none") +
    escala_genero() +
    coord_cartesian(clip = "off") +
    labs(y = "Casos", x = NULL) +
    theme(axis.text.x = element_text(face = "bold", color = color$texto, size = 11, vjust = 0),
          axis.title.y = element_text(vjust = 1, margin = margin(r = 4))) +
    theme(axis.line = element_line(color = color$detalle, lineend = "round", linewidth = 1))
}
  
datos_año |> 
  filter(condicion == "Intentos de suicidio") |>
  grafico_lineas() +
  geom_text(data = ~filter(.x, año %in% c(2022)),
            aes(label = number(valor)),
            nudge_y = 180,
            size = 4, fontface = "bold") |> 
  ggblend::copy_over(color = color$texto, alpha = 0.4)

datos_año |> 
  filter(condicion == "Suicidios consumados") |>
  grafico_lineas() +
  scale_y_continuous(labels = number,
                     breaks = pretty_breaks(n = 8),
                     expand = expansion(c(0.07, 0.07))) +
  geom_text(data = ~filter(.x, año %in% c(2022, 2023)),
            aes(label = number(valor)),
            nudge_y = 1,
            size = 4, fontface = "bold") |> 
  ggblend::copy_over(color = color$texto, alpha = 0.4)


