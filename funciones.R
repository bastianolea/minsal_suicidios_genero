# funciones de temas
escala_genero <- function() {
  scale_color_manual(values = c("femenino" = color$femenino, 
                                "masculino" = color$masculino),
                     aesthetics = c("fill", "color"))
}

tema_sin_fondo <- function() {
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = color$fondo, linewidth = 0),
        plot.background = element_rect(fill = color$fondo, linewidth = 0))
}

tema_sin_ejes <- \() {
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0))
}

grafico_circulos_encima <- function(datos) {
  datos |> 
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
}


grafico_circulos_unidos <- function(datos) {
  datos |> 
    ggplot() +
    list(
      # círculos
      list(
        geom_circle(
          data = ~filter(.x, condicion == "intento"),
          aes(x0 = horizontal, y0 = vertical, r = valor,
              fill = genero, color = genero,
              alpha = condicion),
          linewidth = 0
        ),
        geom_circle(
          data = ~filter(.x, condicion != "intento"),
          aes(x0 = horizontal, y0 = vertical, r = valor,
              fill = genero, color = genero,
              alpha = condicion),
          linewidth = 0.4
        ) 
      ) |> blend("multiply"),
      # líneas horizontales
      geom_hline(
        aes(yintercept = valor, color = genero),
        alpha = 0.5),
      geom_hline(
        aes(yintercept = -valor, color = genero),
        alpha = 0.5)
    ) |> blend("overlay") +
    # bordes
    geom_circle(
      data = ~filter(.x, condicion != "intento"),
      aes(x0 = horizontal, y0 = vertical, r = valor,
          fill = genero, color = genero,
          alpha = condicion),
      fill = NA, linewidth = 0.3
    ) |> blend(alpha = 0.4) +
    # recorte
    coord_fixed(xlim = c(-4400, 4000),
                ylim = c(-3000, 3000),
                expand = FALSE) +
    escala_genero() +
    scale_alpha_manual(values = c("intento" = 0.5, "consumado" = 1)) +
    tema_sin_fondo() +
    tema_sin_ejes()
}


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