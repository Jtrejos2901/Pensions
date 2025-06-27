library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Supongamos que tu data original es:
df <- data.frame(
  Año = names(suma_vejez),
  Vejez = as.numeric(suma_vejez),
  Invalidez = as.numeric(suma_invalidez),
  Viudez = as.numeric(suma_viudez),
  Orfandad = as.numeric(suma_orfandad)
)

# Convertir Año a numérico si es necesario
df$Año <- as.numeric(df$Año)

# Reestructurar a formato largo
df_largo <- df %>%
  pivot_longer(cols = -Año, names_to = "Categoria", values_to = "Valor")

# Crear el gráfico
ggplot(df_largo, aes(x = factor(Año), y = Valor, color = Categoria, group = Categoria)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    limits = c(0, 60000),
    breaks = seq(0, 60000, by = 10000),
    labels = comma
  ) +
  scale_color_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2"))) +
  labs(
    x = "Año",
    y = "Beneficio (colones)",
    color = "Tipo de Beneficio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )
