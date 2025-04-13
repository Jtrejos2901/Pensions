setwd("/Users/joseandres/Desktop/Pensiones/Pensions/Jose C")

library(tidyverse)
library(readxl)
poblacion <- read_excel("variacion_poblacion.xlsx", 
                                  sheet = "Variacion")


variacion_total_porcentual <- poblacion %>% filter(Tipo=="Total")
cambio <- diff(variacion_total_porcentual$personas)/variacion_total_porcentual$personas[1:24]
variacion_total_porcentual <- variacion_total_porcentual[2:25,]
variacion_total_porcentual$personas <- cambio
colnames(variacion_total_porcentual)[2] <- "Cambio Porcentual"

grafico_cambio_poblacional <- ggplot(data = variacion_total_porcentual,mapping = aes(x=Año,y=`Cambio Porcentual`*100))+
  geom_line(color="darkblue",size=1.5)+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  labs(y = "Cambio porcentual",
       x = "Año")+
  theme(axis.title.y = element_text(vjust=-0.5,size=rel(1), angle = 90), 
        legend.position = "bottom") +theme_minimal()+ggtitle("Cambio porcentual año a año en la población de Costa Rica desde el año 2000 a 2024")




grafico_cambio_poblacional <- ggplot(data = variacion_total_porcentual,
                                     mapping = aes(x = Año, y = `Cambio Porcentual` * 100)) +
  geom_line(color = "darkblue", size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x = "Año",
    y = "Cambio porcentual",
    title = "Cambio porcentual año a año\nen la población de Costa Rica desde el año 2000 a 2024"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = -0.5, size = rel(1), angle = 90),
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,   # Centrar el título
      face = "bold", # Negrita
      size = 14      # Ajusta el tamaño a tu preferencia
    )
  )





proyeccion <- read_excel("variacion_poblacion.xlsx", 
                         sheet = "proyeccion")




df_mod <- proyeccion %>%
  # Extraemos el primer número como valor numérico
  mutate(edad_inicio = as.numeric(str_extract(Total, "^\\d+"))) %>%
  mutate(
    GrupoEdad = case_when(
      edad_inicio <= 19 ~ "0-19",
      edad_inicio >= 20 & edad_inicio <= 64 ~ "20-64",
      edad_inicio >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-Total, -edad_inicio) %>%
  group_by(GrupoEdad) %>%
  summarise(across(everything(), ~ sum(as.numeric(gsub("[^0-9.]", "", .)), na.rm = TRUE)))


  
df_final_proyeccion <- df_mod %>% 
  pivot_longer(
    cols = -GrupoEdad,        # Todas las columnas excepto GrupoEdad se pivotearán
    names_to = "Año",         # El nombre de la columna que contendrá el nombre original (año)
    values_to = "Cantidad"    # La columna con los valores (la cantidad de personas)
  ) %>% 
  mutate(
    Año = as.integer(str_extract(Año, "\\d{4}"))  # Extraer el año, ej. "2022" de "2022a"
  ) 



g_proyeccion_poblacional <- ggplot(data = df_final_proyeccion, 
                                   mapping = aes(x = Año, y = Cantidad/1000000, color = GrupoEdad)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("0-19" = "darkblue", "20-64" = "brown3", "65+" = "skyblue")) +
  labs(
    x = "Año",
    y = "Cantidad de personas (En millones)",
    title = "Población de Costa Rica por grupo de edad\ndesde el año 2010 al año 2079"  # Título en dos líneas
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = -0.5, size = rel(1), angle = 90),
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,   # Centrar el título
      face = "bold", # En negrita
      size = 14      # Ajusta el tamaño según prefieras
    )
  )

ggsave(filename = "Cambio_Poblacional.pdf",plot = grafico_cambio_poblacional)

ggsave(filename = "Proyeccion_Poblacional.pdf",plot = g_proyeccion_poblacional)


