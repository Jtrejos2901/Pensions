df <- read_csv("BDCotizantes(Hoja1).csv") %>%
  mutate(Fec.Nac = dmy(Fec.Nac))

meses_map <- c("ene"="01", "feb"="02", "mar"="03", "abr"="04", "may"="05", "jun"="06",
               "jul"="07", "ago"="08", "sep"="09", "oct"="10", "nov"="11", "dic"="12")

cols_cotiz <- names(df)[4:(ncol(df)-1)]

df_largo <- df %>%
  select(-4) %>% 
  pivot_longer(
    cols = 4:ncol(.),  
    names_to  = "Periodo_str",
    values_to = "Monto"
  ) %>%
  mutate(
    Periodo_str = str_to_lower(Periodo_str),
    mes = str_sub(Periodo_str, 1, 3),
    anio = str_sub(Periodo_str, 5, 8),
    Periodo = ymd(paste0(anio, "-", meses_map[mes], "-01")),
    Monto = as.numeric(gsub(",", "", Monto)),
    Edad_anio = year(Periodo) - year(Fec.Nac)
  )


# Filtrar edades válidas y calcular cotización binaria
df_filtrado <- df_largo %>%
  filter(!is.na(Edad_anio), Edad_anio >= 20) %>%
  mutate(Cotiza = if_else(Monto > 0, 1L, 0L))

# Contar meses cotizados por persona y edad
meses_por_edad <- df_filtrado %>%
  group_by(ID, Edad_anio) %>%
  summarise(MesesCotizados = sum(Cotiza, na.rm = TRUE), .groups = "drop")

#  densidad promedio (máximo 12)
densidad_por_edad <- meses_por_edad %>%
  group_by(Edad_anio) %>%
  summarise(DensidadPromedio = round(mean(MesesCotizados), 0), .groups = "drop") %>%
  arrange(Edad_anio)

print(densidad_por_edad)


# Crear variable de rango de edad en intervalos de 5 años
meses_por_edad <- meses_por_edad %>%
  mutate(RangoEdad = case_when(
    Edad_anio >= 20 & Edad_anio <= 24 ~ "20-24",
    Edad_anio >= 25 & Edad_anio <= 29 ~ "25-29",
    Edad_anio >= 30 & Edad_anio <= 34 ~ "30-34",
    Edad_anio >= 35 & Edad_anio <= 39 ~ "35-39",
    Edad_anio >= 40 & Edad_anio <= 44 ~ "40-44",
    Edad_anio >= 45 & Edad_anio <= 49 ~ "45-49",
    Edad_anio >= 50 & Edad_anio <= 54 ~ "50-54",
    Edad_anio >= 55 & Edad_anio <= 59 ~ "55-59",
    Edad_anio >= 60 & Edad_anio <= 64 ~ "60-64",
    Edad_anio >= 65 & Edad_anio <= 69 ~ "65-69",
    Edad_anio >= 70 & Edad_anio <= 74 ~ "70-74",
    Edad_anio >= 75 & Edad_anio <= 79 ~ "75-79",
    Edad_anio >= 80 & Edad_anio <= 80 ~ "80",
    TRUE ~ NA_character_
  ))

densidad_por_rango <- meses_por_edad %>%
  filter(!is.na(RangoEdad)) %>%
  group_by(RangoEdad) %>%
  summarise(DensidadPromedio = round(mean(MesesCotizados), 0), .groups = "drop") %>%
  arrange(RangoEdad)

print(densidad_por_rango)
