# Cargar las librerías
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(ggrepel)

# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Excel/Resultados_puntos_analisis.xlsx")
data_index <-read_excel("Excel/LSWI_zones_hum_buf.xlsx", sheet=2)


################################################################################
                          # GRÁFICO LSWI-REGADÍO
################################################################################
data_irrigation <- datos %>% # ¿Añadir áreas agrarias heterogéneas? ¿No, verdad?
  group_by(COD_IHA)%>%
  summarize(
    regadio84 = sum(ifelse(MUCVA1984 %in% c("Invernaderos", "Lenoso regadio"), 1, 0)),
    secano84 = sum(ifelse(MUCVA1984 %in% c("Cultivos herbaceos", "Lenoso secano"), 1, 0)),
    # regadio20 = sum(ifelse(SIOSE2020 %in% c("Invernaderos", "Lenoso regadio"), 1, 0)),
    # secano20 = sum(ifelse(SIOSE2020 %in% c("Cultivos herbaceos", "Lenoso secano"), 1, 0)),
    regadio20 = sum(ifelse(SIOSE_T_RI %in% c("Invernaderos", "Herbaceo regadio", "Lenoso regadio"), 1, 0)),
    secano20 = sum(ifelse(SIOSE_T_RI %in% c("Herbaceo secano", "Lenoso secano"), 1, 0))
  ) %>%
  mutate(regadio_percent84=regadio84/(secano84+regadio84),
         regadio_percent20=regadio20/(secano20+regadio20))


# Resumir el promedio de LSWI por year_periods
lswi_summary <- data_index %>%
  group_by(year_periods, COD_IHA) %>%
  summarize(
    promedio_lswi = mean(lswi_periods, na.rm = TRUE))  # Promedio de LSWI
lswi_summary$year_periods<-as.factor(lswi_summary$year_periods)
lswi_summary$COD_IHA<-as.factor(lswi_summary$COD_IHA)
lswi_data <- pivot_wider(names_from = year_periods,values_from = promedio_lswi,data = lswi_summary)

# Integrar los datos de LSWI en los datos de irrigación
data_irrigation_combined <- data_irrigation %>%
  left_join(lswi_data, by = "COD_IHA")  # Unión por columna común

#Valores NaN = 0, porque no hay regadio, aunque tampoco haya secano
data_irrigation_combined[2,6]<-0 
data_irrigation_combined[2,7]<-0


data_names <-read_excel("Nombre_ID_humedales.xlsx")

# Integrar los datos de LSWI en los datos de irrigación
data_irrigation_combined <- data_irrigation_combined %>%
  left_join(data_names, by = "COD_IHA")  # Unión por columna común

# Convertir los datos a formato largo
data_long <- data_irrigation_combined %>%
  pivot_longer(
    cols = c("1999_2010", "2011_2021"),  # Columnas de LSWI
    names_to = "periodo",
    values_to = "lswi"
  ) %>%
  pivot_longer(
    cols = c("regadio_percent84", "regadio_percent20"),
    names_to = "regadio_periodo",
    values_to = "regadio_percent"
  )

# Mapear los períodos de regadío a los períodos de LSWI
data_long <- data_long %>%
  mutate(periodo_regadio = ifelse(regadio_periodo == "regadio_percent84", "1999_2010", "2011_2021")) %>%
  filter(periodo == periodo_regadio)


################################################################################
# GRÁFICO PUNTOS
# Crear la gráfica de puntos
ggplot(data_long, aes(x = lswi, y = regadio_percent, color = periodo)) +
  geom_point(size = 3) +  # Puntos de datos
  labs(
    title = "Relación entre % de Regadío y LSWI",
    x = "LSWI",
    y = "% de Regadío"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("1999_2010" = "blue", "2011_2021" = "red")) +
  theme(legend.title = element_blank())  # Eliminar título de la leyenda

#GRÁFICO BARRAS APILADAS POR PERIODO Y ZONA
# Preparar datos para gráfico apilado
data_bar <- data_irrigation_combined %>%
  pivot_longer(
    cols = c("regadio_percent84", "regadio_percent20"),
    names_to = "periodo",
    values_to = "percent_regadio"
  ) %>%
  mutate(periodo = ifelse(periodo == "regadio_percent84", "1999_2010", "2011_2021"))

# Gráfico de barras apiladas
ggplot(data_bar, aes(x = periodo, y = percent_regadio, fill = COD_IHA)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Proporción de Regadío por Período y Zona",
    x = "Período",
    y = "% de Regadío",
    fill = "Zona (COD_IHA)"
  ) +
  theme_minimal()

#GRÁFICO PUNTOS Y LÍNEAS TENDENCIA¿?
ggplot(data_long, aes(x = lswi, y = regadio_percent, color = periodo)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(aes(group = periodo), method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    title = "Relación entre Regadío (%) y LSWI por Período",
    x = "LSWI",
    y = "% de Regadío",
    color = "Período"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
  ) +  # Centrar y estilizar el título
  scale_color_manual(values = c("1999_2010" = "blue", "2011_2021" = "red"))


# EL ANTERIOR PERO SEPARADO EN PERIODOS:
ggplot(data_long, aes(x = lswi, y = regadio_percent)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  facet_wrap(~ periodo, ncol = 2) +
  labs(
    title = "Relación entre Regadío (%) y LSWI en Diferentes Períodos",
    x = "LSWI",
    y = "% de Regadío"
  ) +
  theme_minimal()

# LO ANTERIOR PERO CON ETIQUETAS (ESTE ES EL QUE HE AÑADIDO EN LA MEMORIA DE TFM)
ggplot(data_long, aes(x = lswi, y = regadio_percent)) +
  geom_point(size = 3, alpha = 0.7) +  # Puntos
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +  # Línea de regresión
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~ periodo, ncol = 2) +  # Facetas por período
  geom_text_repel(aes(label = ID_HUM), size = 3, max.overlaps = 10) +  # Etiquetas con ajuste
  labs(
    title = "Relación entre regadío (%) y LSWI en diferentes períodos",
    x = "LSWI",
    y = "% de regadío"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "gray25", fill = NA, size = 1),
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")# Centrar y estilizar el título
  ) 
#+geom_hline(yintercept = 0, color = "gray25") # Línea horizontal en y = 0



################################################################################
                        # GRÁFICO LSWI-INVERNADEROS
################################################################################

data_irrigation_2 <- datos %>% # ¿Añadir áreas agrarias heterogéneas? ¿No, verdad?
  group_by(COD_IHA)%>%
  summarize(
    invernaderos84 = sum(ifelse(MUCVA1984 %in% c("Invernaderos"), 1, 0)),
    otros_agrario84 = sum(ifelse(MUCVA1984 %in% c("Cultivos herbaceos", "Lenoso secano", "Lenoso regadio","Areas agrarias heterogeneas"), 1, 0)),
    # regadio20 = sum(ifelse(SIOSE2020 %in% c("Invernaderos", "Lenoso regadio"), 1, 0)),
    # secano20 = sum(ifelse(SIOSE2020 %in% c("Cultivos herbaceos", "Lenoso secano"), 1, 0)),
    invernaderos20 = sum(ifelse(SIOSE_T_RI %in% c("Invernaderos"), 1, 0)),
    otros_agrario20 = sum(ifelse(SIOSE_T_RI %in% c("Herbaceo secano", "Lenoso secano", "Herbaceo regadio", "Lenoso regadio","Areas agrarias heterogeneas"), 1, 0))
  ) %>%
  mutate(invernaderos_percent84=invernaderos84/(otros_agrario84+invernaderos84),
         invernaderos_percent20=invernaderos20/(otros_agrario20+invernaderos20))


# Resumir el promedio de LSWI por year_periods
lswi_summary <- data_index %>%
  group_by(year_periods, COD_IHA) %>%
  summarize(
    promedio_lswi = mean(lswi_periods, na.rm = TRUE))  # Promedio de LSWI
lswi_summary$year_periods<-as.factor(lswi_summary$year_periods)
lswi_summary$COD_IHA<-as.factor(lswi_summary$COD_IHA)
lswi_data <- pivot_wider(names_from = year_periods,values_from = promedio_lswi,data = lswi_summary)

# Integrar los datos de LSWI en los datos de irrigación
data_irrigation_combined_2 <- data_irrigation_2 %>%
  left_join(lswi_data, by = "COD_IHA")  # Unión por columna común

#Valores NaN = 0, porque no hay invernaderos, aunque tampoco haya otros usos agrarios
data_irrigation_combined_2[2,6]<-0 
data_irrigation_combined_2[2,7]<-0

data_names <-read_excel("Nombre_ID_humedales.xlsx")

# Integrar los datos de LSWI en los datos de irrigación
data_irrigation_combined_2 <- data_irrigation_combined_2 %>%
  left_join(data_names, by = "COD_IHA")  # Unión por columna común

# Convertir los datos a formato largo
data_long_2 <- data_irrigation_combined_2 %>%
  pivot_longer(
    cols = c("1999_2010", "2011_2021"),  # Columnas de LSWI
    names_to = "periodo",
    values_to = "lswi"
  ) %>%
  pivot_longer(
    cols = c("invernaderos_percent84", "invernaderos_percent20"),
    names_to = "invernaderos_periodo",
    values_to = "invernaderos_percent"
  )

# Mapear los períodos de regadío a los períodos de LSWI
data_long_2 <- data_long_2 %>%
  mutate(periodo_invernaderos = ifelse(invernaderos_periodo == "invernaderos_percent84", "1999_2010", "2011_2021")) %>%
  filter(periodo == periodo_invernaderos)


################################################################################
# GRÁFICO PUNTOS
# Crear la gráfica de puntos
ggplot(data_long_2, aes(x = lswi, y = invernaderos_percent, color = periodo)) +
  geom_point(size = 3) +  # Puntos de datos
  labs(
    title = "Relación entre % de invernaderos con respecto al uso agrario y LSWI",
    x = "LSWI",
    y = "% de invernaderos"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("1999_2010" = "blue", "2011_2021" = "red")) +
  theme(legend.title = element_blank())  # Eliminar título de la leyenda

#GRÁFICO BARRAS APILADAS POR PERIODO Y ZONA
# Preparar datos para gráfico apilado
data_bar <- data_irrigation_combined_2 %>%
  pivot_longer(
    cols = c("invernaderos_percent84", "invernaderos_percent20"),
    names_to = "periodo",
    values_to = "percent_invernaderos"
  ) %>%
  mutate(periodo = ifelse(periodo == "invernaderos_percent84", "1999_2010", "2011_2021"))

# Gráfico de barras apiladas
ggplot(data_bar, aes(x = periodo, y = percent_invernaderos, fill = COD_IHA)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Proporción de invernaderos con respecto al uso agrario  por Período y Zona",
    x = "Período",
    y = "% de Regadío",
    fill = "Zona (COD_IHA)"
  ) +
  theme_minimal()

#GRÁFICO PUNTOS Y LÍNEAS TENDENCIA¿?
ggplot(data_long_2, aes(x = lswi, y = invernaderos_percent, color = periodo)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(aes(group = periodo), method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    title = "Relación entre invernaderos con respecto al uso agrario (%) y LSWI por Período",
    x = "LSWI",
    y = "% de invernaderos",
    color = "Período"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
  ) +  # Centrar y estilizar el título
  scale_color_manual(values = c("1999_2010" = "blue", "2011_2021" = "red"))


# EL ANTERIOR PERO SEPARADO EN PERIODOS:
ggplot(data_long_2, aes(x = lswi, y = invernaderos_percent)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  facet_wrap(~ periodo, ncol = 2) +
  labs(
    title = "Relación entre invernaderos con respecto al uso agrario (%) y LSWI en Diferentes Períodos",
    x = "LSWI",
    y = "% de invernaderos"
  ) +
  theme_minimal()

# LO ANTERIOR PERO CON ETIQUETAS (ESTE ES EL QUE HE AÑADIDO EN LA MEMORIA DE TFM)
ggplot(data_long_2, aes(x = lswi, y = invernaderos_percent)) +
  geom_point(size = 3, alpha = 0.7) +  # Puntos
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +  # Línea de regresión
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~ periodo, ncol = 2) +  # Facetas por período
  geom_text_repel(aes(label = ID_HUM), size = 3, max.overlaps = 10) +  # Etiquetas con ajuste
  labs(
    title = "Relación entre invernaderos con respecto al uso agrario (%) y LSWI en diferentes períodos",
    x = "LSWI",
    y = "% de invernaderos"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "gray25", fill = NA, size = 1),
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")# Centrar y estilizar el título
  ) 
#+geom_hline(yintercept = 0, color = "gray25") # Línea horizontal en y = 0



