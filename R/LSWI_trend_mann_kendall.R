#vignette("ggplot2-specs")

#library
library(funtimes)
library(Kendall)
library(emmeans) # post-hoc y letras de significancia
library(multcompView) # post-hoc y letras de significancia


################################################################################
# Mann-Kendall por humedal y wetland/buffer
################################################################################
# Data
setwd("D:/Escritorio/TFM/rTFM") # ¡Recuerda hacer setwd("D:/Escritorio/TFM/rTFM") en el portátil!
#setwd("D:/Escritorio/TFM/rTFM")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone01b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone02b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone03b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone04b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone05b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone06b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone07b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone08b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone09b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone10b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone11b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone12b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone13b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone14b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/buffer/lswi_zone15b.csv", sep=",", dec = ".")

data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone01.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone02.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone03.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone04.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone05.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone06.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone07.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone08.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone09.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone10.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone11.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone12.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone13.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone14.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_lswi/humedal/lswi_zone15.csv", sep=",", dec = ".")

#Option A #library(funtimes)
notrend_test(data$LSWI)
notrend_test(data$LSWI,test='MK') # Usa esta función
#notrend_test(data$LSWI,test='WAVK', factor.length = "adaptive.selection")
plot(data$Year,data$LSWI, type='l') # Pinta siempre los resultados
# apply LM to check
summary(lm(data$LSWI~data$Year))

#Option B# Sólo quería probar otra forma pero da el mismo resultado #library(Kendall)
MannKendall(data$LSWI)


#NDVI: CAMBIAR DATOS BASE
MannKendall(data$NDVI)


#Examples test

# test (source: https://cran.r-project.org/web/packages/funtimes/vignettes/trendtests.html)
# set.seed(777)
# n <- 100
# Time <- c(1:n)
# X0 <- arima.sim(list(order = c(1, 0, 0), ar = 0.5), n = n, n.start = 100, sd = 0.5)
# X1 <- 2*Time/n + X0
# X11<-as.numeric(X1) # convert TS to a vector of numbers

# plot(X11)
# notrend_test(X11)
# end test

# Example (source: https://rdrr.io/cran/Kendall/man/MannKendall.html)
# data(PrecipGL)
# plot(PrecipGL)
# MannKendall(PrecipGL)

################################################################################
                          # Mann-Kendall espacial #
################################################################################
library(here)

# Install the required packages
# install.packages("zyp")
# install.packages("terra")
# install.packages("spatialEco")

# Load required packages

library(spatialEco)
library(terra)

# Create a multiband terra SpatRaster object with 10 layers

# Definir años
years <- 1999:2021

# Definir la ruta base ####################### ELEGIR ########################
base_path <- "D:/Escritorio/TFM/rTFM/Google Earth Engine/zones_ndvi/humedal/"
base_path <- "D:/Escritorio/TFM/rTFM/Google Earth Engine/zones_ndvi/buffer/"
# Definir subrutas para cada zona
paths <- c(
  "zone01h_albufera_honda/",
  "zone02h_albufera_nueva/",
  "zone03h_barranco_del_agua/",
  "zone04h_canada_de_las_norias/",
  "zone05h_cola_del_embalse_del_negratin/",
  "zone06h_charcones_de_punta_entinas/",
  "zone07h_humedales_de_baza/",
  "zone08h_laguna_de_la_gravera/",
  "zone09h_rambla_morales/",
  "zone10h_ribera_de_la_algaida/",
  "zone11h_rio_antas/",
  "zone12h_salinas_de_cabo_de_gata/",
  "zone13h_salar_de_los_canos/",
  "zone14h_salinas_de_cerrillos/",
  "zone15h_saladar_del_margen/"
)

paths <- c(
  "zone01b_albufera_honda/",
  "zone02b_albufera_nueva/",
  "zone03b_barranco_del_agua/",
  "zone04b_canada_de_las_norias/",
  "zone05b_cola_del_embalse_del_negratin/",
  "zone06b_charcones_de_punta_entinas/",
  "zone07b_humedales_de_baza/",
  "zone08b_laguna_de_la_gravera/",
  "zone09b_rambla_morales/",
  "zone10b_ribera_de_la_algaida/",
  "zone11b_rio_antas/",
  "zone12b_salinas_de_cabo_de_gata/",
  "zone13b_salar_de_los_canos/",
  "zone14b_salinas_de_cerrillos/",
  "zone15b_saladar_del_margen/"
)

# Nombres de las zonas (corrigiendo zone13h)
zones <- c("zone01h","zone02h","zone03h",
           "zone04h","zone05h","zone06h",
           "zone07h","zone08h","zone09h",
           "zone10h","zone11h","zone12h",
           "zone13h","zone14h","zone15h")

zones <- c("zone01b","zone02b","zone03b",
           "zone04b","zone05b","zone06b",
           "zone07b","zone08b","zone09b",
           "zone10b","zone11b","zone12b",
           "zone13b","zone14b","zone15b")

# Crear una lista para almacenar los datos de cada zona
r_zones <- list()

# Loop para cargar los archivos de cada zona sin cambiar el directorio de trabajo
for (i in 1:length(paths)) {
  # Cargar los archivos usando lapply, construyendo la ruta completa
  r_zones[[zones[i]]] <- lapply(years, function(year) {
    # Crear la ruta completa del archivo TIFF
    file_path <- paste0(base_path, paths[i], zones[i], "_ndvi_", year, ".tif")
    
    # Cargar el archivo .tif
    rast(file_path)
  })
}

# Ahora tienes todos los archivos cargados en r_zones


# Ahora tienes una lista con los datos cargados para cada zona
# r_zones[["zone01h"]] contiene los archivos de la primera zona
# r_zones[["zone03h"]] contiene los archivos de la segunda zona


# Y ahora paso a rast() cada elemento de esa lista:
r_zone01h <- rast(r_zones$zone01h)
r_zone02h <- rast(r_zones$zone02h)
r_zone03h <- rast(r_zones$zone03h)
r_zone04h <- rast(r_zones$zone04h)
r_zone05h <- rast(r_zones$zone05h)
r_zone06h <- rast(r_zones$zone06h)
r_zone07h <- rast(r_zones$zone07h)
r_zone08h <- rast(r_zones$zone08h)
r_zone09h <- rast(r_zones$zone09h)
r_zone10h <- rast(r_zones$zone10h)
r_zone11h <- rast(r_zones$zone11h)
r_zone12h <- rast(r_zones$zone12h)
r_zone13h <- rast(r_zones$zone13h)
r_zone14h <- rast(r_zones$zone14h)
r_zone15h <- rast(r_zones$zone15h)

r_zone01b <- rast(r_zones$zone01b)
r_zone02b <- rast(r_zones$zone02b)
r_zone03b <- rast(r_zones$zone03b)
r_zone04b <- rast(r_zones$zone04b)
r_zone05b <- rast(r_zones$zone05b)
r_zone06b <- rast(r_zones$zone06b)
r_zone07b <- rast(r_zones$zone07b)
r_zone08b <- rast(r_zones$zone08b)
r_zone09b <- rast(r_zones$zone09b)
r_zone10b <- rast(r_zones$zone10b)
r_zone11b <- rast(r_zones$zone11b)
r_zone12b <- rast(r_zones$zone12b)
r_zone13b <- rast(r_zones$zone13b)
r_zone14b <- rast(r_zones$zone14b)
r_zone15b <- rast(r_zones$zone15b)

# Crear una lista para almacenar los datos de cada zona
r_zones <- list()

# Loop para cargar los archivos de cada zona sin cambiar el directorio de trabajo
for (i in 1:length(paths)) {
  # Cargar los archivos usando lapply, construyendo la ruta completa
  r_zones[[zones[i]]] <- lapply(years, function(year) {
    # Crear la ruta completa del archivo TIFF
    file_path <- paste0(base_path, paths[i], zones[i], "_ndvi_", year, ".tif")
    
    # Cargar el archivo .tif
    rast(file_path)
  })
}

# Ahora tienes todos los archivos cargados en r_zones


# Ahora tienes una lista con los datos cargados para cada zona
# r_zones[["zone01h"]] contiene los archivos de la primera zona
# r_zones[["zone03h"]] contiene los archivos de la segunda zona


# Y ahora paso a rast() cada elemento de esa lista:
r_zone01h <- rast(r_zones$zone01h)
r_zone02h <- rast(r_zones$zone02h)
r_zone03h <- rast(r_zones$zone03h)
r_zone04h <- rast(r_zones$zone04h)
r_zone05h <- rast(r_zones$zone05h)
r_zone06h <- rast(r_zones$zone06h)
r_zone07h <- rast(r_zones$zone07h)
r_zone08h <- rast(r_zones$zone08h)
r_zone09h <- rast(r_zones$zone09h)
r_zone10h <- rast(r_zones$zone10h)
r_zone11h <- rast(r_zones$zone11h)
r_zone12h <- rast(r_zones$zone12h)
r_zone13h <- rast(r_zones$zone13h)
r_zone14h <- rast(r_zones$zone14h)
r_zone15h <- rast(r_zones$zone15h)


r_zone01b <- rast(r_zones$zone01b)
r_zone02b <- rast(r_zones$zone02b)
r_zone03b <- rast(r_zones$zone03b)
r_zone04b <- rast(r_zones$zone04b)
r_zone05b <- rast(r_zones$zone05b)
r_zone06b <- rast(r_zones$zone06b)
r_zone07b <- rast(r_zones$zone07b)
r_zone08b <- rast(r_zones$zone08b)
r_zone09b <- rast(r_zones$zone09b)
r_zone10b <- rast(r_zones$zone10b)
r_zone11b <- rast(r_zones$zone11b)
r_zone12b <- rast(r_zones$zone12b)
r_zone13b <- rast(r_zones$zone13b)
r_zone14b <- rast(r_zones$zone14b)
r_zone15b <- rast(r_zones$zone15b)

# Por ejemplo, r_zone02h se ve así, como 15 gráficas, una para cada año, en ese humedal:
plot(r_zone10b)


# Source: https://www.pmassicotte.com/posts/2022-04-28-changing-spatial-resolution-of-a-raster-with-terra/

# Aggregate the raster using 8 pixels within the horizontal and the vertical directions
# Lista de los objetos raster
r_list <- list(r_zone01h,r_zone02h,r_zone03h,
               r_zone04h,r_zone05h,r_zone06h,
               r_zone07h,r_zone08h,r_zone09h,
               r_zone10h,r_zone11h,r_zone12h,
               r_zone13h,r_zone14h,r_zone15h)

r_list <- list(r_zone01b,r_zone02b,r_zone03b,
               r_zone04b,r_zone05b,r_zone06b,
               r_zone07b,r_zone08b,r_zone09b,
               r_zone10b,r_zone11b,r_zone12b,
               r_zone13b,r_zone14b,r_zone15b)
# Loop para agregar y escribir raster para cada zona
mk_results <- list()
for (i in 1:length(r_list)) {
  # Obtener el raster de la zona actual
  r <- r_list[[i]]
  
  # Agregar con un factor de 8 (mayor resolución aprox. 200m)
  r8 <- aggregate(r, fact = 8)

  ############################# EXPORTAR #######################################  
  # Escribir el raster resultante en un archivo TIFF
  setwd("D:/Escritorio/TFM/rTFM/R/resample_r8")
  output_filename <- paste0("input_", zones[i], "_ndvi_ts_scale8.tif")
  writeRaster(r8, here("R/resample_r8/", output_filename), overwrite = TRUE)
  
  # Imprimir mensaje de progreso
  print(paste("Archivo guardado:", output_filename))
  
  ################### SIGNIFICANCIA P-VALUE < 0.05 #############################
  # Calcular las estadísticas de Mann-Kendall
  mk <- raster.kendall(r8, method = "none")
  
  # Guardar el resultado en la lista con el nombre de la zona
  mk_results[[zones[i]]] <- mk
  
  # Re-clasificar celdas con valores de p menores a 0.05 (significativo)
  signif <- mk$`p-value` < 0.05
  
  # Graficar las celdas significativas y no significativas
  plot(signif, main = paste("Celdas significativas -", zones[i]))
  plot(!signif, main = paste("Celdas no significativas -", zones[i]))
  
  # Crear un nuevo raster que contenga los valores de pendiente originales
  mk_slope <- mk$slope
  
  # Asignar valor NA a las celdas con p-valor mayor a 0.05 (no significativas)
  mk_slope[!signif] <- NA
  
  # Graficar las pendientes significativas
  plot(mk_slope, main = paste("Pendientes significativas -", zones[i]))
  
  # Guardar los archivos raster con y sin significancia
  output_signif_filename <- paste0("output_", zones[i], "_ndvi_mk_slope_scale8_signif.tif")
  output_slope_filename <- paste0("output_", zones[i], "_ndvi_mk_slope_scale8.tif")
  
  ############################# EXPORTAR #######################################
  # Guardar solo los valores significativos
  writeRaster(mk_slope, output_signif_filename, overwrite = TRUE)
  
  # Guardar todas las pendientes (sin NA)
  writeRaster(mk$slope, here("R/mk/", output_slope_filename), overwrite = TRUE)
  
  # Imprimir mensaje de progreso
  print(paste("Archivos guardados para:", zones[i]))
  
}
setwd("D:/Escritorio/TFM/rTFM") # Vuelvo al setwd de antes








# Reclass cells with slope values lower then 0.05 (TRUE) and the rest (FALSE)
signif<-mk$`p-value`<0.05

plot(signif)

plot(!signif) 

# New raster object containing original slope values
mk_slope<-mk$slope

# Assign NA value to all cells with p-value higher than the threshold (cells with signif == FALSE)
mk_slope[!signif]<-NA

plot(mk_slope)

writeRaster(mk_slope,'output_ndvi_mk_slope_scale8_signif.tif',overwrite=T)













#GRÁFICO LSWI HUMEDALES ########################################################

################################################################################
# Tendencia del LSWI en el tiempo por humedal ##################################
library(ggplot2)
library(readxl)

datos <-read_excel("Excel/LSWI_zones_hum_buf.xlsx")

# # Tendencia del LSWI en el tiempo separado por humedal
# ggplot(datos, aes(x = year, y = lswi, color = wetland_name)) +
#   geom_line() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "Tendencia del LSWI en el tiempo por humedal",
#        x = "Año", y = "LSWI") +
#   facet_wrap(~ wetland_name, scales = "free_y") + 
#   theme_minimal()


# GRÁFICO CON R^2 AJUSTADO #####################################################
library(ggplot2)
library(dplyr)
#library(broom) # Necesario para la función glance()

# Calcular el R^2 ajustado para cada humedal y unir con los datos originales
datos_con_r2 <- datos %>%
  group_by(wetland_name) %>%
  do(modelo = lm(lswi ~ year, data = .)) %>%
  mutate(adj_r2 = summary(modelo)$adj.r.squared) %>%
  ungroup() %>%
  left_join(datos, by = "wetland_name")

# # Gráfico con R^2 ajustado
# ggplot(datos_con_r2, aes(x = year, y = lswi, color = wetland_name)) +
#   geom_line() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "Tendencia del LSWI en el tiempo por Humedal",
#        x = "Año", y = "LSWI") +
#   facet_wrap(~ wetland_name, scales = "free_y") + 
#   theme_minimal() +
#   geom_text(data = datos_con_r2 %>% distinct(wetland_name, adj_r2), 
#             aes(x = Inf, y = Inf, label = paste("R² adj: ", round(adj_r2, 3))),
#             hjust = 1.1, vjust = 1.1, inherit.aes = FALSE, size = 3)

# GRAFICO CON LINEAS DE BUFFER Y DE HUMEDAL SEPARADAS
datos$wetland_or_buffer = recode(datos$wetland_or_buffer,
                                    "wetland" = "Humedal",
                                    "buffer" = "Buffer")


datos_con_r2 <- datos %>%
  group_by(wetland_name, wetland_or_buffer) %>%
  summarise(adj_r2 = summary(lm(lswi ~ year, data = .))$adj.r.squared) %>%
  ungroup()

nombre_humedales_correctos <- c(
  "albufera_honda" = "Albufera Honda",
  "albufera_nueva" = "Albufera Nueva",
  "barranco_del_agua" = "Barranco del Agua",
  "canada_de_las_norias" = "Cañada de las Norias",
  "cola_del_embalse_del_negratin" = "Cola del embalse del Negratín",
  "charcones_de_punta_entinas" = "Charcones de Punta-Entinas",
  "humedales_de_baza" = "Humedales de Baza",
  "laguna_de_la_gravera" = "Laguna de la Gravera",
  "rambla_morales" =  "Rambla Morales",
  "ribera_de_la_algaida" = "Ribera de la Algaida",
  "rio_antas" = "Río Antas",
  "salinas_de_cabo_de_gata" = "Salinas de Cabo de Gata",
  "salar_de_los_canos" = "Salar de los Canos",
  "salinas_de_cerrillos" = "Salinas de Cerrillos",
  "saladar_del_margen" = "Saladar del Margen")
  
# Crear el gráfico con líneas separadas por "wetland_or_buffer"
ggplot(datos, aes(x = year, y = lswi, color = wetland_or_buffer)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Humedal" = "blue", "Buffer" = "orange")) +
  labs(title = "Tendencia del índice LSWI en el tiempo por humedal",
       x = "Año", y = "LSWI", color = "Tipo de área") +
  facet_wrap(~ wetland_name, scales = "free_y", ncol = 3, labeller = labeller(wetland_name = nombre_humedales_correctos) 
) + 
  coord_cartesian(ylim = c(-0.05, 0.12)) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  theme(plot.title = element_text(hjust=0.5),    legend.position = "bottom")
  #geom_text(data = datos_con_r2, 
    #         aes(x = Inf, y = Inf, label = paste("R² adj: ", round(adj_r2, 3))),
    #        hjust = 1.1, vjust = 1.1, inherit.aes = FALSE, size = 3)

# Explicación del código:
#aes(x = year, y = lswi, color = wetland_name): Establece el año en el eje x y el LSWI en el eje y, y colorea las líneas según el nombre del humedal.
#geom_line(): Dibuja las líneas de LSWI a lo largo del tiempo.
#geom_smooth(method = "lm", se = FALSE): Añade una línea de tendencia (regresión lineal) para cada humedal sin mostrar el intervalo de confianza (al usar se = FALSE).
#facet_wrap(~ wetland_name, scales = "free_y"): Crea un gráfico separado para cada humedal. scales = "free_y" permite que cada gráfico tenga su propio rango en el eje y, adaptado a los valores de LSWI de cada humedal.
#theme_minimal(): Aplica un tema minimalista al gráfico para mejorar la claridad visual.


# NDVI
max(datos$ndvi)
min(datos$ndvi)
# Crear el gráfico con líneas separadas por "wetland_or_buffer"
ggplot(datos, aes(x = year, y = ndvi, color = wetland_or_buffer)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Humedal" = "chartreuse3", "Buffer" = "chocolate1")) +
  labs(title = "Tendencia del índice NDVI en el tiempo por humedal",
       x = "Año", y = "NDVI", color = "Tipo de área") +
  facet_wrap(~ wetland_name, scales = "free_y", ncol = 3, labeller = labeller(wetland_name = nombre_humedales_correctos)) + 
  coord_cartesian(ylim = c(-0.1, 0.25)) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")




datos$wetland_or_buffer<-as.factor(datos$wetland_or_buffer)
datos$wetland_name<-as.factor(datos$wetland_name)
library(lmerTest)
modeloAMR <- lmer(ndvi ~ wetland_or_buffer:wetland_name + (1 | year), data = datos)
summary(modeloAMR)










################################################################################
                                    # ANÁLISIS
################################################################################

library(readxl)
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Excel/LSWI_zones_hum_buf.xlsx", sheet=1)
datos.p <-read_excel("Excel/LSWI_zones_hum_buf.xlsx", sheet=2)
library(tidyr)
datos.pw <- datos.p %>%
  pivot_wider(names_from = year_periods, values_from = c(lswi_periods, ndvi_periods, ndwi_periods))

t_lswi <- t.test(datos.pw$lswi_periods_2011_2021,
          datos.pw$lswi_periods_1999_2010, 
          alternative = "two.sided",
          var.equal = TRUE,
          conf.level = 0.95,
          paired=T,
          data = datos.pw)

t_lswi
# Hay una diferencia significativa entre los valores de LSWI antes y después, y
# parece que los valores del índice LSWI han AUMENTADO en promedio en el período
# 2011-2021 en comparación con 1999-2010.

t_ndvi <- t.test(datos.pw$ndvi_periods_2011_2021,
          datos.pw$ndvi_periods_1999_2010, 
          alternative = "two.sided",
          var.equal = TRUE,
          conf.level = 0.95,
          paired=T,
          data = datos.pw)

t_ndvi
# Hay una diferencia significativa entre los valores de NDVI antes y después, y
# parece que los valores del índice NDVI han DISMINUIDO en promedio en el período
# 2011-2021 en comparación con 1999-2010.


# ANOVA de medidas repetidas con ezANOVA
library(ez)
datos$wetland_name <- as.factor(datos$wetland_name)
datos$year <- as.factor(datos$year)
datos.p$year_periods <- as.factor(datos.p$year_periods)
datos$protection_yes_no <- as.factor(datos$protection_yes_no)
datos.p$protection_yes_no <- as.factor(datos.p$protection_yes_no)
datos.p$environmental_protection <- as.factor(datos.p$environmental_protection)


############REVISAR
ez_results <- ezANOVA(
  data = datos,                # El dataframe con los datos
  dv = lswi,                   # La variable dependiente (lo que queremos analizar)
  wid = wetland_name,          # réplica o factor aleatorio entre réplicas (en este caso los humedales)
  within_full = .(year),       # variable interna dentro del diseño que se debe al tiempo + _full porque los valores no han sido condensados en una única cifra
  between=wetland_or_buffer,   # entre qué queremos comparar: factor
  detailed = TRUE              # Para obtener todos los resultados detallados
)
ez_results
# LSWI Significativo: wetland_or_buffer 0.01
# No significativo: hydro_periods 0.5, location 0.7, year 0.4, environmental_protection 0.2,
# protection_yes_no 0.5, conservation 0.06, sup_ha 0.4, perim_m 0.3, COD_IHA 0.3

# ASUNCIONES
modelo_lswi_HB <- lmer(lswi ~ wetland_or_buffer + (1 | wetland_name) + (1 | year), data = datos)  # A este se le comprueba todas las asunciones menos la de esfericidad

lillie.test(residuals(modelo_lswi_HB)) 			#NORMALIDAD LILLIEFORS
leveneTest(lswi ~ wetland_or_buffer,data=datos, center="median") 	#HOMOCEDASTICIDAD solo para variables categóricas

# PERIODS
ez_results.p <- ezANOVA(
  data = datos.p,                                # El dataframe con los datos
  dv =  ndvi_periods,                             # La variable dependiente (lo que queremos analizar)
  wid = wetland_name,                            # réplica o factor aleatorio entre réplicas (en este caso los humedales)
  within_full = .(year_periods),                 # variable interna dentro del diseño que se debe al tiempo + _full porque los valores no han sido condensados en una única cifra
  between= wetland_or_buffer, # entre qué queremos comparar: factor
  detailed = TRUE                                # Para obtener todos los resultados detallados
)
ez_results.p
#NDVI periodos: significativo: wetland_or_buffer
#no significativo: protection_yes_no, hydro_periods, location, environmental_protection, conservation 0.059, sup_ha, perim_m,COD_IHA


ez_results.p <- ezANOVA(
  data = datos.p,                                # El dataframe con los datos
  dv = lswi_periods,                             # La variable dependiente (lo que queremos analizar)
  wid = wetland_name,                            # réplica o factor aleatorio entre réplicas (en este caso los humedales)
  within_full = .(year_periods),                 # variable interna dentro del diseño que se debe al tiempo + _full porque los valores no han sido condensados en una única cifra
  between=wetland_or_buffer , # entre qué queremos comparar: factor
  detailed = TRUE                                # Para obtener todos los resultados detallados
)
ez_results.p


################################################################################
                          # GRÁFICOS CAJAS Y BIGOTES
################################################################################
# GRÁFICO WETLAND_OR_BUFFER LSWI
library(ggplot2)

# Crear un gráfico de cajas para mostrar la relación
ggplot(datos.p, aes(x = wetland_or_buffer, y = lswi_periods, fill = year_periods)) +
  geom_boxplot() +
  labs(title = "Relación entre LSWI y situación con respecto al humedal por periodos",
       x = "Humedal o buffer",
       y = "LSWI",
       fill = "Periodos de análisis") +
  theme_minimal() +
  scale_fill_manual(values = c("1999_2010" = "#DEB887", "2011_2021" = "#87CEFF")) +
  facet_wrap(~ year_periods)

ggplot(datos.p, aes(x = wetland_or_buffer, y = ndvi_periods, fill = year_periods)) +
  geom_boxplot() +
  labs(title = "Relación entre NDVI y situación con respecto al humedal por periodos",
       x = "Humedal o buffer",
       y = "NDVI",
       fill = "Periodos de análisis") +
  theme_minimal() +
  scale_fill_manual(values = c("1999_2010" = "#DEB887", "2011_2021" = "#87CEFF")) +
  facet_wrap(~ year_periods)

################################################################################
            # GRÁFICO PARA HUMEDAL/BUFFER Y COSTERO/INTERIOR #
################################################################################
# Creo la columna "periodo"
datos$periodo <- ifelse(datos$year <= 2010, 1, 2)
datos$periodo<-as.factor(datos$periodo)



################################################################################
            # GRÁFICO DE NDVI, NDWI, LSWI PARA HUMEDAL/BUFFER #
################################################################################

library(tidyr)  # Para reorganizar los datos
library(readxl)
library(ggplot2)

datos_comp <-read_excel("Excel/LSWI_zones_hum_buf.xlsx", sheet=4)
# Reorganizar los datos en formato 'long' para graficar fácilmente los índices
data_long <- pivot_longer(datos_comp, cols = c(LSWI, NDWI, NDVI), 
                          names_to = "index", values_to = "value")

# Crear el gráfico con ggplot2
ggplot(data_long, aes(x = year, y = value, color = index)) +
  geom_line(aes(linetype = wetland_or_buffer)) +
  facet_wrap(~ zone, scales = "free_y") +
  labs(title = "Evolución de los índices por zona",
       x = "Año",
       y = "Valor del índice",
       color = "Índice",
       linetype = "Wetland/Buffer") +
  theme_minimal() +
  theme(legend.position = "bottom")


#Segundo intento:
library(ggplot2)
library(dplyr)

# Definir los nombres personalizados para las zonas
nombres_zonas <- c(
  "Albufera Honda", "Albufera Nueva", "Barranco del Agua", "Cañada de las Norias",
  "Cola del embalse del Negratín", "Charcones de Punta-Entinas", "Humedales de Baza",
  "Laguna de la Gravera", "Rambla Morales", "Ribera de la Algaida", "Río Antas",
  "Salinas de Cabo de Gata", "Salar de los Canos", "Salinas de Cerrillos",
  "Saladar del Margen"
)

# Reasignar los nombres de las zonas en el dataframe
data_long <- data_long %>%
  mutate(zone = factor(zone, levels = 1:15, labels = nombres_zonas))

# Definir los colores para los índices
colores_indices <- c(LSWI = "blue", NDWI = "purple", NDVI = "green")

# Crear la gráfica con ejes y globales, colores personalizados y líneas más gruesas
ggplot(data_long, aes(x = year, y = value, color = index)) +
  geom_line(aes(linetype = wetland_or_buffer), linewidth = 1) +  # Aumentar el grosor de las líneas
  geom_hline(yintercept = 0, color = "black") +  # Línea horizontal en y = 0
  facet_wrap(~ zone) +  # Mantener el mismo eje "y" entre todas las gráficas
  labs(title = "Evolución de los índices por zona",
       x = "Año",
       y = "Valor del índice",
       color = "Índice",
       linetype = "Wetland/Buffer") +
  scale_color_manual(values = colores_indices) +  # Asignar colores personalizados
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10),  # Ajustar tamaño del texto de las etiquetas
    axis.title = element_text(size = 12),  # Ajustar tamaño de los títulos de los ejes
    axis.text = element_text(size = 10)    # Ajustar tamaño de los valores en los ejes
  )








library(ggplot2)
library(dplyr)
library(readxl)

# Leer los datos desde el archivo Excel
datos_comp2 <- read_excel("Excel/LSWI_zones_hum_buf.xlsx", sheet = 4, range = "A1:E691")

# Filtrar los datos para incluir solo LSWI y NDVI
datos_largos <- datos_comp2 %>%
  pivot_longer(cols = c(LSWI, NDVI), names_to = "index", values_to = "value")

# Definir los nombres personalizados para las zonas
nombres_zonas <- c(
  "Albufera Honda", "Albufera Nueva", "Barranco del Agua", "Cañada de las Norias",
  "Cola del embalse del Negratín", "Charcones de Punta-Entinas", "Humedales de Baza",
  "Laguna de la Gravera", "Rambla Morales", "Ribera de la Algaida", "Río Antas",
  "Salinas de Cabo de Gata", "Salar de los Canos", "Salinas de Cerrillos",
  "Saladar del Margen"
)

# Reasignar los nombres de las zonas en el dataframe
datos_largos <- datos_largos %>%
  mutate(zone = factor(zone, levels = 1:15, labels = nombres_zonas))

# Definir los colores para los índices
colores_indices <- c(LSWI = "blue", NDVI = "green")

# Crear la gráfica
ggplot(datos_largos, aes(x = year, y = value, color = index)) +
  geom_line(aes(linetype = wetland_or_buffer), size = 1) +  # Aumentar el grosor de las líneas
  geom_hline(yintercept = 0, color = "black") +  # Línea horizontal en y = 0
  facet_wrap(~ zone) +  # Mantener el mismo eje "y" entre todas las gráficas
  labs(title = "Evolución de LSWI y NDVI por zona",
       x = "Año",
       y = "Valor del índice",
       color = "Índice",
       linetype = "Wetland/Buffer") +
  scale_color_manual(values = colores_indices) +  # Asignar colores personalizados
  scale_linetype_manual(values = c("wetland" = "solid", "buffer" = "dashed")) +  # Especificar estilos de línea
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5),
    legend.position = "bottom",
    strip.text = element_text(size = 10),  # Ajustar tamaño del texto de las etiquetas
    axis.title = element_text(size = 12),  # Ajustar tamaño de los títulos de los ejes
    axis.text = element_text(size = 10)    # Ajustar tamaño de los valores en los ejes
  )





# # Crear el gráfico con ggplot2
# ggplot(data_long, aes(x = year, y = value, color = index, group = index)) +
#   geom_line() +
#   facet_wrap(~ wetland_or_buffer, nrow = 2) +  # Dividir los gráficos por 'wetland_or_buffer'
#   labs(
#     title = "Evolución de los índices LSWI, NDWI y NDVI (1999-2021)",
#     x = "Año",
#     y = "Índice",
#     color = "Índice"
#   ) +
#   geom_smooth() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Crear el gráfico con líneas diferenciadas por color de zona y facetas para humedal/buffer
# ggplot(data_long, aes(x = year, y = value, color = factor(zone), group = zone)) +
#   geom_line() +  # No usar `linetype` en este caso
#   facet_wrap(~ wetland_or_buffer, nrow = 2) +  # Dividir gráficos en humedal y buffer
#   labs(
#     title = "Evolución de los índices LSWI, NDWI y NDVI por zona (1999-2021)",
#     x = "Año",
#     y = "Índice",
#     color = "Zona"
#   ) +
#   scale_color_discrete(name = "Zona") +  # Asegurarse de que la leyenda muestre la zona
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# # Con promedios y desviación estándar: #####################################
# datos_comp <-read_excel("Excel/LSWI_zones_hum_buf.xlsx", sheet=4)
# datos_comp2<-datos_comp[-5] #sin NDWI
# # Reorganizar los datos en formato 'long' para graficar fácilmente los índices
# data_long2 <- pivot_longer(datos_comp2, cols = c(LSWI, NDVI), 
#                           names_to = "index", values_to = "value")
# 
# data_sum <- data_long2 %>%
#   group_by(wetland_or_buffer, zone, index) %>%
#   summarize(
#     mean_value = mean(value, na.rm = TRUE),
#     sd_value = sd(value, na.rm = TRUE)
#   )
# 
# # Gráfico de líneas con intervalos de desviación estándar
# ggplot(data_sum, aes(x = factor(zone), y = mean_value, group = index, color = index)) +
#   geom_line(linewidth = 1) +  # Línea para las medias
#   geom_ribbon(
#     aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, fill = index),
#     alpha = 0.2,  # Color gris para el área
#     color = NA
#   ) +
#   facet_wrap(~ wetland_or_buffer, scales = "free_y") +  # Divide por wetland_or_buffer
#   labs(
#     x = "Zona",
#     y = "Media de los índices",
#     title = "Valor de los índices por humedal y situación con respecto a este",
#     fill = "Índice",
#     color = "Índice"
#   ) +
#   scale_color_manual(values = c("LSWI" = "black", "NDWI" = "#D55E00", "NDVI" = "#009E73")) +  # Colores de las líneas
#   scale_fill_manual(values = c("LSWI" = "black", "NDWI" = "#D55E00", "NDVI" = "#009E73")) +  # Colores del relleno
#   coord_cartesian(ylim = c(-0.1, 0.2)) +
#   theme_minimal()











library(lme4)
modeloAMR <- lmer(lswi ~ wetland_or_buffer + location + ndvi + protection_yes_no + (1 | wetland_name) + (1 | year), data = datos)  # A este se le comprueba todas las asunciones menos la de esfericidad
modeloAMR2 <- lmer(lswi_periods ~ wetland_or_buffer + (1 | wetland_name) + (1 | year_periods), data = datos.p)  # A este se le comprueba todas las asunciones menos la de esfericidad
Anova(modeloAMR)
summary(modeloAMR)

#PAQUETES DE R: ASUNCIONES
library(nortest)
library(lmtest)
library(car)

shapiro.test(residuals(modeloAMR)) 			#NORMALIDAD
lillie.test(residuals(modeloAMR)) 			#NORMALIDAD LILLIEFORS
modelo<-lm(lswi ~ wetland_or_buffer + ndvi + protection_yes_no, data = datos)
bptest(modelo)		            			#HOMOCEDASTICIDAD
resettest(modeloAMR)	          			#LINEALIDAD
outlierTest(modeloAMR)         				#OUTLIER
vif(modeloAMR)                 				#REDUNDANCIA


# o también:
# ANOVA de medidas repetidas con aov()
modelo_aov <- aov(lswi ~ year + Error(wetland_name/year), data = datos)

# Resumen del modelo
summary(modelo_aov)

# Mostrar los resultados
print(ez_results)






library(readxl)
setwd("D:/Escritorio/TFM/rTFM")
datos_ndvi <-read_excel("Excel/NDVI_zones_hum_buf.xlsx", sheet=1)
# ANOVA de medidas repetidas con ezANOVA
library(ez)
datos_ndvi$wetland_name <- as.factor(datos_ndvi$wetland_name)
datos_ndvi$year <- as.factor(datos_ndvi$year)
datos_ndvi$protection_yes_no <- as.factor(datos_ndvi$protection_yes_no)


############REVISAR
ez_results <- ezANOVA(
  data = datos,                # El dataframe con los datos_ndvi
  dv = lswi,                   # La variable dependiente (lo que queremos analizar)
  wid = wetland_name,          # réplica o factor aleatorio entre réplicas (en este caso los humedales)
  within_full = .(year),       # variable interna dentro del diseño que se debe al tiempo + _full porque los valores no han sido condensados en una única cifra
  between=ndvi,                # entre qué queremos comparar: factor
  detailed = TRUE              # Para obtener todos los resulta
)
ez_results
