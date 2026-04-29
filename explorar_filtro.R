load("input/data/original/ELSOC_Long_2016_2023.RData")
library(dplyr)

cat("=== ESTRUCTURA BÁSICA ===\n")
cat("Dimensiones:", nrow(elsoc_long_2016_2023), "filas,", ncol(elsoc_long_2016_2023), "columnas\n\n")

cat("=== DISTRIBUCIÓN POR MUESTRA Y OLA ===\n")
print(table(elsoc_long_2016_2023$muestra, elsoc_long_2016_2023$ola))

cat("\n=== TIPO_ATRICIÓN ===\n")
print(table(elsoc_long_2016_2023$tipo_atricion, useNA="ifany"))

cat("\n=== CONTEO DE OLAS POR INDIVIDUO ===\n")
conteo <- elsoc_long_2016_2023 %>%
  filter(!is.na(ola)) %>%
  group_by(idencuesta, muestra) %>%
  summarize(n_olas = n(), olas_str = paste(sort(unique(ola)), collapse=","), .groups="drop")

# Tabla cruzada: muestra vs número de olas
cat("\nTabla: Muestra x Número de Olas\n")
print(table(conteo$muestra, conteo$n_olas))

cat("\n=== PRUEBA 1: Filtro estricto tipo_atricion == 1 ===\n")
filtro1 <- elsoc_long_2016_2023 %>%
  filter(tipo_atricion == 1) %>%
  group_by(idencuesta, muestra) %>%
  summarize(n_olas = n(), .groups="drop")

m1_strict <- nrow(filtro1 %>% filter(muestra == 1, n_olas == 7))
m2_strict <- nrow(filtro1 %>% filter(muestra == 2, n_olas == 5))
cat("Muestra 1 con 7 olas:", m1_strict, "\n")
cat("Muestra 2 con 5 olas:", m2_strict, "\n")
cat("Total:", m1_strict + m2_strict, "\n")

cat("\n=== PRUEBA 2: Sin filtro tipo_atrición, solo conteo de olas ===\n")
m1_count <- nrow(conteo %>% filter(muestra == 1, n_olas == 7))
m2_count <- nrow(conteo %>% filter(muestra == 2, n_olas == 5))
cat("Muestra 1 con 7 olas:", m1_count, "\n")
cat("Muestra 2 con 5 olas:", m2_count, "\n")
cat("Total:", m1_count + m2_count, "\n")

cat("\n=== PRUEBA 3: Filtrando tipo_atrición == 1 Y conteo exacto ===\n")
ids_m1 <- (elsoc_long_2016_2023 %>% 
  filter(tipo_atricion == 1, muestra == 1) %>% 
  group_by(idencuesta) %>% 
  summarize(n = n()) %>%
  filter(n == 7))$idencuesta

ids_m2 <- (elsoc_long_2016_2023 %>% 
  filter(tipo_atricion == 1, muestra == 2) %>% 
  group_by(idencuesta) %>% 
  summarize(n = n()) %>%
  filter(n == 5))$idencuesta

cat("Muestra 1 con 7 olas y tipo_atrición==1:", length(ids_m1), "\n")
cat("Muestra 2 con 5 olas y tipo_atrición==1:", length(ids_m2), "\n")
cat("Total:", length(ids_m1) + length(ids_m2), "\n")

cat("\n=== PRUEBA 4: Análisis detallado de participantes con problema ===\n")
# Ver cuáles son los individuos que tienen tipo_atricion != 1 pero llegaron a 7 olas
test_m1 <- elsoc_long_2016_2023 %>%
  filter(muestra == 1) %>%
  group_by(idencuesta) %>%
  summarize(
    n_olas = n(),
    tipos_atricion = paste(unique(tipo_atricion), collapse=","),
    .groups = "drop"
  ) %>%
  filter(n_olas == 7)

cat("De los", nrow(test_m1), "individuos en muestra 1 con 7 olas:\n")
print(table(test_m1$tipos_atricion))
