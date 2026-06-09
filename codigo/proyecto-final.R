#Prueba final Certificación Profesional en Ciencia de Datos

library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(urca)        
library(forecast)    
library(tsoutliers) 
library(randomForest)
library(xgboost)
library(caret)
library(corrplot)
library(tidyverse)
library(ggthemes)
library(tsibble)  
library(fable)      
library(fabletools)
library(feasts) 
library(gt)


# 1) ¿Cómo varía el Indice de Personal Ocupado?

# Carga y preproceso la base del INE (Solo nos trae la industria manufacturera)

baseipo <- read_excel("datos/IVFIM_IPO.xlsx")
str(baseipo)

#Transformo los datos para quedarme con las filas 7 a 81
#En realidad la columna 4 ya es el total de toda la industria manufacturera, C es la agrupacion, por lo que podemos usar este dato:

datosipo <- data.frame(baseipo[7:81,4])
datosipo <- mutate(datosipo, ...4 = as.numeric(...4))
datosipo_log <- log(datosipo)

summary(datosipo)

#Como tengo mis datos ordenados continúo creando la serie temporal mensual 2018 - 2024

ts_IPO <- ts(datosipo,start = c(2018,1), end = c(2024,3), frequency = 12)
ts_IPO_log <- ts(datosipo_log,start = c(2018,1), end = c(2024,3), frequency = 12)

# Creamos el vector de fechas basado en la serie ts_IPO

ini  <- start(ts_IPO)
n    <- length(ts_IPO)  # <-- ESTA línea evita el error 'closure'
fechas <- seq(as.Date("2018-01-01"), by = "month", length.out = 75)

#Creamos el gráfico de la seríe para poder ver la evolución

par(mfrow=c(1,1))
plot(ts_IPO_log, main = "Evolución mensual IPO (en log)", ylab = "log(Índice de Personal Ocupado)", xlab = "Tiempo")

#2). ¿Se observa algún dato atípico en el empleo en los últimos años?

#Primero pasamos la serie logarítmica a niveles

ipo_nivel <- exp(ts_IPO_log)  

#Intento con tsoutliers (puede no marcar shocks prolongados)

out_nivel <- try(tsoutliers(ipo_nivel), silent = TRUE)

plot(fechas, ipo_nivel, type = "l", main = "IPO (nivel) con outliers (tsoutliers, si aplica)", xlab = "", ylab = "Índice")

if (!inherits(out_nivel, "try-error") && length(out_nivel$index) > 0) {points(fechas[out_nivel$index], ipo_nivel[out_nivel$index], pch = 19, col = "red")}

#Detección manual en diferencias (más sensible a saltos tipo 2020)

ts_ipo_diff  <- diff(ipo_nivel)     # diferencias de la serie en niveles
fechas_diff  <- fechas[-1]          # diff pierde el primer dato

#Umbral de “equilibrio” (sensibilidad): más bajo = más sensible

umbral_z <- 2.0

z <- as.numeric(scale(ts_ipo_diff))
idx_out <- which(abs(z) > umbral_z)

out_tbl_diff <- data.frame(
  Fecha  = fechas_diff[idx_out],
  Valor  = ts_ipo_diff[idx_out],
  Zscore = z[idx_out])

# Gráfico de diferencias con outliers

plot(fechas_diff, ts_ipo_diff, type = "l",
     main = sprintf("Δ IPO con outliers manuales (|z| > %.1f)", umbral_z),
     xlab = "", ylab = "Δ Índice")

if (length(idx_out) > 0) {
  points(out_tbl_diff$Fecha, out_tbl_diff$Valor, pch = 19, col = "blue")
  text(out_tbl_diff$Fecha, out_tbl_diff$Valor,
       labels = format(out_tbl_diff$Fecha, "%Y-%m"), pos = 3, cex = 0.7)}

print(out_tbl_diff)

# Calcular media y desvío estándar

media_log <- mean(datosipo$...4)
desvio_log <- sd(datosipo$...4)

# Gráfico con ±3 desvíos estándar
ggplot(datosipo, aes(x = fechas, y = ...4)) +
  geom_line(color = "darkgreen", linewidth = 0.9) +
  geom_point(color = "seagreen4", size = 2) +
  geom_hline(yintercept = media_log, color = "blue", linetype = "solid", linewidth = 1) +
  geom_hline(yintercept = media_log + 3 * desvio_log, color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = media_log - 3 * desvio_log, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Evolución del logaritmo del Índice de Personal Ocupado (IPO)",
    subtitle = "Líneas de media (azul) y ±3 desvíos estándar (rojas)",
    x = "fechas",
    y = "log(IPO)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 2020q2 Probamos diferentes outliers AO, TC, LS

#Excluimos la columna 4 (total industria manufacturera)
vals <- baseipo |>
  dplyr::slice(7:81) |>
  dplyr::pull(4)

#Si vino como list-column, aplanamos
if (is.list(vals)) vals <- unlist(vals, use.names = FALSE)

# Convertir a numérico soportando coma decimal y separador de miles
IPO <- parse_number(as.character(vals),
                    locale = locale(decimal_mark = ",", grouping_mark = "."))

# Armar el data.frame final con el log
datosipo <- tibble(
  fechas = fechas,
  IPO = IPO
) |>
  dplyr::mutate(log_IPO = log(IPO))

# Creo una función auxiliar para ubicar posiciones por año-mes

idx <- function(anio, mes) which(format(fechas, "%Y-%m") == sprintf("%04d-%02d", anio, mes))

#AO: Additive Outlier (pulso puntual). Un “1” solo en el mes del outlier, “0” en el resto.

AO_2020Q2 <- rep(0, n)
AO_2020Q2[idx(2020, 4)] <- 1
AO_2020Q2[idx(2020, 5)] <- 1
AO_2020Q2[idx(2020, 6)] <- 1  #Para marcar todo el trimestre
plot(AO_2020Q2, type="l", main="AO en 2020-04", col="red") #Gráfico del mes 04-2020

#LS: Level Shift (cambio de nivel permanente a partir del shock) Cero antes del shock y uno desde el shock en adelante.

LS_2020_04 <- as.integer(fechas >= as.Date("2020-04-01"))
plot(LS_2020_04, type="l", main="LS desde 2020-04", col="blue")

#TC: Transitory Change (cambio transitorio que decae geométricamente) 1 en el mes del shock y luego decae con razón delta (0<delta<1). Elegí p.ej. delta = 0.7 (podés probar 0.5–0.9).

delta <- 0.7
TC_2020_04 <- rep(0, n)
t0 <- idx(2020, 4)
TC_2020_04[t0:n] <- delta^(0:(n - t0))

#graficamos el cambio transitorio 
plot(TC_2020_04, type = "l",
     main = sprintf("TC 2020-04 (delta = %.1f)", delta),
     col = "darkgreen", lwd = 2,
     ylab = "Valor", xlab = "Tiempo")
abline(v = t0, col = "gray50", lty = 2)


#Modelo Arima con intervenciones

# AO solo
fit_AO <- auto.arima(ts_IPO_log, xreg = AO_2020Q2, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# LS solo
fit_LS <- auto.arima(ts_IPO_log, xreg = LS_2020_04, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# TC solo
fit_TC <- auto.arima(ts_IPO_log, xreg = TC_2020_04, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Comparar AICc
c(AO = fit_AO$aicc, LS = fit_LS$aicc, TC = fit_TC$aicc)

#Evaluamos la combinación AO + TC

X_AO_TC <- cbind(AO_2020Q2, TC_2020_04)
fit_AO_TC <- auto.arima(ts_IPO_log, xreg = X_AO_TC, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
fit_AO_TC$aicc

#Serie ajustada (removiendo el efecto del outlier). En este caso nos conviene el AO por ser un caso puntual

coef_AO <- coef(fit_AO)["xreg"]
ipo_ajust_AO <- ts_IPO_log  - coef_AO * AO_2020Q2
ts.plot(cbind(ts_IPO_log, ipo_ajust_AO), col=c("black","red"),
        main="IPO original (negro) vs ajustado AO (rojo)",
        ylab="log(IPO)"); 
legend("bottomleft", legend=c("Original","Ajustado AO"),
       col=c("black","red"), lty=1, bty="n")


#Probamos para el LS y TC

coef_LS <- coef(fit_LS)["xreg"]
ipo_ajust_LS <- ts_IPO_log - coef_LS * LS_2020_04
ts.plot(cbind(ts_IPO_log, ipo_ajust_LS), col=c("black","red"), main="IPO original (negro) vs ajustado LS (rojo)", ylab="log(IPO)")
legend("bottomleft", legend=c("Original","Ajustado LS"), col=c("black","red"), lty=1, bty="n")


coef_TC <- coef(fit_TC)["xreg"]
ipo_ajust_TC <- ts_IPO_log - coef_TC * TC_2020_04
ts.plot(cbind(ts_IPO_log, ipo_ajust_TC), col=c("black","red"), main="IPO original (negro) vs ajustado TC (rojo)", ylab="log(IPO)")
legend("bottomleft", legend=c("Original","Ajustado tc"), col=c("black","red"), lty=1, bty="n")

#Unimos los tres graficos para comparar

# Coeficientes de las intervenciones (robusto al nombre)
coef_xreg <- function(fit, k = 1){
 
   # toma el k-ésimo coef de xreg en el modelo
  idx <- grep("^xreg", names(coef(fit)))
  if (length(idx) < k) stop("No encontré coeficiente xreg #", k, " en el modelo.")
  coef(fit)[idx[k]]
}

b_AO <- as.numeric(coef_xreg(fit_AO, 1))  # si tu AO es un único reg
b_LS <- as.numeric(coef_xreg(fit_LS, 1))
b_TC <- as.numeric(coef_xreg(fit_TC, 1))

#Series ajustadas en LOG(IPO) (resto el efecto estimado del xreg)
ipo_ajust_AO_log <- as.numeric(ts_IPO_log) - b_AO * AO_2020Q2
ipo_ajust_LS_log <- as.numeric(ts_IPO_log) - b_LS * LS_2020_04
ipo_ajust_TC_log <- as.numeric(ts_IPO_log) - b_TC * TC_2020_04

# Data frame largo para comparar en FACETs
df_comp <- tibble(
  Fecha       = as.Date(fechas),
  Original    = as.numeric(ts_IPO_log),
  Ajustado_AO = ipo_ajust_AO_log,
  Ajustado_TC = ipo_ajust_TC_log,
  Ajustado_LS = ipo_ajust_LS_log
) %>%
  # pasamos a largo por modelo (AO/TC/LS)
  pivot_longer(cols = c(Ajustado_AO, Ajustado_TC, Ajustado_LS),
               names_to = "Modelo", values_to = "Ajustado") %>%
  mutate(Modelo = recode(Modelo,
                         "Ajustado_AO" = "AO",
                         "Ajustado_TC" = "TC",
                         "Ajustado_LS" = "LS"))

#Plot: tres paneles alineados, misma escala, original vs ajustado
ggplot(df_comp, aes(x = Fecha)) +
  geom_line(aes(y = Original, color = "Original"), linewidth = 0.9) +
  geom_line(aes(y = Ajustado, color = "Ajustado"), linewidth = 0.9) +
  facet_wrap(~ Modelo, nrow = 1) +
  scale_color_manual(values = c("Original" = "black", "Ajustado" = "red")) +
  labs(title = "Comparación de intervenciones sobre log(IPO)",
       subtitle = "Cada panel muestra: serie original (negro) vs. ajustada (rojo)",
       x = "Tiempo", y = "log(IPO)", color = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

#3). ¿El índice de personal ocupado presenta estacionalidad y estacionariedad? Usamos La serie ajustada AO

# Creamos un tsibble en nivel y en log (ajustado)

ipo_ajust_nivel <- exp(ipo_ajust_AO)

fechas_ajust <- seq(
  from = as.Date(sprintf("%d-%02d-01", start(ts_IPO)[1], start(ts_IPO)[2])),
  by = "month",
  length.out = length(ts_IPO))

dts_ajust <- tsibble(
  Fecha     = yearmonth(fechas_ajust),
  IPO       = as.numeric(ipo_ajust_nivel),      # nivel ajustado
  IPO_log   = as.numeric(ipo_ajust_AO),         # log ajustado
  index     = Fecha)

# Gráficos base (visualización)

# Serie en nivel (ajustada)

dts_ajust %>%
  ggplot(aes(x = Fecha, y = IPO)) +
  geom_line(color = "#0072B2") +
  labs(title = "Índice de Personal Ocupado (nivel) — Ajustado AO",
       x = "Tiempo", y = "Índice (nivel)") +
  theme_minimal(base_size = 12)

# Estacionalidad por año (nivel)

gg_season(dts_ajust, IPO) +
  labs(title = "Estacionalidad del IPO (nivel) — Ajustado AO",
       x = "Mes", y = "Índice") +
  theme_minimal(base_size = 12)

# Serie en log (ajustada)

dts_ajust %>%
  ggplot(aes(x = Fecha, y = IPO_log)) +
  geom_line(color = "#E69F00") +
  labs(title = "Índice de Personal Ocupado (log) — Ajustado AO",
       x = "Tiempo", y = "log(IPO)") +
  theme_minimal(base_size = 12)

# Estacionalidad por año (log)

gg_season(dts_ajust, IPO_log) +
  labs(title = "Estacionalidad del IPO (log) — Ajustado AO",
       x = "Mes", y = "log(IPO)") +
  theme_minimal(base_size = 12)

# 2) ACF / PACF 

# Correlogramas sobre la serie en log (ajustada)

ACF(dts_ajust, y = IPO_log)  %>% autoplot() +
  labs(title = "FAC del IPO (log) — Ajustado AO")

PACF(dts_ajust, y = IPO_log) %>% autoplot() +
  labs(title = "FACP del IPO (log) — Ajustado AO")

# ACF/PACF sobre la primera diferencia del log (estacionarizado)

ACF(dts_ajust, y = difference(IPO_log, 1))  %>% autoplot() +
  labs(title = "FAC de Δ IPO (log) — Ajustado AO")

PACF(dts_ajust, y = difference(IPO_log, 1)) %>% autoplot() +
  labs(title = "FACP de Δ IPO (log) — Ajustado AO")

# 3) Descomposición STL (estilo IMS)

dts_ajust %>%
  model(stl = STL(IPO_log)) %>%
  components() %>%
  autoplot() +
  labs(title = "Descomposición STL del IPO (log) — Ajustado AO")


# 4) Test de estacionariedad (ADF) sobre la serie ajustada

ts_ipo_ajust <- ts(dts_ajust$IPO_log, start = c(2018, 1), frequency = 12)
adf_nivel <- ur.df(ts_ipo_ajust, type = "drift", selectlags = "AIC")
adf_diff  <- ur.df(diff(ts_ipo_ajust), type = "drift", selectlags = "AIC")

cat("\n### ADF IPO (log) — Ajustado AO (nivel):\n")
print(summary(adf_nivel))
cat("\n### ADF Δ IPO (log) — Ajustado AO (primera diferencia):\n")
print(summary(adf_diff))

# 5) Modelo ARIMA (fpp3) + diagnóstico y pronóstico
# Ajuste ARIMA automático sobre el log ajustado

md_fin_AO <- dts_ajust %>% model(ARIMA(IPO_log))

report(md_fin_AO)
gg_tsresiduals(md_fin_AO)  # diagnóstico de residuos al estilo clase

# Predicción a 2 años
fc_AO <- forecast(md_fin_AO, h = "2 years")

# Gráfico de predicción (como en el apunte)
dts_ajust %>%
  filter_index("2019 Jan" ~ .) %>%
  autoplot(IPO_log) +
  autolayer(fc_AO) +
  labs(title = "Pronóstico IPO (log) — ARIMA sobre serie ajustada AO",
       x = "Tiempo", y = "log(Índice de Personal Ocupado)") +
  theme_minimal(base_size = 12)

#4) ¿Qué sector tiene más peso en el índice? ¿Todos los sectores se comportan igual?

#Primero filtramos los datos relevantes: columnas con sectores

sectores <- baseipo[7:82, 4:27]
colnames(sectores) <- c(  "Total_Industria", "Alimentos", "Bebidas", "Tabaco", "Textiles", "Prendas_de_Vestir", "Cueros",  "Madera", "Papel", "Impresion",  "Refineria", "Quimicos", "Farmaceuticos",  "Caucho_Plastico", "Minerales_no_metalicos", "Metales_Comunes", "Prod_Metalicos", "Electronicos", "Equipo_Electrico",  "Maquinaria", "Vehiculos", "Otros_Transporte",  "Muebles", "Otras_Industrias", "Reparacion_Maquinaria")

#Forzamos TODO a character para evitar choques de tipos

sectores <- sectores %>%
  mutate(across(everything(), ~as.character(.))) %>%
  mutate(across(everything(), ~trimws(.))) %>%            # limpia espacios
  
#Marcar como NA los símbolos no divulgables / vacíos
  
mutate(across(everything(), ~na_if(., "(s)"))) %>% mutate(across(everything(), ~na_if(., ""))) %>%
  
#Convertir a número con coma decimal

    mutate(across(everything(), ~suppressWarnings( parse_number(., locale = locale(decimal_mark = ",", grouping_mark = ".")))))

#Quitar columnas completamente vacías (por ej., Tabaco u Otros transp.)

sectores <- sectores[, colSums(!is.na(sectores)) > 0, drop = FALSE]

#Imputar NAs restantes con promedio de cada columna

sectores <- sectores %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

#Calculamos la importancia de los sectores utilizando un modelo Random Forest donde  y = Total_Insutria; x = el resto de sectores

y <- sectores$Total_Industria
X <- sectores %>% select(-Total_Industria)

#Por mayor seguridad quitamos las columnas con varianza 0

var_ok <- sapply(X, function(v) sd(v, na.rm = TRUE) > 0)
X <- X[, var_ok, drop = FALSE]

set.seed(123)
rf_model <- randomForest(
  x = X, y = y, importance = TRUE, ntree = 500)

#Importancia

imp <- as.data.frame(importance(rf_model))
imp$Sector <- rownames(imp)
rownames(imp) <- NULL
if (!"IncNodePurity" %in% names(imp)) {
  # fallback si la columna se llama distinto en tu versión
  names(imp)[1] <- "IncNodePurity"}
imp <- imp %>% arrange(desc(IncNodePurity))

#Mostramos top 10

cat("\n--- Top 10 divisiones por importancia (Random Forest) ---\n")
print(head(imp, 10))

#Gráficamos la importancia por Random Ferost

ggplot(imp, aes(x = reorder(Sector, IncNodePurity), y = IncNodePurity)) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(title = "Importancia de cada sector en el Total Industrial (Random Forest)",
       x = "Sector (División CIIU)",
       y = "Importancia (IncNodePurity)") + theme_minimal(base_size = 12)

imp %>%
  arrange(desc(IncNodePurity)) %>%
  gt() %>%
  tab_header(title = md("*Importancia de cada sector en el Total Industrial (Random Forest)*"),
             subtitle = "Medida: IncNodePurity — mayor valor indica")

#Realizamos correlaciones con el total industrial

corr_mat <- cor(sectores, use = "pairwise.complete.obs")
corr_total <- corr_mat[, "Total_Industria"] %>% sort(decreasing = TRUE)
print(round(corr_total, 3))

#Graficamos las correlaciones

corrplot(corr_mat, method = "color", type = "upper",   tl.col = "black", tl.srt = 60, tl.cex = 0.5, number.cex = 0.5, title = "Correlaciones entre sectores y el Total Industrial", mar = c(0,0,2,0))

#Buscamos los 10 sectores que se encuentran más correlacionados con el total

corr_df <- data.frame(Sector = names(corr_total), Correlacion = corr_total)
corr_df %>%
  filter(Sector != "Total_Industria") %>%
  slice_max(order_by = Correlacion, n = 10)

#Realizamos gráfico de barras de los 10 sectores con mayor correlación

corr_df %>%
  filter(Sector != "Total_Industria") %>%
  arrange(desc(Correlacion)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Sector, Correlacion), y = Correlacion)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Sectores más relacionados con el Total Industrial",
       x = "Sector", y = "Correlación con el total") +
  theme_minimal(base_size = 12)

