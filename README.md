# Proyecto Final – Índice de Personal Ocupado (IPO)

Este proyecto analiza la evolución del Índice de Personal Ocupado en la industria manufacturera uruguaya (2018–2024), aplicando técnicas de series temporales (ARIMA, descomposición STL) y modelos de machine learning (Random Forest) para identificar los sectores con mayor incidencia en el índice total.

## Estructura del proyecto

Este proyecto cuenta con tres subcarpetas. 

### Código
- `proyecto-final.R`: script principal con el procesamiento de datos, detección de outliers, modelado ARIMA y análisis por sectores.

### Datos

Contiene el archivo .xlsx con los datos que se utilizan en  `proyecto-final.qmd`: `IVFIM_IPO.xlsx`. Provienen del portal del INE: https://www.gub.uy/instituto-nacional-estadistica/.

### Reporte
- `proyecto-final.qmd`: Reporte principal de Quarto que incluye el desarrollo del análisis, gráficos y conclusiones.