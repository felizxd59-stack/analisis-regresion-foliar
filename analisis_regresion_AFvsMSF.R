# ============================================================================
# ANÁLISIS DE REGRESIÓN LINEAL SIMPLE
# Relación entre Área Foliar (AF) y Masa Seca Foliar (MSF)
# ============================================================================

# Cargar librerías necesarias
library(ggplot2)
library(gridExtra)
library(lmtest)
library(nortest)

# ============================================================================
# 1. CARGAR DATOS
# ============================================================================

# Suponer que tu dataframe se llama "AFvsMSF"
# Con columnas: "AF" (Área foliar) y "MSF" (Masa seca foliar)

# Si necesitas cargar los datos desde CSV:
# AFvsMSF <- read.csv("tu_archivo.csv")

# Para este ejemplo, utilizaremos el dataframe AFvsMSF que ya existe
# AFvsMSF <- AFvsMSF  # Asegurar que el dataframe está disponible

# Verificar que los datos se cargaron correctamente
cat("Dimensiones del dataframe:", dim(AFvsMSF), "\n")
cat("Primeras filas:\n")
print(head(AFvsMSF))
cat("\n")

# ============================================================================
# 2. ANÁLISIS DE REGRESIÓN LINEAL SIMPLE
# ============================================================================

cat("\n========== ANÁLISIS DE REGRESIÓN LINEAL SIMPLE ==========\n\n")

# Crear el modelo de regresión: MSF ~ AF
modelo <- lm(MSF ~ AF, data = AFvsMSF)

# Resumen del modelo
summary(modelo)

# Extraer valores importantes
r_squared <- summary(modelo)$r.squared
r_squared_adj <- summary(modelo)$adj.r.squared
p_value <- summary(modelo)$coefficients[2, 4]
coef_AF <- summary(modelo)$coefficients[2, 1]
intercept <- summary(modelo)$coefficients[1, 1]
se_coef_AF <- summary(modelo)$coefficients[2, 2]
se_intercept <- summary(modelo)$coefficients[1, 2]
t_value_AF <- summary(modelo)$coefficients[2, 3]
rmse <- sqrt(mean(residuals(modelo)^2))

# Crear ecuación del modelo
ecuacion <- paste0("MSF = ", round(intercept, 4), " + ", round(coef_AF, 4), " × AF")

cat("\nECUACIÓN DEL MODELO:\n")
cat(ecuacion, "\n\n")

# ============================================================================
# 3. PRUEBA DE SUPUESTOS DE LA REGRESIÓN
# ============================================================================

cat("\n========== PRUEBA DE SUPUESTOS DE LA REGRESIÓN ==========\n\n")

# 3.1 NORMALIDAD DE LOS RESIDUOS (Test de Shapiro-Wilk)
test_normalidad <- shapiro.test(residuals(modelo))
cat("1. TEST DE NORMALIDAD (Shapiro-Wilk):\n")
cat("   ────────────────────────────────────────\n")
cat("   H0: Los residuos siguen una distribución normal\n")
cat("   Estadístico W:", round(test_normalidad$statistic, 4), "\n")
cat("   p-value:", round(test_normalidad$p.value, 4), "\n")
if(test_normalidad$p.value > 0.05) {
  cat("   ✓ RESULTADO: Los residuos SÍ siguen distribución normal (p > 0.05)\n")
  cat("   ✓ CONCLUSIÓN: Se cumple el supuesto de normalidad\n\n")
} else {
  cat("   ✗ RESULTADO: Los residuos NO siguen distribución normal (p < 0.05)\n")
  cat("   ✗ CONCLUSIÓN: NO se cumple el supuesto de normalidad\n\n")
}

# 3.2 HOMOCEDASTICIDAD (Test de Breusch-Pagan)
test_homocedasticidad <- bptest(modelo)
cat("2. TEST DE HOMOCEDASTICIDAD (Breusch-Pagan):\n")
cat("   ────────────────────────────────────────\n")
cat("   H0: La varianza de los residuos es constante\n")
cat("   Estadístico BP:", round(test_homocedasticidad$statistic, 4), "\n")
cat("   p-value:", round(test_homocedasticidad$p.value, 4), "\n")
if(test_homocedasticidad$p.value > 0.05) {
  cat("   ✓ RESULTADO: Existe homocedasticidad (p > 0.05)\n")
  cat("   ✓ CONCLUSIÓN: La varianza es constante\n\n")
} else {
  cat("   ✗ RESULTADO: No existe homocedasticidad (p < 0.05)\n")
  cat("   ✗ CONCLUSIÓN: La varianza NO es constante\n\n")
}

# 3.3 INDEPENDENCIA DE RESIDUOS (Test de Durbin-Watson)
test_independencia <- dwtest(modelo)
cat("3. TEST DE INDEPENDENCIA (Durbin-Watson):\n")
cat("   ────────────────────────────────────────\n")
cat("   H0: Los residuos son independientes\n")
cat("   Estadístico DW:", round(test_independencia$statistic, 4), "\n")
cat("   p-value:", round(test_independencia$p.value, 4), "\n")
cat("   Nota: DW ≈ 2 indica independencia\n")
if(test_independencia$p.value > 0.05) {
  cat("   ✓ RESULTADO: Los residuos son independientes (p > 0.05)\n")
  cat("   ✓ CONCLUSIÓN: No hay autocorrelación\n\n")
} else {
  cat("   ✗ RESULTADO: Los residuos NO son independientes (p < 0.05)\n")
  cat("   ✗ CONCLUSIÓN: Existe autocorrelación\n\n")
}

# 3.4 LINEALIDAD (Correlación de Pearson)
correlacion <- cor.test(AFvsMSF$AF, AFvsMSF$MSF)
cat("4. TEST DE LINEALIDAD (Correlación de Pearson):\n")
cat("   ────────────────────────────────────────\n")
cat("   Coeficiente de correlación (r):", round(correlacion$estimate, 4), "\n")
cat("   p-value:", round(correlacion$p.value, 4), "\n")
if(correlacion$p.value < 0.05) {
  cat("   ✓ RESULTADO: Existe relación lineal significativa (p < 0.05)\n")
  cat("   ✓ CONCLUSIÓN: La relación lineal es estadísticamente significativa\n\n")
} else {
  cat("   ✗ RESULTADO: No existe relación lineal significativa (p > 0.05)\n")
  cat("   ✗ CONCLUSIÓN: La relación lineal NO es significativa\n\n")
}

# ============================================================================
# 4. ESTADÍSTICAS ADICIONALES
# ============================================================================

cat("\n========== ESTADÍSTICAS DEL MODELO ==========\n\n")
cat("BONDAD DE AJUSTE:\n")
cat("   R² (Coeficiente de determinación):", round(r_squared, 6), "\n")
cat("   Interpretación: Explica el", round(r_squared*100, 2), "% de la variabilidad en MSF\n")
cat("   R² ajustado:", round(r_squared_adj, 6), "\n")
cat("   RMSE (Error cuadrático medio):", round(rmse, 6), "\n\n")

cat("COEFICIENTES DEL MODELO:\n")
cat("   Intercepto (β₀):", round(intercept, 6), "\n")
cat("   Error estándar:", round(se_intercept, 6), "\n")
cat("   Pendiente (β₁):", round(coef_AF, 6), "\n")
cat("   Error estándar:", round(se_coef_AF, 6), "\n")
cat("   t-value:", round(t_value_AF, 4), "\n\n")

cat("SIGNIFICANCIA DEL MODELO:\n")
cat("   p-value de AF:", round(p_value, 6), "\n")
if(p_value < 0.05) {
  cat("   ✓ La variable AF es significativa a nivel α = 0.05\n\n")
} else {
  cat("   ✗ La variable AF NO es significativa a nivel α = 0.05\n\n")
}

cat("INFORMACIÓN DEL CONJUNTO DE DATOS:\n")
cat("   Número de observaciones (n):", nrow(AFvsMSF), "\n")
cat("   Grados de libertad:", modelo$df.residual, "\n")
cat("   Correlación de Pearson (r):", round(correlacion$estimate, 4), "\n\n")

# ============================================================================
# 5. GRÁFICO DE DISPERSIÓN CON ECUACIÓN, R², Y PROBABILIDAD
# ============================================================================

grafico_principal <- ggplot(AFvsMSF, aes(x = AF, y = MSF)) +
  # Puntos de datos
  geom_point(color = "#2E86AB", size = 3.5, alpha = 0.7, shape = 19) +
  # Línea de regresión con intervalo de confianza
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72", fill = "#F18F01", alpha = 0.2, size = 1.2) +
  # Tema minimalista
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#333333", margin = margin(b = 15)),
    axis.title = element_text(size = 13, face = "bold", color = "#333333"),
    axis.text = element_text(size = 11, color = "#555555"),
    panel.grid.major = element_line(color = "gray90", size = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  ) +
  # Etiquetas
  labs(
    title = "Análisis de Regresión: Área Foliar vs Masa Seca Foliar",
    subtitle = paste0(ecuacion, " | R² = ", round(r_squared, 4), " | p-value = ", formatC(p_value, format = "e", digits = 2)),
    x = "Área Foliar - AF (cm²)",
    y = "Masa Seca Foliar - MSF (gramos)"
  ) +
  # Anotaciones con información estadística
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste0(
             "R² = ", round(r_squared, 4), "\n",
             "p-value = ", formatC(p_value, format = "e", digits = 2), "\n",
             "n = ", nrow(AFvsMSF), "\n",
             "r = ", round(correlacion$estimate, 4)
           ),
           hjust = 1.05, vjust = 1.15, 
           size = 4.5, 
           fontface = "bold",
           color = "#2E86AB",
           bbox = list(boxcolour = "#F18F01", fill = "white", alpha = 0.9, size = 1.2))

# Mostrar el gráfico
print(grafico_principal)

# ============================================================================
# 6. GRÁFICOS DE DIAGNÓSTICO DE LOS RESIDUOS
# ============================================================================

cat("\n========== GENERANDO GRÁFICOS DE DIAGNÓSTICO ==========\n\n")

# Crear una figura con 4 gráficos de diagnóstico
png(filename = "diagnosticos_residuos.png", width = 1200, height = 900, res = 120)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo, which = c(1, 2, 3, 5), cex = 1.2, cex.lab = 1.3, cex.main = 1.3)
mtext("Gráficos de Diagnóstico de Residuos - Modelo de Regresión AF vs MSF", 
      outer = TRUE, cex = 1.4, font = 2, line = 0.5)
dev.off()

cat("✓ Gráficos de diagnóstico guardados como 'diagnosticos_residuos.png'\n")

# ============================================================================
# 7. GRÁFICO ADICIONAL: Q-Q PLOT (NORMALIDAD)
# ============================================================================

grafico_qq <- ggplot(data.frame(residuos = residuals(modelo)), aes(sample = residuos)) +
  stat_qq(color = "#2E86AB", size = 3, alpha = 0.7) +
  stat_qq_line(color = "#A23B72", size = 1.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  ) +
  labs(
    title = "Gráfico Q-Q: Prueba de Normalidad de Residuos",
    x = "Cuantiles Teóricos",
    y = "Cuantiles de Muestra"
  )

print(grafico_qq)

# ============================================================================
# 8. GRÁFICO ADICIONAL: RESIDUOS VS VALORES AJUSTADOS
# ============================================================================

grafico_residuos <- ggplot(data.frame(
  ajustados = fitted(modelo),
  residuos = residuals(modelo)
), aes(x = ajustados, y = residuos)) +
  geom_point(color = "#2E86AB", size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#A23B72", linetype = "dashed", size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, color = "#F18F01", size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  ) +
  labs(
    title = "Residuos vs Valores Ajustados (Homocedasticidad)",
    x = "Valores Ajustados",
    y = "Residuos"
  )

print(grafico_residuos)

# ============================================================================
# 9. GUARDAR GRÁFICOS PRINCIPALES
# ============================================================================

ggsave("regresion_AFvsMSF.png", grafico_principal, width = 11, height = 8, dpi = 300)
cat("✓ Gráfico principal guardado como 'regresion_AFvsMSF.png'\n")

ggsave("grafico_qq_plot.png", grafico_qq, width = 9, height = 7, dpi = 300)
cat("✓ Gráfico Q-Q guardado como 'grafico_qq_plot.png'\n")

ggsave("grafico_residuos.png", grafico_residuos, width = 9, height = 7, dpi = 300)
cat("✓ Gráfico de residuos guardado como 'grafico_residuos.png'\n")

# ============================================================================
# 10. RESUMEN FINAL COMPLETO
# ============================================================================

cat("\n")
cat("═════════════════════════════════════════════════════════════════\n")
cat("                    RESUMEN FINAL DEL ANÁLISIS\n")
cat("═════════════════════════════════════════════════════════════════\n\n")

cat("📊 MODELO DE REGRESIÓN:\n")
cat("   ", ecuacion, "\n\n")

cat("📈 BONDAD DE AJUSTE:\n")
cat("   R² =", round(r_squared, 6), "→", round(r_squared*100, 2), "% de variabilidad explicada\n")
cat("   R² ajustado =", round(r_squared_adj, 6), "\n")
cat("   RMSE =", round(rmse, 6), "\n\n")

cat("📋 COEFICIENTES:\n")
cat("   Intercepto (β₀) =", round(intercept, 6), "±", round(se_intercept, 6), "\n")
cat("   Pendiente (β₁) =", round(coef_AF, 6), "±", round(se_coef_AF, 6), "\n")
cat("   t-value =", round(t_value_AF, 4), "\n")
cat("   p-value =", formatC(p_value, format = "e", digits = 2), "\n\n")

cat("✅ VERIFICACIÓN DE SUPUESTOS:\n")
cat("   1. Normalidad (Shapiro-Wilk):")
if(test_normalidad$p.value > 0.05) cat(" ✓ CUMPLE") else cat(" ✗ NO CUMPLE")
cat(" (p =", round(test_normalidad$p.value, 4), ")\n")

cat("   2. Homocedasticidad (Breusch-Pagan):")
if(test_homocedasticidad$p.value > 0.05) cat(" ✓ CUMPLE") else cat(" ✗ NO CUMPLE")
cat(" (p =", round(test_homocedasticidad$p.value, 4), ")\n")

cat("   3. Independencia (Durbin-Watson):")
if(test_independencia$p.value > 0.05) cat(" ✓ CUMPLE") else cat(" ✗ NO CUMPLE")
cat(" (DW =", round(test_independencia$statistic, 4), ")\n")

cat("   4. Linealidad (Correlación de Pearson):")
if(correlacion$p.value < 0.05) cat(" ✓ SIGNIFICATIVA") else cat(" ✗ NO SIGNIFICATIVA")
cat(" (r =", round(correlacion$estimate, 4), ")\n\n")

cat("📌 INFORMACIÓN DEL CONJUNTO:\n")
cat("   N observaciones =", nrow(AFvsMSF), "\n")
cat("   Grados de libertad =", modelo$df.residual, "\n\n")

cat("💾 ARCHIVOS GENERADOS:\n")
cat("   • regresion_AFvsMSF.png\n")
cat("   • diagnosticos_residuos.png\n")
cat("   • grafico_qq_plot.png\n")
cat("   • grafico_residuos.png\n\n")

cat("═════════════════════════════════════════════════════════════════\n")
