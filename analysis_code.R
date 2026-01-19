## ESTADISTICA DESCRIPTIVA 

#cargar datos
df <- read.csv("Students Social Media Addiction.csv", stringsAsFactors = FALSE)

#ver nombres de variables
names(df)

#resumen estadistico general
summary(df)

# Identificar columnas numéricas y categóricas
num_cols <- c("Age", "Avg_Daily_Usage_Hours", "Sleep_Hours_Per_Night",
              "Mental_Health_Score", "Conflicts_Over_Social_Media", "Addicted_Score")
num_cols <- num_cols[num_cols %in% names(df)]

cat_cols <- c("Gender", "Academic_Level", "Country", "Most_Used_Platform",
              "Affects_Academic_Performance", "Relationship_Status")
cat_cols <- cat_cols[cat_cols %in% names(df)]

# Medidas de tendencia central y dispersión (numéricas)
cat("\n==== 4) Medidas descriptivas: numéricas ====\n")
for (col in num_cols) {
  imprime_desc_num(df[[col]], col)
}

# Tablas de frecuencia (categóricas) 
cat("\n==== 5) Tablas de frecuencia (categóricas) ====\n")
for (col in cat_cols) {
  cat("\n>>", col, ":\n")
  print(table(df[[col]], useNA = "ifany"))
  cat("\nProporciones (%):\n")
  print(round(prop.table(table(df[[col]]))*100, 2))
}


## ANÁLISIS SIMPLE, MÚLTIPLE, ANOVA Y CHI CUADRADA

# Regresión lineal simple 
modelo_simple <- lm(Mental_Health_Score ~ Avg_Daily_Usage_Hours, 
                    data = df, na.action = na.omit)
summary(modelo_simple)

plot(df$Avg_Daily_Usage_Hours, df$Mental_Health_Score,
     xlab = "Horas de uso diario",
     ylab = "Puntaje Salud Mental",
     main = "Regresión lineal simple",
     pch = 19, col = "blue")
abline(modelo_simple, col = "red", lwd = 2)

# Regresión lineal múltiple 
modelo_multiple <- lm(Addicted_Score ~ Avg_Daily_Usage_Hours + Sleep_Hours_Per_Night + Age, data = df)
summary(modelo_multiple)

plot(modelo_multiple$fitted.values, df$Addicted_Score[!is.na(df$Addicted_Score)],
     xlab = "Valores ajustados",
     ylab = "Valores observados (Addicted Score)",
     main = "Regresión lineal múltiple",
     pch = 19, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)

# Anova
modelo_anova <- aov(Mental_Health_Score ~ Academic_Level, data = df)
summary(modelo_anova)

# Gráfico por nivel académico
boxplot(Mental_Health_Score ~ Academic_Level, data = df,
        main="Salud mental según nivel académico",
        xlab="Nivel académico", ylab="Puntaje de salud mental")

# Chi-cuadrada 
tab_chi <- table(df$Affects_Academic_Performance, df$Gender)
print(tab_chi)
chisq.test(tab_chi)

## HISTOGRAMAS Y ANÁLISIS

library(ggplot2)

# Age
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "#1f77b4", color = "black") +
  labs(title = "Distribución de edades",
       x = "Edad (años)", y = "Frecuencia") +
  theme_minimal()

# Avg_Daily_Usage_Hours
ggplot(df, aes(x = Avg_Daily_Usage_Hours)) +
  geom_histogram(binwidth = 0.5, fill = "#2ca02c", color = "black") +
  labs(title = "Uso diario de redes sociales",
       x = "Horas promedio por día", y = "Frecuencia") +
  theme_minimal()

# Sleep_Hours_Per_Night 
ggplot(df, aes(x = Sleep_Hours_Per_Night)) +
  geom_histogram(binwidth = 0.5, fill = "#d62728", color = "black") +
  labs(title = "Distribución de horas de sueño",
       x = "Horas de sueño por noche", y = "Frecuencia") +
  theme_minimal()

# Addicted_Score 
ggplot(df, aes(x = Addicted_Score)) +
  geom_histogram(binwidth = 2, fill = "#ff7f0e", color = "black") +
  labs(title = "Puntaje de adicción a redes",
       x = "Addicted Score", y = "Frecuencia") +
  theme_minimal()
