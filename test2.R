library(tidyverse)
data <- read_csv("brain_tumor_dataset.csv")
summary(data)
str(data)

data <- data %>%
  mutate(across(where(is.character), as.factor))

# Create list of unique symptoms
symptoms_list <- c("Headache", "Nausea", "Seizures", "Vision Issues")

# Initialize new binary columns with 0
for (symptom in symptoms_list) {
  data[[symptom]] <- 0
}

# Loop through Symptom_1 to Symptom_3 and update binary indicators
for (i in 1:3) {
  col_name <- paste0("Symptom_", i)
  for (symptom in symptoms_list) {
    data[[symptom]] <- data[[symptom]] | (data[[col_name]] == symptom)
  }
}

# Convert logicals to numeric (0/1)
data[symptoms_list] <- lapply(data[symptoms_list], as.integer)

# Drop the original Symptom_1 to Symptom_3 columns
data <- data[ , !(names(data) %in% c("Symptom_1", "Symptom_2", "Symptom_3"))]


str(data)


# Überblick verschaffen
head(data)
str(data)
summary(data)
 

# Alter als Zahl umwandeln
data$Age <- as.numeric(data$Age)

# Zusammenfassung
mean(data$Age, na.rm = TRUE)
median(data$Age, na.rm = TRUE)
summary(data$Age)

# Histogramm
hist(data$Age, main = "Age distribution", xlab = "Alter", col = "skyblue", breaks = 10)

# Tumor Type Verteilung
table(data$Tumor_Type)

# Stadium (Stage): Gutartig vs Bösartig
table(data$Stage)

# Balkendiagramm
barplot(table(data$Tumor_Type), las=2, col = "orange", main = "Bar Plot of Tumor Types")

barplot(table(data$Stage), col = "tomato", main = "Tumor Stage (benign / malignant)")
# Geschlecht
table(data$Gender)

# Balkenplot
barplot(table(data$Gender), col = "purple", main = "Gender distribution")

View(data)

# Pakete laden 
library(dplyr)

# CSV laden
data <- read.csv("brain_tumor_dataset.csv", stringsAsFactors = FALSE)

# Alter in numerisch umwandeln
data$Age <- as.numeric(data$Age)

# 1. Gesamt-Zusammenfassung
summary(data)

# 2. Durchschnittsalter nach Geschlecht
data %>%
  group_by(Gender) %>%
  summarise(Anzahl = n(),
            Durchschnittsalter = mean(Age, na.rm = TRUE))

# 3. Durchschnittsalter nach Tumor-Typ
data %>%
  group_by(Tumor_Type) %>%
  summarise(Anzahl = n(),
            Durchschnittsalter = mean(Age, na.rm = TRUE))

# 4. Stadium nach Geschlecht aufschlüsseln
data %>%
  group_by(Stage, Gender) %>%
  summarise(Anzahl = n())

# Pakete laden
library(dplyr)

# CSV-Datei laden
data <- read.csv("brain_tumor_dataset.csv", stringsAsFactors = FALSE)

# Patient_ID ignorieren
data <- data[ , -which(names(data) == "Patient_ID")]

# Zahlen umwandeln
data$Age <- as.numeric(data$Age)
data$Tumor_Size <- as.numeric(data$Tumor_Size)
data$Survival_Rate <- as.numeric(data$Survival_Rate)
data$Tumor_Growth_Rate <- as.numeric(data$Tumor_Growth_Rate)

# Ja/Nein-Spalten in TRUE/FALSE umwandeln
yn_cols <- c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy", "Family_History", "Follow_Up_Required")
data[yn_cols] <- lapply(data[yn_cols], function(x) tolower(x) == "yes")

# Überblick
View(data)
str(data)
summary(data)



# Häufigkeiten (TRUE/FALSE zählen → TRUE = Ja, FALSE = Nein)
treatment_data <- data.frame(
  Treatment = c("Radiation", "Radiation", "Surgery", "Surgery", "Chemotherapy", "Chemotherapy"),
  Status = c("No", "Yes", "No", "Yes", "No", "Yes"),
  Count = c(
    sum(data$Radiation_Treatment == FALSE, na.rm = TRUE),
    sum(data$Radiation_Treatment == TRUE, na.rm = TRUE),
    sum(data$Surgery_Performed == FALSE, na.rm = TRUE),
    sum(data$Surgery_Performed == TRUE, na.rm = TRUE),
    sum(data$Chemotherapy == FALSE, na.rm = TRUE),
    sum(data$Chemotherapy == TRUE, na.rm = TRUE)
  )
)

# Umstrukturieren für barplot (Matrix erforderlich)
treatment_matrix <- matrix(treatment_data$Count, nrow = 2, byrow = FALSE)
colnames(treatment_matrix) <- c("Radiation", "Surgery", "Chemotherapy")
rownames(treatment_matrix) <- c("No", "Yes")

# Gruppiertes Balkendiagramm
barplot(treatment_matrix,
        beside = TRUE,
        col = c("lightgray", "darkblue"),
        legend = rownames(treatment_matrix),
        main = "Comparison of Treatment Types",
        ylab = "Number of Patients")

boxplot(Survival_Rate ~ Tumor_Type,
        data = data,
        main = "Survival Rate by Tumor Type",
        xlab = "Tumor Type",
        ylab = "Survival Rate (%)",
        col = "lightblue",
        las = 2)  # Kippt die x-Achse-Beschriftung für bessere Lesbarkeit

boxplot(Survival_Rate ~ Stage,
        data = data,
        main = "Survival Rate by Tumor Stage",
        xlab = "Tumor Stage",
        ylab = "Survival Rate (%)",
        col = "tomato")

boxplot(Survival_Rate ~ Gender,
        data = data,
        main = "Survival Rate by Gender",
        xlab = "Gender",
        ylab = "Survival Rate (%)",
        col = "lightgreen")

# Regression: Survival Rate als Zielvariable
model <- lm(Survival_Rate ~ Age + Gender + Tumor_Size + Stage +
              Radiation_Treatment + Surgery_Performed + Chemotherapy,
            data = data)

# Zusammenfassung des Modells
summary(model)

model2 <- lm(Survival_Rate ~ Age + Gender + Tumor_Size + Stage +
               Radiation_Treatment + Surgery_Performed + Chemotherapy +
               Tumor_Growth_Rate + Family_History + MRI_Result,
             data = data)

summary(model2)

data$Stage <- as.factor(data$Stage)
data$Gender <- as.factor(data$Gender)
data$MRI_Result <- as.factor(data$MRI_Result)
data$Family_History <- as.factor(data$Family_History)

# Pakete laden
library(dplyr)

# CSV einlesen
data <- read.csv("brain_tumor_dataset.csv", stringsAsFactors = FALSE)

# Nicht benötigte Spalte entfernen
data <- data[ , -which(names(data) == "Patient_ID")]

# Datentypen vorbereiten
data$Age <- as.numeric(data$Age)
data$Tumor_Size <- as.numeric(data$Tumor_Size)
data$Survival_Rate <- as.numeric(data$Survival_Rate)
data$Tumor_Growth_Rate <- as.numeric(data$Tumor_Growth_Rate)

# TRUE/FALSE für Ja/Nein-Spalten
yn_cols <- c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy", "Family_History", "Follow_Up_Required")
data[yn_cols] <- lapply(data[yn_cols], function(x) tolower(x) == "yes")

# Faktoren korrekt setzen
data$Gender <- as.factor(data$Gender)
data$Stage <- as.factor(data$Stage)
data$MRI_Result <- as.factor(data$MRI_Result)
data$Family_History <- as.factor(data$Family_History)

# Regressionsmodell erweitern
model2 <- lm(Survival_Rate ~ Age + Gender + Tumor_Size + Stage +
               Radiation_Treatment + Surgery_Performed + Chemotherapy +
               Tumor_Growth_Rate + Family_History + MRI_Result,
             data = data)

# Zusammenfassung ausgeben
summary(model2)

# Diagnoseplots
par(mfrow = c(2, 2))  # 4 Plots auf einmal
plot(model2)

install.packages("randomForest")

library(randomForest)

# Wieder: Kategorische Variablen umwandeln
data$Stage <- as.factor(data$Stage)
data$Gender <- as.factor(data$Gender)
data$MRI_Result <- as.factor(data$MRI_Result)
data$Family_History <- as.factor(data$Family_History)

# Modell: Survival Rate vorhersagen
set.seed(123)  # Für Reproduzierbarkeit
rf_model <- randomForest(Survival_Rate ~ Age + Gender + Tumor_Size + Stage +
                           Radiation_Treatment + Surgery_Performed + Chemotherapy +
                           Tumor_Growth_Rate + Family_History + MRI_Result,
                         data = data,
                         importance = TRUE,
                         ntree = 500)

# Modellzusammenfassung
print(rf_model)

# Wichtige Prädiktoren anzeigen
importance(rf_model)
varImpPlot(rf_model)

print(rf_model)

varImpPlot(rf_model)

install.packages("corrplot")

library(corrplot)

# Nur numerische Spalten auswählen
numeric_data <- data %>% 
  select_if(is.numeric)

# Korrelation berechnen
cor_matrix <- cor(numeric_data, use = "complete.obs")


# Pakete laden
library(dplyr)
library(tidyr)
library(ggplot2)

# Daten vorbereiten
plot_data <- data %>%
  select(Survival_Rate, Radiation_Treatment, Surgery_Performed, Chemotherapy)

# Sicherstellen: Survival_Rate ist numerisch
plot_data$Survival_Rate <- as.numeric(plot_data$Survival_Rate)

# Treatments als Faktor (aber nur die Treatments!)
plot_data$Radiation_Treatment <- as.factor(plot_data$Radiation_Treatment)
plot_data$Surgery_Performed <- as.factor(plot_data$Surgery_Performed)
plot_data$Chemotherapy <- as.factor(plot_data$Chemotherapy)

# Long-Format für ggplot
long_data <- pivot_longer(plot_data,
                          cols = c(Radiation_Treatment, Surgery_Performed, Chemotherapy),
                          names_to = "Treatment_Type",
                          values_to = "Treated")

# Plot zurücksetzen
par(mfrow = c(1, 1))

# Boxplot zeichnen
ggplot(long_data, aes(x = Treated, y = Survival_Rate, fill = Treated)) +
  geom_boxplot() +
  facet_wrap(~Treatment_Type) +
  labs(title = "Survival Rate by Treatment Type",
       x = "Treatment (No / Yes)",
       y = "Survival Rate (%)") +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  theme_minimal()

# Group by age and calculate mean survival rate
library(dplyr)
age_survival <- data %>%
  group_by(Age) %>%
  summarise(Mean_Survival = mean(Survival_Rate, na.rm = TRUE))

# Plot
ggplot(age_survival, aes(x = Age, y = Mean_Survival)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Average Survival Rate by Age",
       x = "Age",
       y = "Mean Survival Rate (%)") +
  theme_minimal()

