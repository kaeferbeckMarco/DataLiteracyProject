# Pakete laden
library(tidyverse)

# Daten laden
data <- read_csv("brain_tumor_dataset.csv")
 
# Datentypen vorbereiten
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Alter in numerisch umwandeln
data$Age <- as.numeric(data$Age)
data$Tumor_Size <- as.numeric(data$Tumor_Size)
data$Survival_Rate <- as.numeric(data$Survival_Rate)
data$Tumor_Growth_Rate <- as.numeric(data$Tumor_Growth_Rate)

# TRUE/FALSE für Yes/No-Spalten
yn_cols <- c("Radiation_Treatment", "Surgery_Performed", "Chemotherapy", "Family_History", "Follow_Up_Required")
data[yn_cols] <- lapply(data[yn_cols], function(x) tolower(as.character(x)) == "yes")

# Faktoren korrekt setzen
data$Gender <- as.factor(data$Gender)
data$Stage <- as.factor(data$Stage)
data$MRI_Result <- as.factor(data$MRI_Result)
data$Family_History <- as.factor(data$Family_History)

# Symptom-Spalten in 0/1 umwandeln (falls noch vorhanden)
symptoms_list <- c("Headache", "Nausea", "Seizures", "Vision Issues")
for (symptom in symptoms_list) {
  data[[symptom]] <- 0
}
for (i in 1:3) {
  col_name <- paste0("Symptom_", i)
  if (col_name %in% names(data)) {
    for (symptom in symptoms_list) {
      data[[symptom]] <- data[[symptom]] | (data[[col_name]] == symptom)
    }
  }
}
data[symptoms_list] <- lapply(data[symptoms_list], as.integer)
data <- data[ , !(names(data) %in% c("Symptom_1", "Symptom_2", "Symptom_3"))]

# Plot 1: Age distribution
hist(data$Age,
     main = "Age distribution",
     xlab = "Age",
     col = "skyblue",
     breaks = 10)

# Plot 2: Gender distribution
barplot(table(data$Gender),
        col = "purple",
        main = "Gender distribution")

# Plot 3: Tumor Stage (benign / malignant)
barplot(table(data$Stage),
        col = "tomato",
        main = "Tumor Stage (benign / malignant)")

# Plot 4: Comparison of Treatment Types (grouped barplot)
treatment_data <- data.frame(
  Treatment = c("Radiation", "Radiation", "Surgery", "Surgery", "Chemotherapy", "Chemotherapy"),
  Status = c("No", "Yes", "No", "Yes", "No", "Yes"),
  Count = c(
    sum(!data$Radiation_Treatment, na.rm = TRUE),
    sum(data$Radiation_Treatment, na.rm = TRUE),
    sum(!data$Surgery_Performed, na.rm = TRUE),
    sum(data$Surgery_Performed, na.rm = TRUE),
    sum(!data$Chemotherapy, na.rm = TRUE),
    sum(data$Chemotherapy, na.rm = TRUE)
  )
)
treatment_matrix <- matrix(treatment_data$Count, nrow = 2, byrow = FALSE)
colnames(treatment_matrix) <- c("Radiation", "Surgery", "Chemotherapy")
rownames(treatment_matrix) <- c("No", "Yes")
barplot(treatment_matrix,
        beside = TRUE,
        col = c("lightgray", "darkblue"),
        legend = rownames(treatment_matrix),
        main = "Comparison of Treatment Types",
        ylab = "Number of Patients")

# Plot 5: Survival Rate by Tumor Type
boxplot(Survival_Rate ~ Tumor_Type,
        data = data,
        main = "Survival Rate by Tumor Type",
        xlab = "Tumor Type",
        ylab = "Survival Rate (%)",
        col = "lightblue",
        las = 2)

# Plot 6: Survival Rate by Tumor Stage
boxplot(Survival_Rate ~ Stage,
        data = data,
        main = "Survival Rate by Tumor Stage",
        xlab = "Tumor Stage",
        ylab = "Survival Rate (%)",
        col = "tomato")

# Plot 7: Survival Rate by Gender
boxplot(Survival_Rate ~ Gender,
        data = data,
        main = "Survival Rate by Gender",
        xlab = "Gender",
        ylab = "Survival Rate (%)",
        col = "lightgreen")

# Plot 8: Survival Rate by Treatment Type (faceted boxplot)
plot_data <- data %>%
  select(Survival_Rate, Radiation_Treatment, Surgery_Performed, Chemotherapy) %>%
  mutate(across(everything(), as.factor)) %>%
  mutate(Survival_Rate = as.numeric(as.character(data$Survival_Rate)))  # Reinsert numeric

long_data <- pivot_longer(plot_data,
                          cols = c(Radiation_Treatment, Surgery_Performed, Chemotherapy),
                          names_to = "Treatment_Type",
                          values_to = "Treated")

ggplot(long_data, aes(x = Treated, y = Survival_Rate, fill = Treated)) +
  geom_boxplot() +
  facet_wrap(~Treatment_Type) +
  labs(title = "Survival Rate by Treatment Type",
       x = "Treatment (No / Yes)",
       y = "Survival Rate (%)") +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  theme_minimal()

# Plot 9: Average Survival Rate by Age (line plot)
age_survival <- data %>%
  group_by(Age) %>%
  summarise(Mean_Survival = mean(Survival_Rate, na.rm = TRUE))

ggplot(age_survival, aes(x = Age, y = Mean_Survival)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Average Survival Rate by Age",
       x = "Age",
       y = "Mean Survival Rate (%)") +
  theme_minimal()

