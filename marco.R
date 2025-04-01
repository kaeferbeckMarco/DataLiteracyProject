library(tidyverse)
df <- read_csv("brain_tumor_dataset.csv")
summary(df)
str(df)

df <- df %>%
  mutate(across(where(is.character), as.factor))

# Create list of unique symptoms
symptoms_list <- c("Headache", "Nausea", "Seizures", "Vision Issues")

# Initialize new binary columns with 0
for (symptom in symptoms_list) {
  df[[symptom]] <- 0
}

# Loop through Symptom_1 to Symptom_3 and update binary indicators
for (i in 1:3) {
  col_name <- paste0("Symptom_", i)
  for (symptom in symptoms_list) {
    df[[symptom]] <- df[[symptom]] | (df[[col_name]] == symptom)
  }
}

# Convert logicals to numeric (0/1)
df[symptoms_list] <- lapply(df[symptoms_list], as.integer)

# Drop the original Symptom_1 to Symptom_3 columns
df <- df[ , !(names(df) %in% c("Symptom_1", "Symptom_2", "Symptom_3"))]


str(df)

library(mlr3verse)
library(DoubleML)

library(data.table)

df_dt <- as.data.table(df)

# Then create the DoubleMLData object
dml_data <- DoubleMLData$new(
  df_dt,
  y_col = "Survival_Rate",
  d_cols = "Tumor_Size",
  x_cols = c("Age","Gender","Location","Histology","Family_History")
)



# Learner for outcome and treatment model
ml_g <- lrn("regr.ranger")  # for outcome model
ml_m <- lrn("regr.ranger")  # for treatment model

# Initialize the DoubleMLPLR (Partially Linear Regression) model
dml_plr <- DoubleMLPLR$new(data = dml_data,
                           ml_g = ml_g,
                           ml_m = ml_m,
                           n_folds = 5)

# Fit the model
dml_plr$fit()

# Print summary
print(dml_plr$summary())


