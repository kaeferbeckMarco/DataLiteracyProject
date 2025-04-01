library(tidyverse)
df <- read_csv("brain_tumor_dataset.csv")
summary(df)
str(df)

df <- df %>%
  mutate(across(where(is.character), as.factor))

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


