library(tidyverse)
df <- read_csv("brain_tumor_dataset.csv")
summary(df)
str(df)

df <- df %>%
  mutate(across(where(is.character), as.factor))

str(df)
