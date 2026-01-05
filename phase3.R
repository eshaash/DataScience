# ==========================================================
# GLOBAL HOUSING MARKET ANALYSIS
# Phase 1: Data Preparation
# Phase 2: EDA & Visualization
# Phase 3: Predictive Modeling & Machine Learning
# ==========================================================

# -------------------------------
# STEP 0: LOAD LIBRARIES
# -------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(randomForest)

# -------------------------------
# STEP 1: DATA IMPORT
# -------------------------------
data <- read_csv("C:/Users/azeem/Downloads/global_housing_market_extended.csv")

# -------------------------------
# STEP 2: FEATURE SELECTION
# -------------------------------
data <- data %>%
  select(
    Country,
    Year,
    `House Price Index`,
    `Affordability Ratio`,
    `Urbanization Rate (%)`,
    `Population Growth (%)`
  )

# -------------------------------
# STEP 3: DATA CLEANING
# -------------------------------
data$Country <- trimws(data$Country)
data <- data %>% distinct()

# -------------------------------
# STEP 4: DATA TRANSFORMATION
# -------------------------------

# Price-to-Income Ratio
data <- data %>%
  mutate(
    Price_to_Income_Ratio =
      `House Price Index` / `Affordability Ratio`
  )

# Urban Growth Category
data <- data %>%
  mutate(
    Urban_Growth_Category = case_when(
      `Urbanization Rate (%)` < 65 ~ "Low Urban Growth",
      `Urbanization Rate (%)` >= 65 & `Urbanization Rate (%)` < 80 ~ "Medium Urban Growth",
      `Urbanization Rate (%)` >= 80 ~ "High Urban Growth",
      TRUE ~ NA_character_
    )
  )

# -------------------------------
# STEP 5: COUNTRY-LEVEL SUMMARY
# -------------------------------
city_level_data <- data %>%
  group_by(Country) %>%
  summarise(
    Avg_House_Price_Index = mean(`House Price Index`, na.rm = TRUE),
    Avg_Affordability_Ratio = mean(`Affordability Ratio`, na.rm = TRUE),
    Avg_Price_to_Income_Ratio = mean(Price_to_Income_Ratio, na.rm = TRUE),
    Avg_Urbanization_Rate = mean(`Urbanization Rate (%)`, na.rm = TRUE),
    Avg_Population_Growth = mean(`Population Growth (%)`, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------
# STEP 6: EDA NUMERIC DATA
# -------------------------------
numeric_data <- data %>%
  select(
    `House Price Index`,
    `Affordability Ratio`,
    Price_to_Income_Ratio,
    `Urbanization Rate (%)`,
    `Population Growth (%)`
  )

# -------------------------------
# STEP 7: CORRELATION MATRIX
# -------------------------------
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)

# -------------------------------
# STEP 8: MODELING DATASET
# -------------------------------
model_data <- numeric_data %>% drop_na()

# -------------------------------
# STEP 9: TRAIN–TEST SPLIT
# -------------------------------
set.seed(123)

train_index <- sample(
  seq_len(nrow(model_data)),
  size = 0.7 * nrow(model_data)
)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# ==========================================================
# PHASE 3: PREDICTIVE MODELING
# ==========================================================

# -------------------------------
# STEP 10: LINEAR REGRESSION
# -------------------------------
lm_model <- lm(
  `House Price Index` ~
    `Affordability Ratio` +
    `Urbanization Rate (%)` +
    `Population Growth (%)` +
    Price_to_Income_Ratio,
  data = train_data
)

summary(lm_model)

# -------------------------------
# STEP 11: LINEAR MODEL EVALUATION
# -------------------------------
lm_predictions <- predict(lm_model, test_data)

lm_rmse <- sqrt(
  mean((lm_predictions - test_data$`House Price Index`)^2)
)

lm_mae <- mean(
  abs(lm_predictions - test_data$`House Price Index`)
)

cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Linear Regression MAE :", lm_mae, "\n")

# -------------------------------
# STEP 12: RANDOM FOREST MODEL
# -------------------------------
rf_model <- randomForest(
  `House Price Index` ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

print(rf_model)

# -------------------------------
# STEP 13: RANDOM FOREST EVALUATION
# -------------------------------
rf_predictions <- predict(rf_model, test_data)

rf_rmse <- sqrt(
  mean((rf_predictions - test_data$`House Price Index`)^2)
)

rf_mae <- mean(
  abs(rf_predictions - test_data$`House Price Index`)
)

cat("Random Forest RMSE:", rf_rmse, "\n")
cat("Random Forest MAE :", rf_mae, "\n")

# -------------------------------
# STEP 14: FEATURE IMPORTANCE
# -------------------------------
importance(rf_model)
varImpPlot(
  rf_model,
  main = "Feature Importance (Random Forest)"
)

# -------------------------------
# STEP 15: MODEL COMPARISON
# -------------------------------
model_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  RMSE  = c(lm_rmse, rf_rmse),
  MAE   = c(lm_mae, rf_mae)
)

print(model_comparison)

# -------------------------------
# STEP 16: PAIR PLOT (OPTIONAL)
# -------------------------------
GGally::ggpairs(model_data)

# ==========================================================
# END OF FILE
# ==========================================================
