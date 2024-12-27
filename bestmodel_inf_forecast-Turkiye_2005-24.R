# Clear all previous variables to avoid errors 
rm(list=ls()) 

###### Load required libraries #######
#to read excel (xlsx) files: 
#install.packages("readxl")
library(readxl)
#DPYLR: Grammar of DataManupilation
#install.packages("dplyr")
library(dplyr)
#Required library for ggplot function: ggplot2
#install.packages("ggplot2")
library(ggplot2)
#required library for lattice(TR:kafes) graph drawing: lattice
#provides functions related to combinatorial analysis: combinat
install.packages("combinat")
library(combinat)  # For generating variable combinations

##### LOADING DATA ####
# Load the dataset
enftahmintam <- read_excel("enftahmintam.xlsx")

# Create multiple lagged variables (1 to 12)
max_lag <- 12
for (i in 1:max_lag) {
  enftahmintam <- enftahmintam %>%
    mutate(!!paste0("lag_", i) := lag(ayenf, i))
}

# Remove rows with missing values due to lags
data <- enftahmintam[-(1:max_lag), ]


##### FUNCTION: Find Best Model with Highest R^2 #####
find_best_model <- function(data, response_var, max_predictors, max_lag) {
  best_r2 <- -Inf  # Initialize the highest R^2
  best_model <- NULL  # Store the best model
  best_combination <- NULL  # Store the best variable combination
  
  # Generate all lag variable names
  lagged_vars <- paste0("lag_", 1:max_lag)
  
  # Iterate over all possible combinations of lagged variables (1 to max_predictors)
  for (k in 1:max_predictors) {
    combinations <- combn(lagged_vars, k, simplify = FALSE)
    
    for (combo in combinations) {
      formula <- as.formula(paste(response_var, "~", paste(combo, collapse = " + ")))
      model <- lm(formula, data = data)
      r2 <- summary(model)$r.squared
      
      # Check if this model has the highest R^2 so far
      if (r2 > best_r2) {
        best_r2 <- r2
        best_model <- model
        best_combination <- combo
      }
    }
  }
  
  return(list(
    best_model = best_model,
    best_combination = best_combination,
    best_r2 = best_r2
  ))
}

##### Find Best Model #####
# Define parameters
response_var <- "ayenf"  # Dependent variable
max_predictors <- 4  # Maximum number of lagged variables to include in the model

# Run the function to find the best model
best_result <- find_best_model(data, response_var, max_predictors, max_lag)

# Extract results
best_model <- best_result$best_model
best_combination <- best_result$best_combination
best_r2 <- best_result$best_r2

##### Display Results #####
cat("Best Model Variables:\n")
print(best_combination)
cat("\nBest R^2 Value:\n", round(best_r2, 4), "\n")

cat("\nBest Model Summary:\n")
print(summary(best_model))

##### Visualization of Best Model Predictions #####
# Add model predictions to the data
data <- data %>%
  mutate(predicted = predict(best_model, data))

library(ggplot2)
ggplot(data, aes(x = 1:nrow(data))) +
  geom_line(aes(y = ayenf, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(
    title = "Actual vs. Predicted Values (Best Linear Model)",
    x = "Time Index",
    y = "Inflation Rate",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  )







############################## Perform prediction for the next months
next_month_prediction <- function(model, data, max_lag, months_ahead) {
  # Retrieve the most recent data row
  last_row <- tail(data, 1)
  
  predictions <- numeric(months_ahead)
  
  for (i in 1:months_ahead) {
    # Generate lagged variables
    lag_vars <- paste0("lag_", 1:max_lag)
    for (lag in 1:max_lag) {
      col_name <- paste0("lag_", lag)
      if (lag == 1) {
        last_row[[col_name]] <- last_row$ayenf  # Copy the most recent value
      } else {
        last_row[[col_name]] <- last_row[[paste0("lag_", lag - 1)]]
      }
    }
    
    # Make prediction
    pred <- predict(model, newdata = last_row)
    predictions[i] <- pred
    
    # Update the "ayenf" value with the new prediction
    last_row$ayenf <- pred
  }
  
  return(predictions)
}

###################################### Predict for the next 3 months
months_ahead <- 3
future_predictions <- next_month_prediction(best_model, data, max_lag, months_ahead)

cat("Future Predictions:\n")
print(future_predictions)


# Combine historical data and predictions for plotting
plot_data <- data.frame(
  Time = 1:(nrow(data) + length(future_predictions)),
  ayenf = c(data$ayenf, rep(NA, length(future_predictions))),
  predicted = c(data$predicted, future_predictions)
)

# Create a ggplot visualization
ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = ayenf, color = "Actual"), na.rm = TRUE) +
  geom_line(aes(y = predicted, color = "Predicted"), na.rm = TRUE) +
  geom_vline(xintercept = nrow(data), linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(
    title = "Actual vs. Predicted Values with Future Predictions",
    x = "Time Index",
    y = "Inflation Rate",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  )

###################################### Predict for the next 6 months
months_ahead <- 6
future_predictions <- next_month_prediction(best_model, data, max_lag, months_ahead)

# Combine historical data and predictions for plotting
plot_data <- data.frame(
  Time = 1:(nrow(data) + months_ahead),
  ayenf = c(data$ayenf, rep(NA, months_ahead)),
  predicted = c(data$predicted, future_predictions)
)

# Create a ggplot visualization
ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = ayenf, color = "Actual"), na.rm = TRUE) +
  geom_line(aes(y = predicted, color = "Predicted"), na.rm = TRUE) +
  geom_vline(xintercept = nrow(data), linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(
    title = "Actual vs. Predicted Values with 6-Month Forecast",
    x = "Time Index",
    y = "Inflation Rate",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom"
  )





