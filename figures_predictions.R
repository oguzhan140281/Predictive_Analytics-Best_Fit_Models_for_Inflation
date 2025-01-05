
###################################### Visualization of Prediction for the next 3 months
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

###################################### Visualization of Prediction for the next 6 months
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