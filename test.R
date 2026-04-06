# packages
library(ggplot2)
library(spmodel)   # splm (spatial linear models)
library(viridis)   # colorblind friendly colors
library(patchwork) # put graphs together

# data
load("P5_Crop_Stress/potato.RData")

# Predictions
preds <- predict(spatial_lm, newdata = pred_data_final, interval = "prediction", level = 0.95)

# Attach back to our grid
pred_grid_results <- pred_data_final %>%
  mutate(
    fit = preds[,1],
    lwr = preds[,2],
    upr = preds[,3]
  )

# Plots
min_val <- min(c(pred_grid_results$fit, pred_grid_results$lwr, pred_grid_results$upr), na.rm = TRUE)
max_val <- max(c(pred_grid_results$fit, pred_grid_results$lwr, pred_grid_results$upr), na.rm = TRUE)
shared_limits <- c(min_val, max_val)

fitplot <- ggplot(pred_grid_results, aes(x = POINT_X, y = POINT_Y)) +
  geom_point(aes(color = fit), size = 1) +
  scale_color_viridis_c(limits = shared_limits) + # Syncing here
  labs(title = "Predicted CWSI (Fit)", x = "X coord", y = "Y coord", color = "CWSI") +
  theme_minimal()

lwrplot <- ggplot(pred_grid_results, aes(x = POINT_X, y = POINT_Y)) +
  geom_point(aes(color = lwr), size = 1) +
  scale_color_viridis_c(limits = shared_limits) + # Syncing here
  labs(title = "Lower 95% Bound", x = "X coord", y = "Y coord", color = "CWSI") +
  theme_minimal()

uprplot <- ggplot(pred_grid_results, aes(x = POINT_X, y = POINT_Y)) +
  geom_point(aes(color = upr), size = 1) +
  scale_color_viridis_c(limits = shared_limits) + # Syncing here
  labs(title = "Upper 95% Bound", x = "X coord", y = "Y coord", color = "CWSI") +
  theme_minimal()

(fitplot / (lwrplot + uprplot)) + 
  plot_layout(heights = c(4, 2), guides = "collect")& 
  coord_fixed() & 
  theme(legend.position = "right")

