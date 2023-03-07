# Gary-Auten
Code for Case Study
Linear v. Bayesian Regression
# Load the required packages
library(dbplyr)
library(rstanarm)

# Read in the data
data <- read.csv("ROS-Examples-master/toyotacorolladata.csv")


# Select the predictor variables and outcome variable
data <- select(data, Age, KM, Weight, HP, CC, QuartTax, Fuel_Type, Price)

# Convert the Fuel_Type variable to a factor
data$Fuel_Type <- factor(data$Fuel_Type)

# Fit a linear regression model
lm_model <- lm(Price ~ Age + KM + Weight + HP + CC + QuartTax + Fuel_Type, data = data)

# Fit a Bayesian regression model without specifying family, prior_intercept, or prior_aux
bayes_model <- stan_glm(Price ~ Age + KM + Weight + HP + CC + QuartTax + Fuel_Type, data = data)

# Compute the mean standard error of the linear regression model
lm_mse <- sqrt(mean((data$Price - predict(lm_model))^2))

# Compute the mean standard error and R-squared of the Bayesian regression model
bayes_mse <- sqrt(mean((data$Price - predict(bayes_model))^2))
bayes_rsq <- as.numeric(cor(data$Price, predict(bayes_model))^2)

# Print the results
cat("Mean standard error of linear regression model:", lm_mse, "\n")
cat("Mean standard error of Bayesian regression model:", bayes_mse, "\n")
cat("R-squared of linear regression model:", summary(lm_model)$r.squared, "\n")
cat("R-squared of Bayesian regression model:", bayes_rsq, "\n")



ELPD, R2 Values
library(rstanarm)
data <- read.csv("ROS-Examples-master/toyotacorolladata.csv")
model <- stan_glm(Price ~ Age + KM + QuartTax + Fuel_Type + Weight + HP + CC, data = data)

posterior_linpred <- posterior_linpred(model)


posterior_predictive <- exp(posterior_linpred)


y_pred <- apply(posterior_predictive, 1, mean)

 
y_mean <- mean(data$Price)
ss_tot <- sum((data$Price - y_mean)^2)
ss_res <- sum((data$Price - y_pred)^2)
r_squared <- 1 - ss_res / ss_tot


cat("R-squared:", round(r_squared, 3), "\n")

loo <- loo(model, k_threshold = 0.7)


if (any(loo$diagnostics$p_k > 0.7)) {
 
  r2_loo <- 1 - loo$pseudo_BMA$log_pd / log(mean(data$Price)^2)
  
  n <- nrow(data)
  p <- 7  
  adj_r2 <- 1 - (1 - r2_loo) * ((n - 1) / (n - p - 1))
  
  cat("Age:", round(adj_r2[1], 3), "\n")
  cat("KM:", round(adj_r2[2], 3), "\n")
  cat("QuartTax:", round(adj_r2[3], 3), "\n")
  cat("Fuel_Type:", round(adj_r2[4], 3), "\n")
  cat("Weight:", round(adj_r2[5], 3), "\n")
  cat("HP:", round(adj_r2[6], 3), "\n")
  cat("CC:", round(adj_r2[7], 3), "\n")
} else {
 
  r2_loo <- 1 - loo$pointwise[, "elpd_loo"] / log(mean(data$Price)^2)
  
  n <- nrow(data)
  p <- 7  
  adj_r2 <- 1 - (1 - r2_loo) * ((n - 1) / (n - p - 1))
  
  cat("Age:", round(adj_r2[1], 3), "\n")
  cat("KM:", round(adj_r2[2], 3), "\n")
  cat("QuartTax:", round(adj_r2[3], 3), "\n")
  cat("Fuel_Type:", round(adj_r2[4], 3), "\n")
  cat("Weight:", round(adj_r2[5], 3), "\n")
  cat("HP:", round(adj_r2[6], 3), "\n")
  cat("CC:", round(adj_r2[7], 3), "\n")
}


elpd_values <- loo$pointwise[, "elpd_loo"]

predictors <- c("Age", "CC", "Weight", "KM", "QuartTax", "Fuel_Type", "HP")


data_subset <- data[, predictors]


elpd_matrix <- matrix(0, nrow = length(elpd_values), ncol = length(predictors), dimnames = list(NULL, predictors))
for (i in 1:length(elpd_values)) {
  elpd_matrix[i, ] <- as.numeric(data_subset[i, ]) * elpd_values[i]
}

total_elpd_vars <- colSums(elpd_matrix)
prop_elpd_vars <- total_elpd_vars / sum(elpd_values)


cat("Summary of ELPD values by predictor variable:\n")
for (i in 1:length(prop_elpd_vars)) {
  cat(predictors[i], ":", round(prop_elpd_vars[i], 3), "\n")
}
