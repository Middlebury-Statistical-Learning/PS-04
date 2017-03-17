# Setup -------------------------------------------------------------------
#Bianca Gonzalez - Machine Learning - March 17th, 2017
library(tidyverse)
library(broom)

# Let these be:
# -true function f(x) for a one-dimensional predictor x
# -the standard deviation of the true epsilon i.e. the noise
f <- function(x) {
  x^2                           #NOISE
}
sigma <- 0.3

# This function generates a random sample. Random in terms of both
# -the predictor x
# -the amount of noise epsilon.
generate_sample <- function(f, n_sim, sigma) {
  data_frame(
    x = runif(n = n_sim, min = 0, max = 1),
    f_x = f(x),
    eps = rnorm(n = n_sim, mean = 0, sd = sigma),
    y = f_x + eps
  )
}

# Generate Training Data via Sampling -------------------------------------
train_data <- generate_sample(f, n_sim = 100, sigma = sigma)

train_data
# The true model f(x) is in red. We observe points (x,y). Here we are pretending
# to know the exact break down of y into f(x) and eps
ggplot(train_data, aes(x=x)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=f_x), col="red", size=1)

# In a realistic situation, we only know x & y, not f(x) & eps. So we should
# remove them
train_data <- train_data %>%
  select(x, y)
train_data

  #function of x [predictors]
  #eps (epsilon - error - noise)

# Generate Test Data, Fit Model, and Get Predictions -------------------

# 1. Generate test data via sampling. Recall in test data, we only have the
# predictors x:
test_data <- generate_sample(f, n_sim = 100, sigma = sigma) %>%
  select(x)
train_data
test_data
# 2. Fit Model: Simple linear regression
model <- lm(y ~ x, data = train_data)
train_data_augmented <- model %>%
  augment() %>%
  tbl_df()
# Plot fitted model to training data
ggplot(data=train_data_augmented, aes(x=x)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=.fitted), col="blue")

# 3. Get Predictions
# Note: we use augment(newdata=BLAH) here, since it works. This is equivalent
# to using the predict() function.
predictions <- model %>%
  augment(newdata=test_data) %>%
  tbl_df() %>%
  .[[".fitted"]]

str(predictions)


# Main For Loop -------------------------------------------------
set.seed(76)
n_sim <- 100

# Create new test data: a single point
x0 <- 0.95
test_data <- data_frame(x=x0)
test_data

# Save predictions here
predictions <- rep(0, n_sim)

for(i in 1:n_sim) {
  
# 1. Sample new train data - used fun generate sample with 100 sims
  # so for every simulation, create new dataset of 100, make model on that datset, 
  # predict on one single point, and then save the predictions on each single point. 
  
  train_data_for <- generate_sample(f, n_sim, sigma = sigma) # Using 100 simulations
  train_data_for <- train_data %>%
    select(x, y)
  
# 2. Fit Model: Simple linear regression
  model <- lm(y ~ x, data = train_data_for)
  
# 3. Get predictions at single point .95 already defined ^
  one_guess <- predict(model, newdata = test_data)

# 4. Save all predicted values for each simulation
  predictions[i] <- one_guess  
 
  if(i %% 100 == 0){
    print(i)
  }
}

# Pull the Curtain! Reveal truth: i.e. show all components we really know, but
# pretended not to:
set.seed(76)
truth <- data_frame(
  x = rep(x0, n_sim),
  f_x = f(x),
  eps = rnorm(n = n_sim, mean = 0, sd = 0.3),
  y = f_x + eps
  )

# Add predictions
truth <- truth %>%
  mutate(y_hat=predictions)  # our simulated predictions using one single point saved here

#COMPUTING MSE, (AVG BIAS OF FHAT)^2, VARIANCE OF FHAT, SIGMA^2

# Computing average MSE over n_sims. 
mse <- (mean(truth$y - truth$y_hat)^2)
mse

# (Avg Bias of f_hat)^2  - truth - prediction. 
avg_bias <- (mean(truth$f_x - truth$y_hat)^2)
avg_bias

#Variance of f_hat
var_fhat <- var(truth$x, truth$y_hat)
var_fhat

#sigma^2 
sig_sq <- sigma^2
sig_sq

#In theory these should equal the MSE --- 
eq_mse <- sig_sq + var_fhat + avg_bias
eq_mse
mse

#Two Kaggle Competitions: 
# 1. Bike Sharing Demand - Forecasting the use of a city bikeshare system
#https://www.kaggle.com/c/bike-sharing-demand

# 2. Airbnb New User Bookings - Where will a new guest book their first travel experience?
#https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings

