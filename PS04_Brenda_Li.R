# Setup -------------------------------------------------------------------
library(tidyverse)
library(broom)

# Let these be:
# -true function f(x) for a one-dimensional predictor x
# -the standard deviation of the true epsilon i.e. the noise
f <- function(x) {
  x^2
}
sigma <- 0.3

# This function generates a random sample. Random in terms of both
# -the predictor x
# -the amount of noise eps
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

# The true model f(x) is in red. We observe points (x,y). Here we are pretending
# to know the exact break down of y into f(x) and eps
ggplot(train_data, aes(x=x)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=f_x), col="red", size=1)

# In a realistic situation, we only know x & y, not f(x) & eps. So we should
# remove them
train_data <- train_data %>%
  select(x, y)



# Generate Test Data, Fit Model, and Get Predictions -------------------

# 1. Generate test data via sampling. Recall in test data, we only have the
# predictors x:
test_data <- generate_sample(f, n_sim = 100, sigma = sigma) %>%
  select(x)

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



# Main For Loop -------------------------------------------------
set.seed(76)
n_sim <- 10000

# Create new test data: a single point
x0 <- 0.95
test_data <- data_frame(x=x0)

# Save predictions here
predictions <- rep(0, n_sim)

for(i in 1:n_sim) {
  # Generate sample traning data
  sample_train<-generate_sample(f,n_sim=100,sigma)
  
  # Train model on sample training data
  model <- lm(y ~ x, data = sample_train)
  
  # Make prediction on test_data point 0.95
  predictions[i]<-model %>%
    augment(newdata=test_data) %>%
    tbl_df() %>%
    .[[".fitted"]]
  
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
  mutate(y_hat=predictions)

# Compute MSE, (Avg Bias of f_hat)^2, Variance of f_hat, sigma^2:
# FILL THIS IN:

MSE <- mean((truth$y_hat-truth$y)^2)

Bias <- (mean(truth$f_x-truth$y_hat))^2

Var_f_Hat <- var(truth$y_hat)

Sigma_Sqrd <- var(truth$eps)

# Calculated MSE is approximately Bias + Var_f_Hat + Sigma_Sqrd
         
  
