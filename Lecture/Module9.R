library(ggplot2)

# Generate data
mydata <- data.frame(Ft = c(1, 6, 11, 16, 21, 2, 7, 12, 17, 22, 3, 8,
                            13, 18, 23, 4, 9, 14, 19, 5, 10, 15, 20),
                     Temp = c(66, 72, 70, 75, 75, 70, 73, 78, 70, 76, 69, 70,
                              67, 81, 58, 68, 57, 53, 76, 67, 63, 67, 79),
                     TD = c(0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,
                            0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0))

# Run logistic regression model
model <- glm(TD ~ Temp, data = mydata, family = binomial(link = "logit"))

# Create a temporary data frame of hypothetical values
temp.data <- data.frame(Temp = seq(53, 81, 0.5))

# Predict the fitted values given the model and hypothetical data
predicted.data <- as.data.frame(predict(model, newdata = temp.data,
                                        type = "link", se = TRUE))

# Combine the hypothetical data and predicted values
new.data <- cbind(temp.data, predicted.data)

# Calculate confidence intervals
std <- qnorm(0.95 / 2 + 0.5)
new.data$ymin <- model$family$linkinv(new.data$fit - std * new.data$se)
new.data$ymax <- model$family$linkinv(new.data$fit + std * new.data$se)
new.data$fit <- model$family$linkinv(new.data$fit) # Rescale to 0-1

# Plot everything
p <- ggplot(mydata, aes(x = Temp, y = TD))
p + geom_point() +
  geom_ribbon(data = new.data, aes(y = fit, ymin = ymin, ymax = ymax), alpha = 0.5) +
  geom_line(data = new.data, aes(y = fit)) +
  labs(x = "Temperature", y = "Thermal Distress")