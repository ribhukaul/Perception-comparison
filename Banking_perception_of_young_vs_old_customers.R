library(readxl)
library(dplyr)
library(car)



model1<- lm(silver_score ~ age +occupation + 
              qualification + gender, data = adults_youngsters_data)
plot(model1)
summary(model1)

plot(adults_youngsters1$age, adults_youngsters1$silver_score, xlab= "Age", ylab= "silver_score")
abline(model1, col="red", lw=4)


# Cross validation

model4 <- lm(silver_score ~ age + I(age^2) + I(age^3), data = adults_youngsters_data)
summary(model4)
plot(model4)




install.packages("DAAG")
library(DAAG)
k<- 10
high_leverage_indices <- c(2, 105, 120, 121, 122, 164, 197, 200, 201, 205)
new_data<- adults_youngsters_data[-high_leverage_indices, ]



modeln2 <- lm(silver_score ~I(age^2) + I(age^3), data = new_data)
summary(modeln2)
plot(modeln2)



formula <- formula(silver_score ~  I(age^2) + I(age^3))
              
k<- 10
cv_results <- cv.lm(data = new_data, form.lm = formula, m = k)

summary(cv_results)

install.packages("car")
library(car)

install.packages("carData")
library(carData)

independent_vars <- data.frame(adults_youngsters1$age, age_squared = adults_youngsters1$age^2, 
                               age_cubed = adults_youngsters1$age^3)

vif_values <- car::vif(independent_vars)

summary(model45)
plot(model45)

model5 <- lm(silver_score ~ age + I(age^2) + I(age^3) + I(age^4) + log(age), data = adults_youngsters1)
plot(model5)

summary(model5)

info@mediamenteconsulting.it

vif(model4)

library(boot)

data<- adults_youngsters_data
correlation_matrix <- cor(data[c("silver_score", "age", "qualification")])
print(correlation_matrix)
reg1<- lm(adults_youngsters_data$silver_score~ adults_youngsters_data$age)
summary(reg1)
# Define the function to calculate the mean squared error
mse <- function(y, yhat) {
  mean((y - yhat)^2)
}

# Define the linear regression model
model1 <- lm(silver_score ~ age + occupation + qualification, data = adults_youngsters1)

# Perform 10-fold cross-validation
set.seed(123) # for reproducibility
cv_results <- cv.glm(data = adults_youngsters1, 
                     glmfit = model1, 
                     K = 10, 
                     cost = mse)

# Print the cross-validation results
cv_results$delta


