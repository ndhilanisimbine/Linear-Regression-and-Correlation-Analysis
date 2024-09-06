#Question 1 
Auto_numeric <- Auto[, -which(names(Auto) == "name")]

pairs(Auto_numeric)

cor_matrix <- cor(Auto_numeric)
print(cor_matrix)


lm1 <- lm(mpg ~ horsepower, data = Auto)

summary(lm1)


#Output 
#Residual standard error: 4.906 on 390 degrees of freedom
#Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
#F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

#Comments
#Coefficients:
  
#  The Estimate for horsepower shows the change in mpg for each additional unit of horsepower.
#The Intercept gives the expected value of mpg when horsepower is zero.
#R-squared:
  
#  Indicates how well horsepower explains the variation in mpg. A higher value (closer to 1) means a better fit.
#p-value:
#  If the p-value is small (typically less than 0.05), it means horsepower is a statistically significant predictor of mpg.


# Comments on the Relationship:
# The linear regression analysis between mpg (miles per gallon) and horsepower reveals a negative relationship.
# The coefficient for horsepower is negative, which indicates that as horsepower increases, mpg decreases.
# Specifically, for every 1-unit increase in horsepower, the mpg decreases by approximately 
# Insert value of the ‘horsepower’ coefficient.

# The R-squared value of Insert R-squared value suggests that around percentage% of the variation in mpg is explained by horsepower alone.
# While this shows a moderate fit, it indicates that horsepower explains a reasonable portion of the variability in mpg.

# The p-value for horsepower is Insert p-value, which is less than the standard significance level of 0.05.
# This confirms that the relationship between horsepower and mpg is statistically significant, meaning horsepower is a reliable predictor of mpg.

# In conclusion, there is a statistically significant, strong negative relationship between horsepower and mpg.
# Cars with higher horsepower tend to have lower fuel efficiency.



# Scatterplot for mpg vs horsepower
plot(Auto$horsepower, Auto$mpg,
     main = "Scatterplot of MPG vs Horsepower",
     xlab = "Horsepower",
     ylab = "Miles Per Gallon (MPG)",
     pch = 19,  # Set point type to solid circle
     col = "blue")

# Adding the OLS regression line to the plot
abline(lm(mpg ~ horsepower, data = Auto), col = "red", lwd = 2)


#Part C

lm2 <- lm(mpg ~ weight, data = Auto)

summary(lm2)


# Scatterplot for mpg vs weight
plot(Auto$weight, Auto$mpg,
     main = "Scatterplot of MPG vs Weight",
     xlab = "Weight",
     ylab = "Miles Per Gallon (MPG)",
     pch = 19,  # Set point type to solid circle
     col = "blue")

# Adding the OLS regression line to the plot
abline(lm2, col = "red", lwd = 2)


#Part D

lm3 <- lm(mpg ~ horsepower + weight, data = Auto)

summary(lm3)

Call:
  lm(formula = mpg ~ horsepower + weight, data = Auto)

#Residuals:
 # Min       1Q   Median       3Q      Max 
#-11.0762  -2.7340  -0.3312   2.1752  16.2601 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 45.6402108  0.7931958  57.540  < 2e-16 ***
#  horsepower  -0.0473029  0.0110851  -4.267 2.49e-05 ***
#  weight      -0.0057942  0.0005023 -11.535  < 2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.24 on 389 degrees of freedom
#Multiple R-squared:  0.7064,	Adjusted R-squared:  0.7049 
#F-statistic: 467.9 on 2 and 389 DF,  p-value: < 2.2e-16

  new_data <- data.frame(horsepower = 98, weight = 2500)

predicted_mpg <- predict(lm3, newdata = new_data)

print(predicted_mpg)

#Result: 26.51914 


plot(lm3)

print(predicted_mpg)


# Part D: Multiple Linear Regression

# 1. Summary of the multiple regression model (mpg ~ horsepower + weight):
# The linear regression model includes mpg as the response variable and horsepower and weight as the predictors.
# The coefficient for horsepower is -0.0473, meaning that for each additional unit of horsepower, mpg decreases by approximately 0.0473 units, 
# assuming weight remains constant.
# The coefficient for weight is -0.0058, meaning that for each additional pound in weight, mpg decreases by 0.0058 units, 
# assuming horsepower remains constant.
# The R-squared value is 0.7064, meaning that approximately 70.64% of the variation in mpg is explained by the combined effects of horsepower and weight.
# Both predictors, horsepower and weight, have p-values well below 0.05 (2.49e-05 for horsepower and < 2e-16 for weight), 
# indicating that both relationships are statistically significant.

# 2. Comment on the relationships:
# - The relationship between horsepower and mpg is negative, as indicated by the negative coefficient (-0.0473). 
#   This means that as horsepower increases, fuel efficiency (mpg) tends to decrease.
# - The relationship between weight and mpg is also negative, with a coefficient of -0.0058. As the weight of the car increases, fuel efficiency decreases.
# - With an R-squared value of 0.7064, the model explains a large portion of the variability in mpg, indicating a strong fit to the data.

# 3. Diagnostic Plots:
# The plot(lm3) function produces four diagnostic plots:
# - Residuals vs Fitted: Shows no clear pattern, which is a good indication that the linearity assumption is satisfied.
# - Normal Q-Q: The residuals fall roughly along the line, indicating that the residuals are approximately normally distributed.
# - Scale-Location: The residuals appear randomly scattered, which suggests that the assumption of homoscedasticity (equal variance) is likely satisfied.
# - Residuals vs Leverage: No points appear to have both high leverage and large residuals, suggesting there are no overly influential data points.

# 4. Prediction:
# The predicted mpg for a car with horsepower = 98 and weight = 2500 is 26.52 mpg.
# This value represents the expected fuel efficiency for a car with these characteristics based on the model.

#Question 2 


data(Auto)

mpg_median <- median(Auto$mpg)

Auto$mpg01 <- ifelse(Auto$mpg > mpg_median, 1, 0)

head(Auto)

Auto_numeric <- Auto[, -which(names(Auto) == "name")]

cor_matrix <- cor(Auto_numeric)

print(cor_matrix)


#Part C and Part D

set.seed(123)

train_indices <- sample(1:nrow(Auto), size = 0.7 * nrow(Auto))

train_data <- Auto[train_indices, ]

test_data <- Auto[-train_indices, ]


# Loading the necessary data
data(Auto)

# Creating the binary variable mpg01
mpg_median <- median(Auto$mpg)
Auto$mpg01 <- ifelse(Auto$mpg > mpg_median, 1, 0)

set.seed(123)

train_indices <- sample(1:nrow(Auto), size = 0.7 * nrow(Auto))
train_data <- Auto[train_indices, ]
test_data <- Auto[-train_indices, ]

glm_model <- glm(mpg01 ~ horsepower + weight, data = train_data, family = binomial)

summary(glm_model)


# Loading the necessary data
data(Auto)

# Creating the binary variable mpg01
mpg_median <- median(Auto$mpg)
Auto$mpg01 <- ifelse(Auto$mpg > mpg_median, 1, 0)

#seed for reproducibility
set.seed(123)

# data into training (70%) and test (30%) sets
train_indices <- sample(1:nrow(Auto), size = 0.7 * nrow(Auto))
train_data <- Auto[train_indices, ]
test_data <- Auto[-train_indices, ]

# logistic regression model on the training set
glm_model <- glm(mpg01 ~ horsepower + weight, data = train_data, family = binomial)

#  summary of the logistic regression model
summary(glm_model)

# Part E:
predicted_probs <- predict(glm_model, newdata = test_data, type = "response")

# Part F: 
predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)

# Part G: 
# Create a vector where 1 indicates incorrect classification (predicted != actual)
error_vector <- ifelse(predicted_class != test_data$mpg01, 1, 0)

# Part H: 
test_error <- mean(error_vector)


#Question 3


# Loading the iris dataset
data(iris)

# Part A: 
set.seed(123)  # Set seed for reproducibility

# Part B: 
kc <- kmeans(iris[, c("Sepal.Length", "Sepal.Width")], centers = 3)

# Part C: 
print(kc$cluster)

# Part D: 
print(kc$centers)

# Part E: 
library(ggplot2)

# scatterplot of Sepal.Length vs Sepal.Width and color the points by cluster
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = factor(kc$cluster))) +
  geom_point() +
  geom_point(aes(x = kc$centers[, 1], y = kc$centers[, 2]), color = "black", size = 4, shape = 3) +
  labs(title = "K-means Clustering of Iris Data (Sepal.Length and Sepal.Width)",
       x = "Sepal Length", y = "Sepal Width", color = "Cluster") +
  theme_minimal()














