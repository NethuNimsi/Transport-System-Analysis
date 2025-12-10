
packages <- c("ggplot2", "dplyr", "corrplot")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

# Load the libraries
library(ggplot2)
library(dplyr)
library(corrplot)


# Load the data
data <- read.csv("C:/Users/Nethmi Nimsilu/Desktop/auto_Info.csv")

# Check the data
head(data)
summary(data)
str(data)

# Check for missing values
colSums(is.na(data))



##############################################
#  Central Tendency Analysis 
##############################################

cat("\n=== CENTRAL TENDENCY ANALYSIS ===\n")

# Engine Size Analysis
engine_mean <- mean(data$engine_size)
engine_median <- median(data$engine_size)
engine_sd <- sd(data$engine_size)

cat("Engine Size Statistics:\n")
cat("Mean:", engine_mean, "\n")
cat("Median:", engine_median, "\n")
cat("Standard Deviation:", engine_sd, "\n")

# Horsepower Analysis  
hp_mean <- mean(data$horsepower)
hp_median <- median(data$horsepower)
hp_sd <- sd(data$horsepower)

cat("\nHorsepower Statistics:\n")
cat("Mean:", hp_mean, "\n")
cat("Median:", hp_median, "\n")
cat("Standard Deviation:", hp_sd, "\n")

# Curb Weight Analysis
weight_mean <- mean(data$curb_weight)
weight_median <- median(data$curb_weight)
weight_sd <- sd(data$curb_weight)

cat("\nCurb Weight Statistics:\n")
cat("Mean:", weight_mean, "\n")
cat("Median:", weight_median, "\n")
cat("Standard Deviation:", weight_sd, "\n")

# Price Analysis
price_mean <- mean(data$price)
price_median <- median(data$price)
price_sd <- sd(data$price)

cat("\nPrice Statistics:\n")
cat("Mean:", price_mean, "\n")
cat("Median:", price_median, "\n")
cat("Standard Deviation:", price_sd, "\n")

# Bell Curve Plots (student level - simple histograms with normal curve)

# Engine Size Bell Curve
hist(data$engine_size, freq = FALSE, main = "Distribution of Engine Size", 
     xlab = "Engine Size (L)", col = "lightblue", border = "black")
curve(dnorm(x, mean = engine_mean, sd = engine_sd), add = TRUE, col = "red", lwd = 2)
abline(v = engine_mean, col = "red", lty = 2)

# Horsepower Bell Curve  
hist(data$horsepower, freq = FALSE, main = "Distribution of Horsepower",
     xlab = "Horsepower (HP)", col = "lightgreen", border = "black")
curve(dnorm(x, mean = hp_mean, sd = hp_sd), add = TRUE, col = "red", lwd = 2)
abline(v = hp_mean, col = "red", lty = 2)

# Curb Weight Bell Curve
hist(data$curb_weight, freq = FALSE, main = "Distribution of Curb Weight",
     xlab = "Curb Weight (lbs)", col = "lightyellow", border = "black")
curve(dnorm(x, mean = weight_mean, sd = weight_sd), add = TRUE, col = "red", lwd = 2)
abline(v = weight_mean, col = "red", lty = 2)

# Price Bell Curve
hist(data$price, freq = FALSE, main = "Distribution of Price",
     xlab = "Price (USD)", col = "lightpink", border = "black")
curve(dnorm(x, mean = price_mean, sd = price_sd), add = TRUE, col = "red", lwd = 2)
abline(v = price_mean, col = "red", lty = 2)

##############################################
#Hypothesis Testing - Price by Vehicle Type 
##############################################

cat("\n=== HYPOTHESIS TESTING ===\n")

# Convert vehicle_type to factor
data$vehicle_type <- as.factor(data$vehicle_type)

# Check vehicle types
table(data$vehicle_type)

# Hypothesis:
# H0: There is no significant difference in price between vehicle types
# H1: There is a significant difference in price between vehicle types

#Boxplot first
boxplot(price ~ vehicle_type, data = data, 
        main = "Price Distribution by Vehicle Type",
        xlab = "Vehicle Type", ylab = "Price (USD)",
        col = rainbow(length(levels(data$vehicle_type))))

# ANOVA test
price_anova <- aov(price ~ vehicle_type, data = data)
anova_summary <- summary(price_anova)
print(anova_summary)

# Get p-value
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
cat("\nANOVA Results:\n")
cat("F-statistic:", anova_summary[[1]][["F value"]][1], "\n")
cat("p-value:", p_value, "\n")

if(p_value < 0.05) {
  cat("Result: Reject H0. There IS a significant difference in price between vehicle types.\n")
} else {
  cat("Result: Fail to reject H0. There is NO significant difference in price between vehicle types.\n")
}

# Summary by vehicle type
price_by_type <- data %>%
  group_by(vehicle_type) %>%
  summarise(
    count = n(),
    mean_price = mean(price),
    median_price = median(price),
    sd_price = sd(price)
  )

print(price_by_type)

# Create mean price comparison
mean_prices <- aggregate(price ~ vehicle_type, data = data, mean)
bp <- barplot(mean_prices$price, names.arg = mean_prices$vehicle_type,
              main = "Mean Vehicle Price by Type", xlab = "
     Vehicle Type", 
              ylab = "Mean Price (USD)", col = "lightblue", las = 2,
              ylim = c(0, max(mean_prices$price) * 1.15))
text(bp, mean_prices$price + max(mean_prices$price) * 0.05,
     labels = paste0("$", round(mean_prices$price, 0)), cex = 0.8)

# Tukey's HSD Post-hoc test
tukey_results <- TukeyHSD(price_anova)
cat("\nTukey's HSD Results:\n")
print(tukey_results)

# Density plots for detailed distribution comparison
library(ggplot2)
density_plot <- ggplot(data, aes(x = price, fill = vehicle_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ vehicle_type, scales = "free") +
  labs(title = "Price Distribution Density by Vehicle Type",
       x = "Price (USD)", y = "Density") +
  theme_minimal()

print(density_plot)

##############################################  
#Correlation and Regression Analysis 
##############################################

cat("\n=== TASK 5: CORRELATION AND REGRESSION ANALYSIS ===\n")


# Shapiro-Wilk test for normality
if(nrow(data) <= 5000) { 
  price_normality <- shapiro.test(data$price)
  cat("Price Normality Test (Shapiro-Wilk):\n")
  cat("p-value:", price_normality$p.value, "\n")
  
  if(price_normality$p.value > 0.05) {
    cat("Price data is normally distributed (p > 0.05)\n")
  } else {
    cat("Price data is NOT normally distributed (p < 0.05)\n")
  }
}

# Test normality for other variables too
engine_normality <- shapiro.test(data$engine_size)
cat("\nEngine Size Normality Test:\n")
cat("p-value:", engine_normality$p.value, "\n")

# Correlation Analysis
cat("\n--- CORRELATION ANALYSIS ---\n")

# Test 1: Price vs Engine Size
cor_price_engine <- cor(data$price, data$engine_size)
cor_test_engine <- cor.test(data$price, data$engine_size)

cat("\n1. Price vs Engine Size:\n")
cat("Correlation coefficient:", cor_price_engine, "\n")
cat("p-value:", cor_test_engine$p.value, "\n")

# Hypothesis for correlation:
# H0: No correlation between price and engine size (r = 0)
# H1: There is correlation between price and engine size (r ≠ 0)

if(cor_test_engine$p.value < 0.05) {
  cat("Result: Significant correlation exists\n")
} else {
  cat("Result: No significant correlation\n")
}

# Test 2: Price vs Horsepower
cor_price_hp <- cor(data$price, data$horsepower)
cor_test_hp <- cor.test(data$price, data$horsepower)

cat("\n2. Price vs Horsepower:\n")
cat("Correlation coefficient:", cor_price_hp, "\n")
cat("p-value:", cor_test_hp$p.value, "\n")

if(cor_test_hp$p.value < 0.05) {
  cat("Result: Significant correlation exists\n")
} else {
  cat("Result: No significant correlation\n")
}

# Test 3: Price vs Curb Weight
cor_price_weight <- cor(data$price, data$curb_weight)
cor_test_weight <- cor.test(data$price, data$curb_weight)

cat("\n3. Price vs Curb Weight:\n")
cat("Correlation coefficient:", cor_price_weight, "\n")
cat("p-value:", cor_test_weight$p.value, "\n")

if(cor_test_weight$p.value < 0.05) {
  cat("Result: Significant correlation exists\n")
} else {
  cat("Result: No significant correlation\n")
}

# Test 4: Price vs Age
cor_price_age <- cor(data$price, data$age)
cor_test_age <- cor.test(data$price, data$age)

cat("\n4. Price vs Age:\n")
cat("Correlation coefficient:", cor_price_age, "\n")
cat("p-value:", cor_test_age$p.value, "\n")

if(cor_test_age$p.value < 0.05) {
  cat("Result: Significant correlation exists\n")
} else {
  cat("Result: No significant correlation\n")
}

# Create correlation matrix
numeric_data <- data[, c("engine_size", "horsepower", "curb_weight", "age", "price")]
cor_matrix <- cor(numeric_data)

cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 3))

#correlation plot
corrplot(cor_matrix, method = "circle", type = "upper")


plot(data$engine_size, data$price, main = "Price vs Engine Size",
     xlab = "Engine Size (L)", ylab = "Price (USD)",
     pch = 16, col = "blue")
abline(lm(price ~ engine_size, data = data), col = "red", lwd = 2)

plot(data$horsepower, data$price, main = "Price vs Horsepower", 
     xlab = "Horsepower (HP)", ylab = "Price (USD)",
     pch = 16, col = "green")
abline(lm(price ~ horsepower, data = data), col = "red", lwd = 2)

plot(data$curb_weight, data$price, main = "Price vs Curb Weight",
     xlab = "Curb Weight (lbs)", ylab = "Price (USD)", 
     pch = 16, col = "orange")
abline(lm(price ~ curb_weight, data = data), col = "red", lwd = 2)

plot(data$age, data$price, main = "Price vs Age",
     xlab = "Age (years)", ylab = "Price (USD)",
     pch = 16, col = "purple")
abline(lm(price ~ age, data = data), col = "red", lwd = 2)

# Regression Analysis
cat("\n--- REGRESSION ANALYSIS ---\n")

# Multiple linear regression
regression_model <- lm(price ~ engine_size + horsepower + curb_weight + age, data = data)
reg_summary <- summary(regression_model)

cat("Multiple Linear Regression Results:\n")
print(reg_summary)

# Extract key values
r_squared <- reg_summary$r.squared
adj_r_squared <- reg_summary$adj.r.squared
f_statistic <- reg_summary$fstatistic[1]

cat("\nKey Regression Statistics:\n")
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n") 
cat("F-statistic:", f_statistic, "\n")

#Residual analysis
plot(regression_model$fitted.values, regression_model$residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 16)
abline(h = 0, col = "red", lty = 2)

##############################################
#Conclusions and Recommendations
##############################################

cat("\n=== ANALYSIS SUMMARY FOR MINISTRY ===\n")
cat("1. Vehicle prices vary significantly by type\n")
cat("2. Strong correlations found between price and vehicle characteristics\n") 
cat("3. Regression model explains", round(r_squared * 100, 1), "% of price variation\n")

#Fuel efficiency analysis for ministry
fuel_by_type <- data %>%
  group_by(vehicle_type) %>%
  summarise(
    avg_fuel_efficiency = mean(fuel_efficiency),
    avg_price = mean(price),
    count = n()
  ) %>%
  arrange(desc(avg_fuel_efficiency))

cat("\nFuel Efficiency by Vehicle Type:\n")
print(fuel_by_type)


# Save the workspace 
save.image("transport_analysis.RData")
cat("\nAnalysis completed and saved!\n")