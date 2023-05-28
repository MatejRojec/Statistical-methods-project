library(ggplot2)


####################################  Data prepiration ###############################################################

data <- as.data.frame(read.table("Data/DatasetC.txt", sep="\t", header=TRUE))
# Remove ID Column
data$ID <- NULL  
data$FRAME <- factor(data$FRAME)
data$LOCATION <- factor(data$LOCATION)
data$GENDER <- factor(data$GENDER)

data <- na.omit(data)

data$BMI <- 703*data$WHT/(data$HHT)^2
# Calculate HIP_WAIST_RATIO
data$HIP_WAIST_RATIO <- data$H / data$W


####################################  Model selection and model fitting  ###############################################################

# Full model 
full_model <- glm(GHB ~ CHOL + SGLU + LOCATION + AGE + GENDER + HHT + WHT + FRAME + SBP + DSP + W + H + BMI + HIP_WAIST_RATIO, 
                  data = data, family = Gamma(link = "log"))
summary(full_model)
full_model$aic
step(full_model)
full_model


####################################  Model comparison ###############################################################

reduced_models <- glm(GHB ~ CHOL + SGLU + AGE + W, data = data, family = Gamma(link = "log"))
summary(reduced_models)
residuals <- residuals(reduced_models)
fitted_values <- fitted(reduced_models)
conf_intervals <- confint(reduced_models)


plot(reduced_models, which = 1)  # Residuals vs. Fitted values
plot(reduced_models, which = 2)  # Normal Q-Q plot of residuals
plot(reduced_models, which = 3)  # Scale-Location plot
plot(reduced_models, which = 5)  # Cook's distance plot


summary(reduced_models)
exp(coef(reduced_models))
confint(reduced_models)


anova(reduced_models, test = "Chisq")


residuals <- residuals(reduced_models, type = "pearson") / sqrt(reduced_models$deviance / reduced_models$df.residual)

# Calculate the absolute values of the standardized residuals
abs_residuals <- abs(residuals)

# Set a threshold for identifying outliers (e.g., 2 or 3 standard deviations)
threshold <- 2

# Identify the outliers based on the threshold
outliers <- which(abs_residuals > threshold)


rows_to_remove <- outliers  # Specify the row numbers you want to remove

# Create a new dataframe excluding the specified rows
data <- data[-rows_to_remove, ]


# Set a threshold for identifying influential observations (e.g., 1 or any other value)
threshold <- 0.1

# Calculate Cook's distance values
cooksd <- cooks.distance(reduced_models)

# Identify influential observations based on the threshold
influential_observations <- which(cooksd > threshold)

# Extract the influential rows
influential_rows <- data[influential_observations, ]

# Print the influential rows
print(influential_rows)


# Generate random numbers from gamma distribution

gamma_values <- reduced_models$fitted.values
df_gamma <- data.frame(x = gamma_values)

# Create a data frame for data$GHB
df_data <- data.frame(x = data$GHB)

# Plot histogram with data$GHB overlay
ggplot() +
  geom_density(data = df_gamma, aes(x = x, y = stat(density), fill = "Fitted model distribution"), alpha = 0.7) +
  geom_histogram(data = df_data, aes(x = x, y = stat(density), fill = "Data histogram"), alpha = 0.7, binwidth = 0.5) +
  labs(title = "Fitted model distribution vs data histogram", x = "GHB", y = "Relative frequency") +
  scale_fill_manual(values = c("Fitted model distribution" = "lightblue", "Data histogram" = "lightgreen"), guide = guide_legend(title = "Legend"))





