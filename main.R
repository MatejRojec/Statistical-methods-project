library(ggplot2)


####################################  Exploritory Data Analysis ###############################################################

data <- as.data.frame(read.table("Data/DatasetC.txt", sep="\t", header=TRUE))
# Remove ID Column
data$ID <- NULL  
data$FRAME <- factor(data$FRAME)
data$LOCATION <- factor(data$LOCATION)
data$GENDER <- factor(data$GENDER)

data <- na.omit(data)


data$BMI <- data$W / ((data$H/100)^2)
# Calculate HIP_WAIST_RATIO
data$HIP_WAIST_RATIO <- data$H / data$W

# Data summary
summary(data)

# NA values
na_rows <- which(rowSums(is.na(data)) > 0)
na_data <- data[na_rows,]

# Scatter plots
par(mfrow=c(3,4), mar=c(2,2,2,2))
for (i in 1:12) {
  plot(data[,i], data$GHB, main=colnames(data)[i])
}

# Distributions

var_names <- names(data)
par(mfrow=c(3, 4), mar=c(2,2,2,2)) # adjust rows and columns according to your data
for (i in 1:length(var_names)) {
  if (is.numeric(data[[i]])) {
    hist(data[[i]], main = var_names[i], xlab = var_names[i])
  } 
}

# Outliers analysis

par(mfrow=c(3,4))
for (i in 1:12) {
  boxplot(data[,i], main=colnames(data)[i])
}

# identify outliers in each column
for (i in 1:12) {
  if (is.numeric(data[[i]])) {
  col <- data[,i]
  q1 <- quantile(na.omit(col), 0.25) 
  q3 <- quantile(na.omit(col), 0.75) 
  iqr <- q3 - q1
  upper_bound <- q3 + 1.5 * iqr
  lower_bound <- q1 - 1.5 * iqr
  outliers <- which(col > upper_bound | col < lower_bound)
  if (length(outliers) > 0) {
    cat("Outliers in column", colnames(data)[i], ":", outliers, "\n")
  }
  }
}


####################################  Model selection and model fitting  ###############################################################



# Full model 
full_model <- glm(GHB ~ CHOL + SGLU + LOCATION + AGE + GENDER + HHT + WHT + FRAME + SBP + DSP + W + H
   #               + BMI +H
                  , 
                  data = data, family = Gamma(link = "log"))
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




