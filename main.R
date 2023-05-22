library(ggplot2)


####################################  Exploritory Data Analysis ###############################################################

data <- as.data.frame(read.table("Data/DatasetC.txt", sep="\t", header=TRUE))
# Remove ID Column
data$ID <- NULL  
data$FRAME <- factor(data$FRAME)
data$LOCATION <- factor(data$LOCATION)
data$GENDER <- factor(data$GENDER)

data <- na.omit(data)
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
full_model <- glm(GHB ~ CHOL + SGLU + LOCATION + AGE + GENDER + HHT + WHT + FRAME + SBP + DSP + W + H, data = data, family = Gamma(link = "log"))
summary(full_model)
full_model


####################################  Model comparison ###############################################################

reduced_models <- glm(GHB ~ CHOL + SGLU + AGE, data = data, family = Gamma(link = "log"))
summary(reduced_models)
residuals <- residuals(reduced_models)
fitted_values <- fitted(reduced_models)

residuals_df <- data.frame(Fitted_Values = fitted_values, Residuals = residuals)

ggplot(residuals_df, aes(x = Fitted_Values, y = Residuals)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs. Fitted Values")
