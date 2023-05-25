library(DataExplorer)
library(tidyverse)
library(summarytools)
library(ggplot2)

data <- as.data.frame(read.table("Data/DatasetC.txt", sep="\t", header=TRUE))
colnames(data) <- c("ID", "CHOL", "SGLU", "GHB", "LOCATION", "AGE", "GENDER", "HHT", "WHT", "FRAME", "SBP", "DBP", "W", "H")
data <- data[,-1] # remove ID
t(introduce(data)) # initial info
plot_missing(data)
data <- na.omit(data)
data$diabetes <- with(data, ifelse(GHB > 7, 'yes', 'no'))




#################################################################### EDA

############ Statistics for all features

my.summary <- function(X)
{
  results <- c(min(X),quantile(X,0.25), median(X), mean(X), quantile(X,0.75), max(X), var(X), sd(X), IQR(X))
  names(results) <- c("min", "Q1", "median", "mean", "Q3", "max", "var", "sd", "IQR")
  return(results)
}

apply(select_if(data, is.numeric),2,my.summary)


############ Hemoglobin

plot_histogram(data$GHB)
nrow(data[data$GHB >7, ])/nrow(data) * 100
nrow(data[data$GHB >7, ])
nrow(data[data$GHB <=7, ])

############ Cholesterol

plot_histogram(data$CHOL)
ggplot(data, aes(x = CHOL, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Total Cholesterol", y = "Density")

nrow(data[data$CHOL < 200 & data$CHOL > 125, ])/nrow(data) * 100
nrow(data[data$CHOL < 125, ])

############ Glucose

plot_histogram(data$SGLU)
ggplot(data, aes(x = SGLU, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Stabilized Glucose", y = "Density")

############Systolic Blood Pressure

plot_histogram(data$SBP)
ggplot(data, aes(x = SBP, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Systolic Blood Pressure", y = "Density")
nrow(data[data$SBP <= 119 & data$diabetes =="yes", ])
nrow(data[data$SBP <= 119 & data$diabetes =="no", ])

mean(data[ data$diabetes =="yes", ]$SBP)
mean(data[ data$diabetes =="no", ]$SBP)

nrow(data[data$SBP > 119 & data$SBP < 140 & data$diabetes =="yes", ])
nrow(data[data$SBP > 119 & data$SBP < 140 & data$diabetes =="no", ])

nrow(data[data$SBP >= 140 & data$diabetes =="yes", ])
nrow(data[data$SBP >= 140 & data$diabetes =="no", ])

nrow(data[data$SBP >= 210 & data$diabetes =="yes", ])

############ Diastolic Blood Pressure

plot_histogram(data$DBP)
ggplot(data, aes(x = DBP, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Diastolic Blood Pressure", y = "Density")

nrow(data[data$DBP < 60, ])
nrow(data[data$DBP >= 90, ])

############ Age

plot_histogram(data$AGE)
ggplot(data, aes(x = AGE, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Age", y = "Density")

############ Height

plot_histogram(data$HHT,geom_histogram_args = list(bins = 25L))
ggplot(data, aes(x = HHT, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Height", y = "Density")

############ Weight

plot_histogram(data$WHT)
ggplot(data, aes(x = WHT, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Weight", y = "Density")

############ Waist

plot_histogram(data$W)
ggplot(data, aes(x = W, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Waist", y = "Density")


############ Hip

plot_histogram(data$H)
ggplot(data, aes(x = H, fill = diabetes)) + geom_density(alpha = 0.8) +  labs(x = "Hip", y = "Density")

############ Location, Gender, Frame


names<-names(data)
classes<-sapply(data,class)

for(name in names[classes == 'factor'])
{
  print(ctable(data[,name], data$diabetes))
  #dev.new()
  filename <- paste("bar_",name,".png",sep="")
  #png(file=filename)
  plot_bar(data[,name])
  #dev.off()
}

####################################################################

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


data$BMI <- data$W / ((data$H/100)^2)
# Calculate HIP_WAIST_RATIO
data$HIP_WAIST_RATIO <- data$H / data$W


# Full model 
full_model <- glm(GHB ~ CHOL + SGLU + LOCATION + AGE + GENDER + HHT + WHT + FRAME + SBP + DSP + W + H
                               + BMI +H
                  , 
                  data = data, family = Gamma(link = "log"))
step(full_model)
full_model


####################################  Model comparison ###############################################################

reduced_models <- glm(GHB ~ CHOL + SGLU + AGE + W, family = Gamma(link = "log"), data = data)
dispersion <- gamma.shape(reduced_models)
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



## final dist.

# Generate random numbers from gamma distribution
n <- 100000
alpha <- exp(0.05529806)
beta <- 1
gamma_values <- rgamma(n, alpha, beta)

# Create a data frame for gamma distribution
df_gamma <- data.frame(x = gamma_values)

# Create a data frame for data$GHB
df_data <- data.frame(x = data$GHB)

# Plot histogram with data$GHB overlay
ggplot() +
  geom_density(data = df_gamma, aes(x = x, y = stat(density)), fill = "lightblue", alpha = 0.7) +
  geom_histogram(data = df_data, aes(x = x, y = stat(density)), fill = "lightgreen", alpha = 0.7, binwidth = 0.5) +
  labs(title = "Density Plot with Histogram Overlay", x = "GHB", y = "Relative frequency") +
  scale_fill_manual(values = c("Gamma Distribution" = "lightblue", "Data" = "lightgreen"), guide = guide_legend(title = "Legend"))
