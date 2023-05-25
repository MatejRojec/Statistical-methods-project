library(DataExplorer)
library(tidyverse)
library(summarytools)

data <- read.table("data_.txt")
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

data$LOCATION <- as.factor(data$LOCATION)
data$GENDER <- as.factor(data$GENDER)
data$FRAME <- as.factor(data$FRAME)

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