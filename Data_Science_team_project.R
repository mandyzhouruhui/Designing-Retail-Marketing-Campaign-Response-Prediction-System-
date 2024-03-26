##loading the relevant libraries
################################################################################
################################################################################
###############DATA PREPARATION#################################################

library(readxl)
library(ggplot2)
install.packages("lubridate")
library(lubridate)

## loading the dataset
data_iFood <- read.csv("ifood_data.csv")


## basic EDA 
summary(data_iFood)

print(sapply(data_iFood, class))


## converting the Dt_customer column to Date type column 
## created a new column called days_until to find the difference between today and the Dt_customer

data_iFood$date_customer <- ymd(data_iFood$Dt_Customer)
data_iFood$days_until <- as.integer(today() - data_iFood$date_customer)

## The column Marital_Status has 7 categories [Married, Divorced, Single, Together, Alone, Widow, Absurd and YOLO]
## I have converted these categories ("Divorced", "Absurd", "Alone", "Widow", "YOLO")them to 1 single category Single

print(table(data_iFood$Education))
# Create a vector of categories to be converted to 'Single'
categories_to_convert <- c("Divorced", "Absurd", "Alone", "Widow", "YOLO")


data_iFood$consolidated_marital_status <- ifelse(data_iFood$Marital_Status %in% c("Married", "Single", "Together"),
                                         data_iFood$Marital_Status,
                                         "Single")


print(table(data_iFood$consolidated_marital_status))
## converting the marital_status to dummy variables

# Create dummy variables for 'marital_status' using ifelse

# Define the categories
categories <- c("Married", "Single", "Together")

# Create dummy variables for marital_status
for (category in categories) {
  data_iFood[paste("consolidated_marital_status", category, sep = "-")] <- ifelse(data_iFood$consolidated_marital_status == category, 1, 0)
}

## calculating the age of customer
data_iFood$age = 2019 - data_iFood$Year_Birth
print(max(data_iFood$date_customer))
print(min(data_iFood$date_customer))
## how to categorize the education variable


install.packages("dplyr")
library(dplyr)


data_iFood <- data_iFood %>%
  cbind(model.matrix(~ Education - 1, data = data_iFood))


## creating 2 new columns for helping our analysis

data_iFood$MntTotal = data_iFood$MntWines + data_iFood$MntFruits + data_iFood$MntMeatProducts + data_iFood$MntFishProducts + data_iFood$MntSweetProducts+ data_iFood$MntGoldProds
# Assuming you have a data frame named "data_iFood" with columns A, B, C, D, and E
data_iFood$AcceptedCmpOverall <- ifelse(rowSums(data_iFood[, c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Response")]) == 0, 0, 1)
#checking the distribution of income
# Create a histogram to visualize income distribution
ggplot(data_iFood, aes(x = Income)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Income Distribution", x = "Income", y = "Frequency")


## drop ID, Dt_customer , date_customer, Z_CostContact, Z_Revenue, Marital_Status, consolidated_marital_status, Year_Birth,
drops <- c("ID","Year_Birth","Dt_Customer","Education","date_customer","Z_CostContact","Z_Revenue", "Marital_Status", "consolidated_marital_status")
### This creates a dataframe (DATA) from d without the columns in drops
data_iFood <- data_iFood[,!(names(data_iFood) %in% drops)]
############################################
#imputing NAs for income
# Load necessary libraries
library(caret)

library(dplyr)

# Define the name of the target variable to be imputed
target_variable <- "Income"

# Identify rows with missing Income values
missing_rows <- is.na(data_iFood$Income)

# Split the dataset into two parts: one with missing Income and one without
data_missing <- data_iFood[missing_rows, ]
data_complete <- data_iFood[!missing_rows, ]

# Create a regression model to predict Income based on predictors
regression_model <- train(
  x = data_complete[, setdiff(names(data_complete), target_variable)],
  y = data_complete$Income,
  method = "lm"  # Linear regression model, you can choose other regression methods if needed
)
regression_model <- lm(Income ~ ., data = data_complete)
# Predict missing Income values
imputed_incomes <- predict(regression_model, newdata = data_missing)

# Replace missing Income values with imputed values in the original dataset
data_iFood$Income[missing_rows] <- imputed_incomes


##### we can see that the salary column has no NAs
summary(data_iFood)

###creating a column Income_level for analysis
# Assuming you have a data frame named "data_iFood" with a column named "Income"

# Create a new column "Income_level" based on the specified conditions
# Assuming you have a data frame named "data_iFood" with a column named "Income"
data_iFood$Income_level <- ifelse(data_iFood$Income < 10000, 1,
                                  ifelse(data_iFood$Income < 25000, 2,
                                         ifelse(data_iFood$Income < 50000, 3,
                                                ifelse(data_iFood$Income < 75000, 4,
                                                       ifelse(data_iFood$Income < 100000, 5, 6)))))


### renaming certain columns

names(data_iFood)[names(data_iFood) == "consolidated_marital_status-Married"] <- "Married"
names(data_iFood)[names(data_iFood) == "consolidated_marital_status-Single"] <- "Single"
names(data_iFood)[names(data_iFood) == "consolidated_marital_status-Together"] <- "Together"
names(data_iFood)[names(data_iFood) == "Education2n Cycle"] <- "Education2nCycle"


write.csv(data_iFood, "cleaned_data.csv")
#########END OF DATA PREPARATION ##############################################
###############################################################################
####### MODELLING#############################################################







data_iFood$Response <- as.factor(data_iFood$Response)
# Sanitize factor levels
data_iFood$Response2 <- make.names(data_iFood$Response)
drops <- c("Response2")
### This creates a dataframe (DATA) from d without the columns in drops
data_iFood <- data_iFood[,!(names(data_iFood) %in% drops)]

################################################################################
#checking the correlation of all variables
# Calculate the correlation matrix
cor_matrix <- cor(data_iFood)

ggplot(data = as.data.frame(cor_matrix), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  coord_fixed(ratio = 1)


library(ggplot2)

# Count missing values in each column of the dataset
missing_values <- sapply(data_iFood, function(x) sum(is.na(x)))

# Create a data frame for plotting
missing_data <- data.frame(Variables = names(missing_values), Missing_Values = missing_values)

# Create a bar plot
ggplot(missing_data, aes(x = Variables, y = Missing_Values)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Missing Values by Column",
       x = "Variables",
       y = "Count of Missing Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################data exploration#################################



# 1) Calculate the average age for each income level
avg_age_by_income <- aggregate(Age ~ Income_level, data = data_iFood, FUN = mean)

# Create a bar plot with labels
ggplot(avg_age_by_income, aes(x = factor(Income_level), y = age)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(age, 2), vjust = -0.5), size = 3) +  # Add labels
  labs(title = "Average Age by Income Level",
       x = "Income Level",
       y = "Average Age") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))  # Use labels for discrete x-axis

# 2) Calculate the average num of deal purchases for each income level
avg_deal_purchases <- aggregate(NumDealsPurchases ~ Income_level, data = data_iFood, FUN = mean)

# Create a bar plot with labels
ggplot(avg_deal_purchases, aes(x = factor(Income_level), y = NumDealsPurchases)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(NumDealsPurchases, 2), vjust = -0.5), size = 3) +  # Add labels
  labs(title = "Average Age by Income Level",
       x = "Income Level",
       y = "Average number of deal purchases") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5","6"))  # Use labels for discrete x-axis
###############################################################################
#### end of data exploration###################################################


##trying lasso # Load necessary libraries

data <- data_iFood
dropss <- c("AcceptedCmpOverall","Income_level","MntTotal")
### This creates a dataframe (DATA) from d without the columns in drops
data <- data_iFood[,!(names(data_iFood) %in% dropss)]


install.package("glmnet")
library(glmnet)
#### Lets run Lasso
M1x<- model.matrix(Response ~ ., data=data)[,-1]
M1y<- data$Response
lasso1 <- glmnet(M1x,M1y)
lassoCV1 <- cv.glmnet(M1x,M1y)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV1, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))
#### Post Lasso #####
features.min <- support(lasso1$beta[,which.min(lassoCV1$cvm)])
length(features.min)
data.min <- data.frame(M1x[,features.min],M1y)

selected_feature_names <- colnames(M1x[, features.min])
print(selected_feature_names)

