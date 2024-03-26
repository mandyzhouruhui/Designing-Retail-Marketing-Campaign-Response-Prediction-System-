##loading the relevant libraries

library(readxl)
library(ggplot2)
install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tidyr)

## loading the dataset
data_iFood <- file.choose()

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

## converting the marital_status to dummy variables
# Assuming your dataset is named 'df' and 'marital_status' is the column of interest
# Create dummy variables for 'marital_status' using ifelse

# Define the categories
categories <- c("Married", "Single", "Together")

# Create dummy variables for marital_status
for (category in categories) {
  data_iFood[paste("consolidated_marital_status", category, sep = "-")] <- ifelse(data_iFood$consolidated_marital_status == category, 1, 0)
}

## calculating the age of customer
data_iFood$age = 2023 - data_iFood$Year_Birth

## how to categorize the education variable
## drop ID, Dt_customer , date_customer, Z_CostContact, Z_Revenue, Marital_Status, 
## consolidated_marital_status, Year_Birth,
drops <- c("ID","Z_CostContact","Z_Revenue", "Marital_Status")
### This creates a dataframe (DATA) from d without the columns in drops
data_iFood <- data_iFood[,!(names(data_iFood) %in% drops)]

-------------------------------------------------------------------------------------------------
  ## I WAS TRYING TO RUN SOME EDA AND VISUALIZATION ON MY OWN

# Load the ggplot2 library if it's not already loaded
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}

# Create a sample dataframe (replace with your actual dataset)
# Assuming X, Y, Z, P, and Q are columns in your dataset


# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)



# Set the 'rocket' color palette
rocket_palette <- c("#FA5A50", "#FAD02E", "#31A2AC", "#61C0BF", "#E3E3E3")

# Create a ggplot for the violin plot with zoomed-in scale
ggplot(data_iFood, aes(x = Education, y = Income, fill = Education)) +
  geom_violin(scale = "count", width = 0.6) +
  scale_fill_manual(values = rocket_palette) +
  labs(x = "Education", y = "Income") +
  ggtitle("Income Distribution by Education") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 120000))  # Adjust the y-axis limits to zoom in



#Customer Enrollments by Month
# Convert 'Dt_Customer' to date format
data_iFood$Dt_Customer <- as.Date(data_iFood$Dt_Customer, format = "%m/%d/%Y")

# Define the order of months
month_order <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

# Create a countplot with sorted months
ggplot(data_iFood, aes(x = factor(format(Dt_Customer, "%B"), levels = month_order))) +
  geom_bar(stat = "count", fill = "lightblue") +
  labs(x = "Month", y = "Enrollments") +
  ggtitle("Customer Enrollments by Month") +
  theme_minimal()



#Income vs. Campaign

ggplot(data_iFood[data_iFood$Income < 3e+05 & !is.na(data_iFood$Income), ], aes(x = cut(Income, breaks = c(0, 10000, 25000, 50000, 75000, 100000, 3e+05), labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')), fill = factor(AcceptedCmp1))) +
  geom_bar(position = "dodge") +
  labs(x = "Income Range", y = "Count") +
  ggtitle("Distribution of Users Accepting Campaign1 According to Income Ranges") +
  scale_fill_manual(values = c("0" = "tomato1", "1" = "red3")) +
  theme_minimal()

ggplot(data_iFood[data_iFood$Income < 3e+05 & !is.na(data_iFood$Income), ], aes(x = cut(Income, breaks = c(0, 10000, 25000, 50000, 75000, 100000, 3e+05), labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')), fill = factor(AcceptedCmp2))) +
  geom_bar(position = "dodge") +
  labs(x = "Income Range", y = "Count") +
  ggtitle("Distribution of Users Accepting Campaign2 According to Income Ranges") +
  scale_fill_manual(values = c("0" = "tomato1", "1" = "red3")) +
  theme_minimal()

ggplot(data_iFood[data_iFood$Income < 3e+05 & !is.na(data_iFood$Income), ], aes(x = cut(Income, breaks = c(0, 10000, 25000, 50000, 75000, 100000, 3e+05), labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')), fill = factor(AcceptedCmp3))) +
  geom_bar(position = "dodge") +
  labs(x = "Income Range", y = "Count") +
  ggtitle("Distribution of Users Accepting Campaign3 According to Income Ranges") +
  scale_fill_manual(values = c("0" = "tomato1", "1" = "red3")) +
  theme_minimal()

ggplot(data_iFood[data_iFood$Income < 3e+05 & !is.na(data_iFood$Income), ], aes(x = cut(Income, breaks = c(0, 10000, 25000, 50000, 75000, 100000, 3e+05), labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')), fill = factor(AcceptedCmp4))) +
  geom_bar(position = "dodge") +
  labs(x = "Income Range", y = "Count") +
  ggtitle("Distribution of Users Accepting Campaign4 According to Income Ranges") +
  scale_fill_manual(values = c("0" = "tomato1", "1" = "red3")) +
  theme_minimal()

ggplot(data_iFood[data_iFood$Income < 3e+05 & !is.na(data_iFood$Income), ], aes(x = cut(Income, breaks = c(0, 10000, 25000, 50000, 75000, 100000, 3e+05), labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')), fill = factor(AcceptedCmp5))) +
  geom_bar(position = "dodge") +
  labs(x = "Income Range", y = "Count") +
  ggtitle("Distribution of Users Accepting Campaign5 According to Income Ranges") +
  scale_fill_manual(values = c("0" = "tomato1", "1" = "red3")) +
  theme_minimal()




#Kids_Teens
data_iFood$Kids_Teens <- ifelse(data_iFood$Kidhome != 0 & data_iFood$Teenhome != 0, 'Kids and Teens',
                                     ifelse(data_iFood$Kidhome == 0 & data_iFood$Teenhome != 0, 'Teens',
                                            ifelse(data_iFood$Kidhome != 0 & data_iFood$Teenhome == 0, 'Kids', 'No Kids and Teens')))


##goods
average_mntsum <- data_iFood %>%
  group_by(Kids_Teens) %>%
  summarise(Avg_MntSum = mean(MntSum))

ggplot(average_mntsum, aes(x = Kids_Teens, y = Avg_MntSum, fill = Kids_Teens)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Avg_MntSum, 2), y = Avg_MntSum), vjust = -0.5) +
  labs(x = "Kids/Teens Status", y = "Average Amount Spent on Goods") +
  ggtitle("Average Amount Spent on Goods by Kids/Teens Status") +
  scale_fill_manual(values = c("tomato4", "tomato3", "tomato2", "tomato1")) +
  theme_minimal()

##gold product
average_mntgold <- data_iFood %>%
  group_by(Kids_Teens) %>%
  summarise(Avg_MntGold = mean(MntGoldProds))


ggplot(average_mntgold, aes(x = Kids_Teens, y = Avg_MntGold, fill = Kids_Teens)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Avg_MntGold, 2), y = Avg_MntGold), vjust = -0.5) +
  labs(x = "Kids/Teens Status", y = "Average Amount Spent on Gold Products") +
  ggtitle("Average Amount Spent on Gold Products by Kids/Teens Status") +
  scale_fill_manual(values = c("tomato4", "tomato3", "tomato2", "tomato1")) +
  theme_minimal()



#Average Purchases by Kids/Teens Status and Purchase Type

data_iFood_filtered <- data_iFood %>%
  filter(!is.na(Kids_Teens), !is.na(NumDealsPurchases), !is.na(NumWebPurchases), !is.na(NumStorePurchases), !is.na(NumCatalogPurchases))


agg_data <- data_iFood_filtered %>%
  group_by(Kids_Teens) %>%
  summarise(Deals = mean(NumDealsPurchases),
            Web = mean(NumWebPurchases),
            Store = mean(NumStorePurchases),
            Catalog = mean(NumCatalogPurchases))

agg_data <- agg_data %>%
  pivot_longer(cols = c(Deals, Web, Store, Catalog),
               names_to = "PurchaseType",
               values_to = "Purchases")

ggplot(agg_data, aes(x = Kids_Teens, y = Purchases, fill = PurchaseType, label = round(Purchases, 2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c('tomato4', 'tomato2', 'tomato3', 'salmon')) +  # Set bar colors
  labs(x = "Kids and Teens in Customer's Household", y = "Average Purchases", fill = "Purchase Type") +
  ggtitle("Average Purchases by Kids/Teens Status and Purchase Type") +
  theme_minimal()


#Average Purchases by Education Level and Purchase Type
data_iFood_filtered <- data_iFood %>%
  filter(!is.na(Education), !is.na(NumDealsPurchases), !is.na(NumWebPurchases), !is.na(NumStorePurchases), !is.na(NumCatalogPurchases))

agg_data <- data_iFood_filtered %>%
  group_by(Education) %>%
  summarise(Deals = round(mean(NumDealsPurchases), 2),
            Web = round(mean(NumWebPurchases), 2),
            Store = round(mean(NumStorePurchases), 2),
            Catalog = round(mean(NumCatalogPurchases), 2))

agg_data <- agg_data %>%
  pivot_longer(cols = c(Deals, Web, Store, Catalog),
               names_to = "PurchaseType",
               values_to = "Purchases")

agg_data$Education <- factor(agg_data$Education, levels = c('Basic', '2n Cycle', 'Graduation', 'Master', 'PhD'))

ggplot(agg_data, aes(x = Education, y = Purchases, fill = PurchaseType, label = Purchases)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c('tomato4', 'tomato2', 'tomato3', 'salmon')) +  # Set bar colors
  labs(x = "Education Level", y = "Average Purchases", fill = "Purchase Type") +
  ggtitle("Average Purchases by Education Level and Purchase Type") +
  theme_minimal()



#Income Range

data_iFood$Income_level <- ifelse(data_iFood$Income < 10000, 1,
                            ifelse(data_iFood$Income < 25000, 2,
                                   ifelse(data_iFood$Income < 50000, 3,
                                          ifelse(data_iFood$Income < 75000, 4,
                                                 ifelse(data_iFood$Income < 100000, 5, 6)))))


#income level and age
averages <- data_iFood %>%
  filter(!is.na(Income_level)) %>%
  group_by(Income_level) %>%
  summarize(Avg_Age = mean(age))

ggplot(averages, aes(x = factor(Income_level), y = Avg_Age)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Income Level vs. Average Age",
       x = "Income Level",
       y = "Average Age") +
  scale_x_discrete(labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')) 

#Income Level vs. Average Purchases on Goods
averages <- data_iFood %>%
  filter(!is.na(Income_level)) %>%
  group_by(Income_level) %>%
  summarize(Avg_PurchaseTotal = mean(MntSum))


ggplot(averages, aes(x = factor(Income_level), y = Avg_PurchaseTotal)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Income Level vs. Average Purchases on Goods",
       x = "Income Level",
       y = "Average Purchases on Goods") +
  scale_x_discrete(labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')) 


#Income Level vs. Average Purchases Through Website
averages <- data_iFood %>%
  filter(!is.na(Income_level)) %>%
  group_by(Income_level) %>%
  summarize(Avg_WebPurchase = mean(NumWebPurchases))

ggplot(averages, aes(x = factor(Income_level), y = Avg_WebPurchase)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Income Level vs. Average Purchases Through Website",
       x = "Income Level",
       y = "Average Purchases Through Website ") +
  scale_x_discrete(labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')) 


#Average Purchases Using Catalogue
averages <- data_iFood %>%
  group_by(Income_level) %>%
  summarize(Avg_CatalogPurchase = mean(NumCatalogPurchases))

# Create a bar graph
ggplot(averages, aes(x = factor(Income_level), y = Avg_CatalogPurchase)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Income Level vs. Average Purchases Using Catalogue",
       x = "Income Level",
       y = "Average Purchases Using Catalogue") +
  scale_x_discrete(labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')) 


#Income Level vs. Average Purchases in Store
averages <- data_iFood %>%
  filter(!is.na(Income_level)) %>%
  group_by(Income_level) %>%
  summarize(Avg_StorePurchase = mean(NumStorePurchases))

ggplot(averages, aes(x = factor(Income_level), y = Avg_StorePurchase)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Income Level vs. Average Purchases in Store",
       x = "Income Level",
       y = "Average Purchases in Store") +
  scale_x_discrete(labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')) 


#Income Level vs. Average Visits to Website in the Last Month
averages <- data_iFood %>%
  filter(!is.na(Income_level)) %>%
  group_by(Income_level) %>%
  summarize(Avg_WebVisits = mean(NumWebVisitsMonth))

ggplot(averages, aes(x = factor(Income_level), y = Avg_WebVisits)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Income Level vs. Average Visits to Website in the Last Month",
       x = "Income Level",
       y = "Average Visits to Website in the Last Month") +
  scale_x_discrete(labels = c('0-10k', '10k-25k', '25k-50k','50k-75k','75k-100k','100k-200k')) 


