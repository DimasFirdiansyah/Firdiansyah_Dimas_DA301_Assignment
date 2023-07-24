## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(moments)
library(psych)

# Import the data set.
t_sales <- read.csv('turtle_sales.csv', header=T)

# Print the data frame.
head(t_sales)
View(t_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
t_sales_nw <- select(t_sales, -c('Ranking','Year','Genre','Publisher'))

# View the data frame.
head(t_sales_nw) 

# View the descriptive statistics.
summary(t_sales_nw)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Global Sales vs. NA sales 
ggplot(t_sales_nw, aes(x = NA_Sales, y = Global_Sales, color = Platform)) +
  geom_point() +
  theme_minimal()

# Global Sales vs. EU Sales
ggplot(t_sales_nw, aes(x = EU_Sales, y = Global_Sales, color = Product)) +
  geom_point() +
  theme_minimal()

## 2b) Histograms
# Create histograms.

# Distribution of global sales
ggplot(t_sales_nw, aes(x = Global_Sales)) +
  geom_histogram(fill = "purple", color = "white") +
  theme_minimal()

# Distribution of NA sales
ggplot(t_sales_nw, aes(x = NA_Sales)) +
  geom_histogram(fill = "orange", color = "white") +
  theme_minimal()

# Distribution of EU sales
ggplot(t_sales_nw, aes(x = EU_Sales)) +
  geom_histogram(fill = "yellow", color = "white") +
  theme_minimal()

## 2c) Boxplots
# Create boxplots.

# Compare Global Sales by Platform
ggplot(t_sales_nw, aes(x = Platform, y = Global_Sales)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compare Global Sales by Platform for Top 10 Product
top_10_products <- head(arrange(t_sales_nw, desc(Global_Sales)), 10)
ggplot(top_10_products, aes(x = Platform, y = Global_Sales)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################

# 3. Observations and insights

# In this task, we used R to conduct an exploratory data analysis on Turtle Games' sales data. 
# Our analysis involved scatter plots, histograms, and box plots, allowing us to explore the 
# relationships between different sales regions, understand the sales distribution across 
# global, North America, and Europe markets, and compare sales performances across different 
# gaming platforms and products. We noticed a positive correlation between global and regional 
# sales, indicating that games with high regional sales tend to perform well globally. 
# Histograms provided insights into the frequency distribution of sales, while box plots compared 
# sales by platforms, revealing potential best-performing platforms and products. 
# Outliers were also identified, offering leads for further investigation. 
# Overall, our visual and statistical exploration offered crucial insights that can guide 
# Turtle Games' sales department in optimizing their marketing strategies and improving sales 
# performance across different markets and platforms.




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(t_sales_nw)

# Check output: Determine the min, max, and mean values.

# NA_sales  
min(t_sales_nw$NA_Sales, na.rm=TRUE) # using na.rm to exclude from calculation
max(t_sales_nw$NA_Sales, na.rm=TRUE)
mean(t_sales_nw$NA_Sales, na.rm=TRUE)

# EU_sales
min(t_sales_nw$EU_Sales, na.rm=TRUE)
max(t_sales_nw$EU_Sales, na.rm=TRUE)
mean(t_sales_nw$EU_Sales, na.rm=TRUE)

# Global_Sales
min(t_sales_nw$Global_Sales, na.rm=TRUE)
max(t_sales_nw$Global_Sales, na.rm=TRUE)
mean(t_sales_nw$Global_Sales, na.rm=TRUE)

# View the descriptive statistics.
summary(t_sales_nw)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
impact_prod <- t_sales_nw %>%
  group_by(Product) %>%
  summarise(t_sales_nw = sum(Global_Sales))

# View the data frame.
head(impact_prod)

# Explore the data frame.
summary(impact_prod)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
# NA vs Global
ggplot(t_sales_nw, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  theme_minimal()

# EU vs Global
ggplot(t_sales_nw, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  theme_minimal()

# Create histograms.
# Global Sales
ggplot(t_sales_nw, aes(x = Global_Sales)) +
  geom_histogram(fill = "purple", color = "white") +
  theme_minimal()

# EU Sales
ggplot(t_sales_nw, aes(x = NA_Sales)) + 
  geom_histogram(fill = "green", color = "white") + 
  theme_minimal()

# NA Sales
ggplot(t_sales_nw, aes(x = EU_Sales)) +
  geom_histogram(fill = "blue", color = "white") + 
  theme_minimal()

# Create boxplots.
# Global
ggplot(t_sales_nw, aes(x = Platform, y = Global_Sales)) +
  geom_boxplot(fill = "lightblue", color = "purple") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# NA
ggplot(t_sales_nw, aes(x = Platform, y = NA_Sales)) +
  geom_boxplot(fill = "lightblue", color = "darkgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EU
ggplot(t_sales_nw, aes(x = Platform, y = EU_Sales)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Global
qqnorm(t_sales_nw$Global_Sales, col = 'purple')
qqline(t_sales_nw$Global_Sales, col = "red") # add Refrence line
# EU
qqnorm(t_sales_nw$EU_Sales, col = 'blue')
qqline(t_sales_nw$EU_Sales, col =  'red') # add Refrence line
# NA
qqnorm(t_sales_nw$NA_Sales, col = 'darkgreen')
qqline(t_sales_nw$NA_Sales, col = 'red') # add Refrence line

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.

# Global
shapiro.test(t_sales_nw$Global_Sales)
# NA
shapiro.test(t_sales_nw$NA_Sales)
# EU
shapiro.test(t_sales_nw$EU_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Global
skewness(t_sales_nw$Global_Sales)
kurtosis(t_sales_nw$Global_Sales)
# NA
skewness(t_sales_nw$NA_Sales)
kurtosis(t_sales_nw$NA_Sales)
# EU
skewness(t_sales_nw$EU_Sales)
kurtosis(t_sales_nw$EU_Sales)
  

## 3d) Determine correlation
# Determine correlation.

sales_corr <- cor(t_sales_nw[, c('NA_Sales','EU_Sales','Global_Sales')])
sales_corr

# Calculate Correlation coefficients
cor_na_eu <- sales_corr[1, 2]
cor_na_global <- sales_corr[1, 3]
cor_eu_global <- sales_corr[2, 3]

cor_na_eu
cor_na_global
cor_eu_global

# Assuming you have selected only the numeric columns for the correlation analysis
correlation_matrix <- cor(t_sales_nw[, c("NA_Sales", "EU_Sales", "Global_Sales")])

ggplot(data = as.data.frame(as.table(correlation_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  labs(title = "Correlation Heatmap",
       x = "Sales Variables",
       y = "Sales Variables",
       fill = "Correlation")

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.

ggplot(t_sales_nw, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("NA Sales vs. EU Sales")

ggplot(t_sales_nw, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("NA Sales vs. Global Sales")

ggplot(t_sales_nw, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("EU Sales vs. Global Sales")

ggplot(t_sales_nw, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 5) +  # Adjust binwidth as needed
  labs(title = "Distribution of Global Sales",
       x = "Global Sales",
       y = "Frequency")

ggplot(t_sales, aes(x = Platform, y = Global_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Global Sales by Platform and Region",
       x = "Platform",
       y = "Global Sales",
       fill = "Region")

# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights

# We used R to evaluate the reliability of the sales data from Turtle Games. 
# The dataset's normality was assessed via the Shapiro-Wilk test, with a p-value
# above 0.05 indicating a normal distribution.
# Skewness and kurtosis evaluated the distribution's asymmetry and the heaviness
# of the tails, respectively.The strength and direction of relationships between
# various sales data columns were determined using correlation coefficients.
# Scatter plots with trend lines offered a visual perspective of these relationships,
# with the slope of the trend line indicating the correlation direction.
# These analysis tools enabled us to understand the distribution of sales data 
# and identify patterns and trends among different regions. 




###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
# t_sales_nw <- read.csv('sales_nw',header =T)
head(t_sales_nw)
View(t_sales_nw)

# Determine a summary of the data frame.
summary(t_sales_nw)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(t_sales_nw[, c('NA_Sales','EU_Sales','Global_Sales')])

# plot the relationship between 3 variables (EU,NA,Global sales)
plot(t_sales_nw$Global_Sales, t_sales_nw$NA_Sales)
plot(t_sales_nw$Global_Sales, t_sales_nw$EU_Sales)
plot(t_sales_nw$NA_Sales, t_sales_nw$EU_Sales)

# Create a linear regression model on the original data.
model1 <- lm(Global_Sales ~ EU_Sales + NA_Sales, data = t_sales_nw)

summary(model1)

# 2b) Create a plot (simple linear regression)
# Basic visualisation.
par(mfrow=c(2,2))

ggplot(t_sales_nw, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Simple Linear Regression - Global Sales vs. NA Sales",
       x = "North America Sales",
       y = "Global Sales")

ggplot(t_sales_nw, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Simple Linear Regression - EU Sales vs. Global Sales",
       x = "Eurpean Sales",
       y = "Global Sales")

ggplot(t_sales_nw, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Simple Linear Regression - NA Sales vs. EU Sales",
       x = "North America Sales",
       y = "European Sales")


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
t_sales_num <- select(t_sales_nw,-c('Platform'))

corPlot(t_sales_num, cex = 2)

# Create a new regression model
# Specifying the lm function and the variables
modela = lm(Global_Sales~EU_Sales+NA_Sales, data=t_sales_num)

# View the model a summary
summary(modela)



###############################################################################

# 4. Predictions based on given values

# Creating new dataframe for obeserved value
obs_value <- data.frame(NA_Sales = c(34.02,3.93,2.73,2.26,22.08),
                        EU_Sales = c(23.80,1.56,0.65,0.97,0.52))
# Check df
obs_value

# Compare with observed values for a number of records.
predict(modela,
        newdata = obs_value)

obs_value$logIndex <- predict(modela,
                              newdata = obs_value)

obs_value <- mutate(obs_value,
                    Index=exp(logIndex))

obs_value
###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The multiple linear regression model developed for predicting Global Sales 
# based on EU_Sales and NA_Sales demonstrates an exceptional goodness of fit and
# accuracy. The model's high R-squared value of 0.9687 indicates that 
# approximately 97% of the variability in Global Sales can be explained by the 
# variations in EU_Sales and NA_Sales. Both EU_Sales and NA_Sales exhibit strong
# positive relationships with Global Sales, suggesting that an increase in sales
# in these regions is likely to result in a corresponding rise in overall Global
# Sales. The model's reliable predictions offer valuable insights for strategic
# decision-making in the sales department.To further enhance the model, 
# incorporating additional relevant variables, such as marketing expenditure or 
# product attributes,could improve its explanatory power.




###############################################################################
###############################################################################




