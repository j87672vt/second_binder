library(tidyverse)
library(Hmisc)
library(performance)

crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")
head(crime)


#Tidying Data - separate columns
crime <- separate(crime, col = "City, State", into = c("City", "State"))
head(crime)



#Tidying Data - Change heading names
crime <- crime %>%
  rename(House_Price = index_nsa) %>% #change index_nsa TO House_Price
  rename(Violent_Crimes = "Violent Crimes")#Change violent crimes TO Violent_Crimes
   
head(crime)

#Plotting the data - population vs crime rate with best fit line
crime %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() + #scatter
  geom_smooth(method = "lm", se = FALSE) + # best fit line uses Linear Model
  theme_minimal() +
  theme(text = element_text(size = 13)) %>%
  labs(x = "Population",
      y = "Violent Crimes")

#Pearson's R
rcorr(crime$Population, crime$Violent_Crimes)

#r=0.81, p<0.001 so ~64% of variance in Violent_crimes variable is explained by 
#the pop. size variable. There is a clear positive relationship between populat-
#ion size and violent crime. From plot, could conclude the relationship is overly
#influenced by crime in a small number of very large cities - so we could exclude 
#them.

#Exclude large cities
crime_filtered <- filter(crime, Population < 2000000)

#Redo plot with filtered data + translucent plots to see overlap
crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point(alpha = .25) + #translucent as alpha is less than 1
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes")

#Pearson's R
rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

#r = .69 so still a clear positive corr., though not quite as strong. Next will 
#make a linear model, but only want each city to appear once instead of once per
#year. Will filter for 2015.

crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) + #add city labels, makes sure they don't overlap
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 1800000) + #x-coordinate limits
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes")

#From plot see clear positive correlation. Now Pearson's R
rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)
 #r = 0.65

#Model the data - 2 linear models, one with outcome variable mean as the predictor, 
#second using pop size to predict violent crimes outcome.

model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered) #using violent crimes mean
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)#using population to predict.

library("see")
check_model(model2)
#small dataset so don't all look great atm.

anova(model1, model2)
#model2 is better than just using the mean. RSS is smaller for second model so 
#deviation between observed data and regression model is significantly less in 
#model2 than model1.

#interpreting the model

summary(model2)
#intercept is where regression line crosses y-axis: for every increase in pop by 1 
#there is an extra 0.006963 increase in violence. For a city of about 1 million,
#there will be about 7907 crimes (0.006963*1,000,000 + 944.3). This fits the 
#regression line! ***HOW TO EXCLUDE OUTLIERS - WHICH ARE OUTLIERS??***

#THE CHALLENGE

#1. See if same relationship is there for population size and robberies in 2015.
crime_filtered %>% #scatter plot
  ggplot(aes(x = Population, y = Robberies, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) + #add city labels, makes sure they don't overlap
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 1800000) + #x-coordinate limits
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Robberies")

rcorr(crime_filtered$Population, crime_filtered$Robberies)#Pearson's R
#r=0.63 - positive correlation

#model the data
model1 <- lm(Robberies ~ 1, data = crime_filtered)#using robbery mean
model2 <- lm(Robberies ~ Population, data = crime_filtered) #using pop as predictor

check_model(model2)

anova(model1, model2)
#model2 is better than just the mean as RSS is smaller.

summary(model2)
#for every increase in pop by 1 there is a 0.0002729 increase in robberies. For
#city of around 1 mill, that equates to 0.0002729*1,000,000 + 178.5 = 451.4

#2. See if house prices are predicted by violent crime numbers in 2015.
crime_filtered %>%
  ggplot(aes(x = Violent_Crimes, y = House_Price)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Violent Crimes",
       y = "House Prices")

rcorr(crime_filtered$Violent_Crimes, crime_filtered$House_Price)
#weak negative correlation, r = -0.18. Next model the data

model1 <- lm(House_Price ~ 1, data = crime_filtered)
model2 <- lm(House_Price ~ Violent_Crimes, data = crime_filtered)

check_model(model2)
#not good predictor, little linearity, normality or homogeneity (see plot)

anova(model1, model2)
#model 2 is still better than model1 as RSS is smaller for 2 than 1.

#3. See if house prices are predicted by population size in 2015.
crime_filtered %>%
  ggplot(aes(x = Population, y = House_Price)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "House Prices")

rcorr(crime_filtered$Population, crime_filtered$House_Price)
#weak positive correlation, but stronger than house price and violent crime. r =0.3721

model1 <- lm(House_Price ~ 1, data = crime_filtered)
model2 <- lm(House_Price ~ Population, data = crime_filtered)

check_model(model2)
#not much normality or homogeneity

anova(model1, model2)
#model2 is only marginally better than model1 (RSS is only slightly smaller). 
#F is bigger than P value, but again only slightly so population is NOT a good 
#predictor of House price.