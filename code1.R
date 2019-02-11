
#read the data from url 
housing_data<-read.csv("https://www.utdallas.edu/~sxp175331/datasets/housing.csv")


#Since Ocean proximity is a categorical variable
#we need to find all the disntinct categories

#x <- unique(housing_data$ocean_proximity)

#encoding all categories to numbers

#housing_data$ocean_proximity= factor(housing_data$ocean_proximity,
#                                     levels = c('<1H OCEAN','INLAND','ISLAND','NEAR BAY','NEAR OCEAN'),
#                                     labels = c(1,2,3,4,5))

#-----------------------convert categories

library(dplyr)

categories = unique(housing_data$ocean_proximity)
#split the categories off
cat_housing = data.frame(ocean_proximity = housing_data$ocean_proximity)

for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}

for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))


#-------------------------------------------------

#check if there are any NULL values in any
# of the columns

print('checking if any null values')
na_count <-sapply(housing_data, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)  # we got 207 null values in total_bedrooms column

#replace Null value in a column with its mean

for(i in 1:ncol(housing_data)){
  housing_data[is.na(housing_data[,i]), i] <- mean(housing_data[,i], na.rm = TRUE)
}

#Scaling the numerical variables

drops= c('ocean_proximity','median_house_value')
housing_new_data=housing_data[, !(names(housing_data) %in% drops)]
scaled_housing_new_data = scale(housing_new_data)

housing_mod_data = cbind(cat_housing,scaled_housing_new_data,median_house_value=housing_data$median_house_value)


par(mfrow=c(1,1))
library(e1071)
# density plot for 'median_house_value'
plot(density(housing_mod_data$median_house_value), main="Density Plot: Median House Price ", 
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(housing_mod_data$median_house_value), 2)))  
polygon(density(housing_mod_data$median_house_value), col="red")

#we see that the distribution for Median House Price is not normal

attach(housing_mod_data)

par(mfrow=c(1,1))
#check the correlation between various variables using corrplot
require(corrplot)
corrplot(cor(housing_mod_data), method = "number")

#we see that some of the features like 'total_rooms', 'total_bedrooms', 'population'and 'households' are highly correlated with each other.
#Also latitude and longitude are highly correlated which is obvious as we are using california state data 

#There is moderate linear relationship between median income and and house value
par(mfrow=c(1,1))
plot(housing_mod_data$median_house_value ~ housing_mod_data$median_income, xlab = "Median Income", ylab = "Median House Price", col = 'blue')

fit<-lm(median_house_value ~ median_income)
summary(fit)
abline(fit, lwd = 4, col = 'red')

#scatter plots for median house value wrt longitude and wrt latitude 
par(mfrow=c(1,2))
plot(median_house_value~longitude, col = 'red')
plot(median_house_value~latitude, col = 'red')
#we dont see any correlation between them

#scatter plots for median house value wrt 'total_rooms', 'total_bedrooms', 'population'and 'households'
#par(mfrow=c(2,2))
#plot(median_house_value~total_rooms)
#plot(median_house_value~total_bedrooms)
#plot(median_house_value~population)
#plot(median_house_value~households)




fit1<-lm(median_house_value ~ latitude + longitude)
summary(fit1) 

#We can see p value is very small,hence we will reject
#The null hypothesis which is "lat & long collectively have no effect on house price"
#also latitude alone & longitude are significant in controlling the house price

#Lets include another column housing_median_age

fit2 <- lm(median_house_value ~ latitude + longitude + housing_median_age)
summary(fit2)
#we can see p value is around 0.0187 so we cannot reject the null hypothesis ,
#therefore we can conclude that housing_median_age column doesnot contribute
#towards house price


#for fit3 we have added the Ocean Proximity variables.
fit3 <- lm(median_house_value  ~ latitude + longitude + total_rooms + total_bedrooms + population + households + median_income + `<1H OCEAN` + INLAND + ISLAND + `NEAR OCEAN` + `NEAR BAY`)
summary(fit3)
#we can see p value is around 0.0217, 0.7516, and NA. so we cannot reject the null hypothesis ,
#therefore we can conclude that Ocean Proximity variables do not contribute
#towards house price

fit4 <- lm(median_house_value  ~ latitude + longitude + total_rooms + total_bedrooms + population + households + median_income )
summary(fit4)

#------------)------------------------------------------------------------------------------
#target variable is median_house_value
#rest of the variables are features

install.packages(caTools)
library(caTools)
split = sample.split(housing_mod_data$median_house_value,SplitRatio = 0.8)

training_data = subset(housing_mod_data,split == TRUE)
test_data = subset(housing_mod_data,split == FALSE)


#Fitting multiple Linear regression to the Training Set

regressor = lm(formula = median_house_value  ~ latitude + longitude + total_rooms + total_bedrooms + population + households + median_income,
               data = training_data)

#Prediction on test set

y_pred = predict(regressor,newdata = test_data)

#Residual sum of squares (RSS) is the sum of the squared residuals
rss <- c(crossprod(regressor$residuals))
mse <- rss/length(regressor$residuals)
rmse <- sqrt(mse)
rmse

