
#read the data from url 
housing_data<-read.csv("https://www.utdallas.edu/~sxp175331/datasets/housing.csv")



#view the dataset and its structure
View(housing_data)
str(housing_data)

detach(housing_data2)


#scale the data
housing_data[1:8] <- lapply(housing_data[1:8], function(x) c(scale(x)))

#from the structure we can see that "ocean_proximity" has 5 categories
View(housing_data)

#transform categorical data to make each category as a predictor varaible
housing_data2<-cbind(housing_data[1:9], sapply(levels(housing_data$ocean_proximity), function(x) as.integer(x == housing_data$ocean_proximity)))

#view the updated dataset
View(housing_data2)
str(housing_data2)

#now we have 13 predictor variables


#ignore this block for now
#convert categorical into numerical
#convert<-sapply(housing_data,is.factor)       
#ocean_proximity<-sapply(housing_data[,convert],unclass)    
#housing_data<-cbind(housing_data[,!convert],ocean_proximity)  


#check if the dataset contains NULL values
print('checking if any null values')
na_count <-sapply(housing_data, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)  
na_count

#We get 207 null values in total_bedrooms column

#So we replace Null value in a column with its mean
for(i in 1:ncol(housing_data2)){
  housing_data2[is.na(housing_data2[,i]), i] <- mean(housing_data2[,i], na.rm = TRUE)
}

library(e1071)
# density plot for 'median_house_value'
plot(density(housing_data2$median_house_value), main="Density Plot: Median House Price ", 
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(housing_data2$median_house_value), 2)))  
polygon(density(housing_data2$median_house_value), col="red")

#we see that the distribution for Median House Price is not normal

attach(housing_data2)

#check the correlation between various variables using corrplot
require(corrplot)
corrplot(cor(housing_data2), method = "number")

#we see that some of the features like 'total_rooms', 'total_bedrooms', 'population'and 'households' are highly correlated with each other.
#Also latitude and longitude are highly correlated which is obvious as we are using california state data 

#There is moderate linear relationship between median income and and house value
plot(housing_data2$median_house_value ~ housing_data2$median_income, xlab = "Median Income", ylab = "Median House Price")
fit<-lm(median_house_value ~ median_income)
summary(fit)

abline(fit, lwd = 4, col = 'red')




myvars <- c("median_house_value","longitude","latitude")
housing_data3 = housing_data2[myvars]
plot(housing_data3)



fit1<-lm(median_house_value ~ latitude + longitude)
summary(fit1) 

#We can see p value is very small,hence we will reject
#The null hypothesis which is "lat & long collectively have no effect on house price"
#also latitude alone & longitude are significant in controlling the house price

#Lets include another column housing_median_age

fit2 <- lm(median_house_value ~ latitude + longitude + housing_median_age)
summary(fit2)
#we can see p value is around 0.02 so we cannot reject the null hypothesis ,
#therefore we can conclude that housing_median_age column doesnot contribute
#towards house price

#Now lets add few more variables to the model
fit3 <- lm(median_house_value ~ latitude + longitude + total_rooms + total_bedrooms +
               population + households + median_income )
summary(fit3) #every variable is pretty much significant


#for fit4 we have added the Ocean Proximity variables.
fit4 <- lm(median_house_value  ~ latitude + longitude + total_rooms + total_bedrooms + population + households + median_income + `<1H OCEAN` + INLAND + ISLAND + `NEAR OCEAN` + `NEAR BAY`)
summary(fit4)

#from fit4 we can see that NEAR OCEAN is insignificant and NEAR BAY has NA as coeffcients.
#that means NEAR BAY is linearly related to some other variable. So we can drop these two

#Dropping NEAR BAY and NEAR OCEAN
fit5 <- lm(median_house_value  ~ latitude + longitude + total_rooms + total_bedrooms + population + households + median_income + `<1H OCEAN` + INLAND + ISLAND)
summary(fit5)

#we will do a combination of variable (product and sum)
fit6 <- lm(median_house_value  ~ (latitude * longitude) + (total_bedrooms * total_rooms * population) + median_income + `<1H OCEAN` + INLAND + ISLAND)
summary(fit6)

#------------)------------------------------------------------------------------------------
#target variable is median_house_value
#rest of the variables are features

install.packages(caTools)
library(caTools)
split = sample.split(housing_data2$median_house_value,SplitRatio = 0.8)

training_data = subset(housing_data2,split == TRUE)
test_data = subset(housing_data2,split == FALSE)


#Fitting multiple Linear regression to the Training Set

regressor = lm(formula = median_house_value  ~ latitude + longitude + total_rooms + total_bedrooms + population + households + median_income + `<1H OCEAN` + INLAND + ISLAND,
               data = training_data)

#Prediction on test set

y_pred = predict(regressor,newdata = test_data)

#Residual sum of squares (RSS) is the sum of the squared residuals
rss <- c(crossprod(regressor$residuals))


mse <- rss/length(regressor$residuals)
rmse <- sqrt(mse)
rmse

