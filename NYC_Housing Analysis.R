
#Import data 
setwd("~/Desktop/...")      # change directory
housing <- data.frame(read.csv("NYC_housing.csv"))

#Explore 
summary(housing)
str(housing)
dim(housing)


# The number of missing values in each column 
apply(is.na(housing), 2, sum)

# Remove NAs
housing <- housing[!(is.na(housing$Value)),]

# Find quantiles
quantile(housing$Value)

# Add new column with two values High and NotHigh  based on the third qualitile value
housing$HighValu <- ifelse(housing$Value > Q3, "High", "NotHigh")  

# Filter by Borough
contingency_table <- (tab1<-table(housing$Borough, housing$HighValu)/length(housing$HighValu))
contingency_table


# the proportion of properties in Bronx & in the upper quartile 
#(1)
print(paste("proportion_1 = ", contingency_table[1,1] / sum(contingency_table) ))
#(2)
print(paste("proportion_2 = ", mean(housing$Borough == "Bronx" & housing$HighValu == "High") ))

# Add new columns as log of housing value and log of number of units 
housing$logValue <- log(housing$Value)
housing$logUnits <- log(housing$UnitCount)


# Compare the housing value per borough
library(ggplot2)

ggplot(housing, aes(Borough,Value)) + geom_boxplot(fill = "red", colour = "#a3a4a8", alpha = 0.8) +
  xlab("Borough") + ylab("Value") +
  labs(title = "Property Value Across Five Boroughs", subtitle = "linear scale") +
  theme_minimal() 


ggplot(housing, aes(Borough,logValue)) + geom_boxplot(fill = "orange", colour = "#a3a4a8", alpha = 0.8) +
  xlab("Borough") + ylab("log Value") +
  labs(title = "Property Value Across Five Boroughs", subtitle = "log scale") +
  theme_minimal() 



# Plot the housing value Vs.number of units 

ggplot(housing, aes(logUnits, logValue)) + geom_point(alpha = 1/4, colour = "#09c4be", size = 1) +
  xlab("Log Units") + ylab("Log Value") +
  labs(title = "Property Units Value", subtitle = "log scale") +
  theme_minimal()  



# Add new colum indicating whether the property is built before or after 1950
housing$after1950 <- ifelse(housing$YearBuilt >= 1950, TRUE , FALSE)  


# Plot the housing value Vs.number of units again color coded by the year built 
ggplot(housing, aes(logUnits, logValue, col=as.factor(housing$after1950))) + geom_point(alpha = 1/4, size = 1) +
  xlab("Log Units") + ylab("Log Value") +
  labs(title = "Property Units Value", subtitle = "log scale") + scale_color_manual(values= c("#e89b27", "#c42000"), name="Year Built", labels=c("Before 1950", "After 1950")) +
  theme_minimal()  



# Plot the housing value Vs.number of units one last time color coded by borough (Manhattan and Brooklyn only)
plot_data <- subset(housing, Borough == "Manhattan" | Borough == "Brooklyn", 
select=c(logValue, logUnits,Borough))

ggplot(plot_data, aes(logUnits, logValue, col=as.factor(plot_data$Borough))) + geom_point(alpha = 1/4, size = 1) +
  xlab("Log Units") + ylab("Log Value") +
  labs(title = "Property Units Value Color Coded By Borough ", subtitle = "log scale") +
  scale_color_manual(values= c("#bc32bc", "#1c8fb2"), name="Borough", labels=c("Brooklyn", "Manhattan")) +
  theme_minimal()  


# Fins median property value per borough
tapply(housing$Value,housing$Borough,median)





