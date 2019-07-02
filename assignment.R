# Read CSV File
mydata = read.csv("Car_sales.csv")
head(mydata)

# Converting factor variable into numeric variable
mydata[, c(4,6:14)] <- sapply(mydata[, c(4,6:14)], as.numeric)
head(mydata)

# Scatter Plots
plot(Price.in.thousands ~ Sales.in.thousands, mydata, xlab="Sales", ylab="Prices") # ScatterPlot 1
abline(lm(Price.in.thousands ~ Sales.in.thousands, mydata), col="red")

plot(X4.year.resale.value ~ Sales.in.thousands, mydata, xlab="Sales", ylab="Resale Value") # ScatterPlot 2
abline(lm(X4.year.resale.value ~ Sales.in.thousands, mydata), col="red")

# Boxplots
boxplot(mydata$Price.in.thousands, main="Price")  # box plot for 'Price'

boxplot(mydata$Sales.in.thousands, main="Sales")  # box plot for 'Sales'

# Normalization
newdata = mydata[, -c(1,2,5,15)]
m <- apply(newdata, 2, mean)
s <- apply(newdata, 2, sd)
newdata <- scale(newdata, m, s)
head(newdata)

# EDA
library(DataExplorer)
plot_str(mydata)
plot_missing(mydata)
plot_histogram(mydata)
plot_density(mydata)
plot_bar(mydata)
plot_correlation(mydata, type = 'continuous')


# Regression
linearMod <- lm(Price.in.thousands ~ Sales.in.thousands, data = mydata) # Linear Regression
summary(linearMod)

logitMod1 = glm(X4.year.resale.value ~ Sales.in.thousands, data = mydata) # logistic regression
summary(logitMod1)

logitMod2 <- glm(Price.in.thousands ~ Engine.size + Horsepower + Wheelbase + Width + Length + Curb.weight + Fuel.capacity + Fuel.efficiency, data=mydata)
summary(logitMod2)


