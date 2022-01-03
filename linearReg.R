climate_data <- read.csv("C:/Users/Anurag/Desktop/New folder/datasets/climate_change.csv")
summary(climate_data)

model <- lm(climate_data$Month ~ climate_data$Temp)
summary(model)

cor(climate_data)

plot(climate_data$Month, climate_data$Temp, abline(model), cex = 1.3, pch = 16)

boxplot(climate_data$Month)
boxplot(climate_data$Temp)

##############################################################################################

placement_data <- read.csv("C:/Users/Anurag/Desktop/New folder/datasets/placement_data.csv")
summary(placement_data)

model <- lm(placement_data$hsc_p ~ placement_data$degree_p)
summary(model)

cor(placement_data)

plot(placement_data$hsc_p, placement_data$degree_p,abline(model), cex = 1.3, pch = 16, xlab = "hsc_p", ylab = "degree")

par(mfrow=c(1,2))
a <- placement_data
boxplot(a$hsc_p,main="hsc_p",sub=paste("Outlier rows:"),boxplot.stats(a$hsc_p)$out)
boxplot(a$degree_p,main="degree",sub=paste("Outlier rows:"),boxplot.stats(a$degree_p)$out)

#############################################################################################



##############################################################################################

#read.csv,read.xlsx,library(pakage),readLine(web)
#fromJSON(file = "path"),xmlParse(file = "path")

#Single category
Eligibility <- as.factor(c(1, 0))
res <- cbind(clear = c(6, 5), notclear = c(9, 10))
model <- glm(res ~ Eligibility, family = binomial("logit"))
summary(model)

#Single category predict V-engine
install.packages('caTools')
library(caTools)

split <- sample.split(mtcars,SplitRatio = 0.8)
training <- subset(mtcars, split == TRUE)
testing <- subset(mtcars, split == FALSE)

model <- glm(vs ~ wt + disp, family = "binomial", data = training)
res <- predict(model, testing, type = "response")

table(ActualValue = testing$vs, predictedValue = (res > 0.5))
(4 + 3) / 8

#3-way and K-way Tables
E <- factor(c("NA", "C", "A"))
res <- cbind(clear = c(6,5,10), notclear = c(9,10,0))
EN = (E == "N")
EC = (E == "C")
bl3 = glm(res~EC+EN, family = binomial("logit"))
summary(bl3)

#continuous covariates
r <- read.csv("C:/Users/Anurag/Desktop/New folder/datasets/studata.csv")
model <- glm(r$Eligibility ~ r$AnnualAttendance, family = binomial("logit"))
summary(model)

