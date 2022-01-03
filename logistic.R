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

#multinomial on icecream
library(nnet)
m <- read.csv("C:/Users/Anurag/Desktop/New folder/datasets/icecream.csv")
model <- multinom(response ~ butscot + vanila, data = m)
summary(model)

#multinomial on iris
IrisDataset <- iris
IrisDataset$SpeciesReleveled <- relevel(IrisDataset$Species, ref = "virginica")
levels(IrisDataset$SpeciesReleveled)

split <- sample.split(IrisDataset, SplitRatio = 0.6)
training <- subset(IrisDataset[c(-5)], split == TRUE)
testing <- subset(IrisDataset[c(-5)], split == FALSE)

model <- multinom(SpeciesReleveled ~ ., data = training)
print(model)

random_test_obs <- testing[c(6, 13, 22, 34, 49, 53), ]
print(random_test_obs)

predicted_prob <- data.frame(predict(model, random_test_obs, type = "prob"))
print(predicted_prob)

predicted_class <- data.frame(predict(model, random_test_obs, type = "class"))
print(predicted_class)

actual_class <- random_test_obs$SpeciesReleveled

compare <- data.frame(actual_class, predicted_class)
print(compare)

predicted_class <- predict(model, testing, type = "class")
print(predicted_class)

actual_class <- testing$SpeciesReleveled

addmargins(table(actual_class, predicted_class))

