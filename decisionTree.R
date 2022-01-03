#simple decision tree
a <- c(1:50)
cb <- data.frame(a)

library(party)
model <- ctree(a ~ ., data = cb)
plot(model)
model

#cars dataset
print(head(cars))
model <- ctree(speed ~ dist, data = cars)
plot(model)
model

#saving as png
readingSkills[c(1 : 100), ]
inputData <- readingSkills[c(1 : 105), ]
png(file = "decision_tree.png")
model <- ctree(nativeSpeaker ~ age + shoeSize + score, data = inputData)
plot(model)
model
dev.off()

#reading skills
library(rpart)
nativeSpeaker_find <- data.frame("age" = 11, "shoeSize" = 30.63692, "score" = 55.721149)
model <- rpart(nativeSpeaker ~ age + shoeSize + score, data = readingSkills)
prediction <- predict(model, newdata = nativeSpeaker_find, type = "class")
print(prediction)

#air quality
head(airquality)
airq <- subset(airquality, !is.na(Ozone))
head(airq)
model <- ctree(Ozone ~ ., data = airq, controls = ctree_control(maxsurrogate = 3))
model
plot(model)

#iris dataset
head(iris)
model <- ctree(Species ~ ., data = iris, controls = ctree_control(maxsurrogate = 3))
model
plot(model)

species_find <- data.frame("Sepal.Length" = 5.1, "Sepal.Width" = 3.5, "Petal.Length" = 1.4, "Petal.Width" = 0.2)
model <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
prediction <- predict(model, newdata = species_find, type = "class")
print(prediction)
plot(model)
text(model, use.n = TRUE)
plotcp(model)

#mtcars dataset
head(mtcars)
model <- ctree(am ~ disp + hp + mpg, data = mtcars)
model
plot(model)

#purity
IsPure <- function(data) {
  length(unique(data[n, ncol(data)])) == 1
}

#Entropy
Entropy <- function(vls) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

Entropy(c(10, 0))
Entropy(c(4, 2))

#Info Gain
InformationGain <- function(tble) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum(s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

m <- read.csv("C:/Users/Anurag/Desktop/New folder/datasets/mango.csv")
mt <- table(m[, c('color', 'edibility')])
InformationGain(mt)
mt <- table(m[, c('size', 'edibility')])
InformationGain(mt)


