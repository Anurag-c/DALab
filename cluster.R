head(iris)
new_iris <- iris
new_iris$Species <- NULL # deleting the column
head(new_iris)

model <- kmeans(new_iris, 3)
model

table(iris$Species, model$cluster)

plot(new_iris[c("Sepal.Length", "Sepal.Width")], col = model$cluster)
points(model$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 2)
                     
                     