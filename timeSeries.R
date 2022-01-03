#case study on time series
AirPassengers
passengers <- decompose(AirPassengers)
passengers

plot(passengers$trend, main = "Trend", xlab = "Jan 1949 to Dec 1960", ylab = "No.of Passengers")
plot(passengers$seasonal, main = "Seasonal", xlab = "Jan 1949 to Dec 1960", ylab = "No.of Passengers")
plot(ts(passengers$seasonal[1:12]), main = "Seasonal monthly data for 1949", xlab = "Jan 1949 to Dec 1949", ylab = "No.of Passengers")

data(AirPassengers)
class(AirPassengers)

start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)
plot(AirPassengers)
abline(reg = lm(AirPassengers ~ time(AirPassengers)))
cycle(AirPassengers)
plot(aggregate(AirPassengers, FUN = mean))
boxplot(AirPassengers ~ cycle(AirPassengers))

#ARIMA MODEL
r <- scan("C:/Users/Anurag/Desktop/New folder/datasets/attendance.txt")




