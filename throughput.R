rm(list=ls())
dev.off(dev.list()["RStudioGD"])

library(ggplot2)
library(quantreg)

coefficient_of_performance <- 1 -> lambda
number_of_worker           <- 1 -> N
thoughput                  <- 1 -> X
coefficient_of_contention  <- 0 -> sigma
crosstalk                  <- 0 -> kappa

# Perfect linearity

perfect_linearity_as_dataframe <- function(coefficient_of_performance, number_of_worker) {
  data.frame(N = number_of_worker, 
             X = coefficient_of_performance * number_of_worker
  )
}

coefficient_of_performance <- 800
coefficient_of_performance2 <- 1200

number_of_worker <- 1:20

linear_throughput  <- perfect_linearity_as_dataframe(coefficient_of_performance, number_of_worker)
linear_throughput2 <- perfect_linearity_as_dataframe(coefficient_of_performance2, number_of_worker)

ggplot(linear_throughput, mapping = aes(N, X)) + 
  geom_quantile(color="blue", formula = y ~ x) +
  geom_quantile(data = linear_throughput2, color="red", formula = y ~x)

# Amdahl law

amdahl_law_as_dataframe <- function(coefficient_of_performance, number_of_worker, coefficient_of_contention) {
  data.frame(N = number_of_worker, 
             X = coefficient_of_performance * number_of_worker / (1 + coefficient_of_contention * (number_of_worker - 1))
  )
}

coefficient_of_contention <- .05
amdahl_throughput <- amdahl_law_as_dataframe(coefficient_of_performance, number_of_worker, coefficient_of_contention)

ggplot(amdahl_throughput, aes(N, X)) +
  geom_smooth(color="blue", method="loess", formula = y ~ x) +
  geom_point(data = linear_throughput, color = "blue")

# USL
usl_as_dataframe <-  function(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk) {
  data.frame(N = number_of_worker, 
             X = coefficient_of_performance * number_of_worker / (1 + coefficient_of_contention * (number_of_worker - 1) + crosstalk * number_of_worker * (number_of_worker - 1))
  )
}

crosstalk <- 0.02
usl_throughput <- usl_as_dataframe(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk)

ggplot(usl_throughput, aes(N, X)) +
  geom_smooth(color="blue", method="loess", formula = y ~ x) +
  geom_point(data = linear_throughput, color = "blue")+
  geom_point(data = amdahl_throughput, color = "blue")

# Facing real data (from Practical Scalability with USL)

benchmark <- read.csv("data/benchmark.txt", sep=" ")
usl <- nls(tput ~ lambda*size/(1 + sigma*(size-1) + kappa*size*(size-1)), benchmark, start=c(sigma=.1, kappa=.01, lambda=1000))
summary(usl)
sigma  <- coef(usl)["sigma"]
kappa  <- coef(usl)["kappa"]
lambda <- coef(usl)["lambda"]
extrapolated_throughput=function(x){y=x*lambda/(1 + sigma*(x-1) + kappa*x*(x-1))}
plot(extrapolated_throughput, max(benchmark$size)*2, xlab="Size", ylab="Throughput", lty="dashed")
points(benchmark$size, benchmark$tput)

optimal_number_of_worker = floor(sqrt((1-sigma)/kappa))

# Facing half real data

benchmark <- read.csv("data/benchmark.txt", sep=" ")
half_data <- benchmark[1:16,]
usl <- nls(tput ~ lambda*size/(1 + sigma*(size-1) + kappa*size*(size-1)), half_data, start=c(sigma=.1, kappa=.01, lambda=1000))
summary(usl)
sigma  <- coef(usl)["sigma"]
kappa  <- coef(usl)["kappa"]
lambda <- coef(usl)["lambda"]
extrapolated_throughput=function(x){y=x*lambda/(1 + sigma*(x-1) + kappa*x*(x-1))}
plot(extrapolated_throughput, max(half_data$size)*4, xlab="Size", ylab="Throughput", lty="dashed")
points(benchmark$size, benchmark$tput)

optimal_number_of_worker = floor(sqrt((1-sigma)/kappa))
## really pessimistic


# Facing quarter real data

benchmark <- read.csv("data/benchmark.txt", sep=" ")
quarter_data <- benchmark[1:8,]
usl <- nls(tput ~ lambda*size/(1 + sigma*(size-1) + kappa*size*(size-1)), quarter_data, start=c(sigma=.1, kappa=.01, lambda=1000))
summary(usl)
sigma  <- coef(usl)["sigma"]
kappa  <- coef(usl)["kappa"]
lambda <- coef(usl)["lambda"]
extrapolated_throughput=function(x){y=x*lambda/(1 + sigma*(x-1) + kappa*x*(x-1))}
plot(extrapolated_throughput, max(half_data$size)*4, xlab="Size", ylab="Throughput", lty="dashed")
points(benchmark$size, benchmark$tput)

optimal_number_of_worker = floor(sqrt((1-sigma)/kappa))
## optimistic

# Facing three quarter real data

benchmark <- read.csv("data/benchmark.txt", sep=" ")
three_quarter_data <- benchmark[1:24,]
usl <- nls(tput ~ lambda*size/(1 + sigma*(size-1) + kappa*size*(size-1)), three_quarter_data, start=c(sigma=.1, kappa=.01, lambda=1000))
summary(usl)
sigma  <- coef(usl)["sigma"]
kappa  <- coef(usl)["kappa"]
lambda <- coef(usl)["lambda"]
extrapolated_throughput=function(x){y=x*lambda/(1 + sigma*(x-1) + kappa*x*(x-1))}
plot(extrapolated_throughput, max(half_data$size)*4, xlab="Size", ylab="Throughput", lty="dashed")
points(benchmark$size, benchmark$tput)

optimal_number_of_worker = floor(sqrt((1-sigma)/kappa))
## Not bad

# Let's try the USL package

library(usl)
benchmark <- read.csv("data/benchmark.txt", sep=" ")
usl.model <- usl(tput ~ size, benchmark)
summary(usl.model)

peak.scalability(usl.model)
plot(benchmark, pch=16)
plot(usl.model, col="red", add=TRUE)
