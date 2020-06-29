rm(list=ls())
dev.off(dev.list()["RStudioGD"])

library(ggplot2)
library(quantreg)

library(usl)

coefficient_of_performance <- 1 -> lambda
number_of_worker           <- 1:20 -> N
thoughput                  <- 1 -> X
coefficient_of_contention  <- 0.05 -> sigma
crosstalk                  <- 0.01 -> kappa

# Replacing quadratic crosstalk by a nlogn one
quadratic_usl_as_dataframe <-  function(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk) {
  data.frame(N = number_of_worker, 
             X = coefficient_of_performance * number_of_worker / (1 + coefficient_of_contention * (number_of_worker - 1) + crosstalk * number_of_worker * (number_of_worker - 1))
  )
}

nlogn_usl_as_dataframe <-  function(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk) {
  data.frame(N = number_of_worker, 
             X = coefficient_of_performance * number_of_worker / (1 + coefficient_of_contention * (number_of_worker - 1) + crosstalk * log(number_of_worker) * (number_of_worker - 1))
  )
}

ggplot(quadratic_usl_as_dataframe(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk), aes(N, X)) +
  geom_smooth(color = "blue", method="loess", formula = y ~ x) +
  geom_smooth(color = "red",  method="loess", formula = y ~ x, linetype = "dashed", data = nlogn_usl_as_dataframe(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk))

# nlogn on real data

benchmark <- read.csv("data/benchmark.txt", sep=" ")
twelve_first_points <- benchmark[1:12,]
ologn_usl <- nls(tput ~ lambda*size/(1 + sigma*(size-1) + kappa*log(size)*(size-1)), twelve_first_points, start=c(sigma=.1, kappa=.01, lambda=1000))
coefficient_of_contention  <- coef(ologn_usl)["sigma"]
crosstalk  <- coef(ologn_usl)["kappa"]
coefficient_of_performance <- coef(ologn_usl)["lambda"]
extrapolated_throughput=function(x){y=x*coefficient_of_performance/(1 + coefficient_of_contention*(x-1) + crosstalk*log(x)*(x-1))}
plot(extrapolated_throughput, max(benchmark$size), xlab="Size", ylab="Throughput", lty="dashed")
points(benchmark$size, benchmark$tput)

# Not bad