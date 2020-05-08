rm(list=ls())
dev.off(dev.list()["RStudioGD"])

library(ggplot2)
library(quantreg)
library(usl)

coefficient_of_performance <- 1000 -> lambda
throughput                 <- 1    -> X
coefficient_of_contention  <- 0     -> sigma
crosstalk                  <- 0     -> kappa
response_time              <- 1     -> R

number_of_worker  <- throughput * response_time -> N


number_of_worker <- 1:60
#Response time with USL
usl_response_time_as_dataframe <- function(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk){
  data.frame(
    N = number_of_worker,
    R = (1 + coefficient_of_contention*(number_of_worker - 1) + crosstalk*number_of_worker*(number_of_worker-1))/lambda
  )
}

benchmark <- read.csv("data/benchmark.txt", sep=" ")
usl.model <- usl(tput ~ size, benchmark)
coefficient_of_performance <- usl.model@coefficients['gamma'] -> real_coefficient_of_performance
coefficient_of_contention  <- usl.model@coefficients['alpha'] -> real_coefficient_of_contention
crosstalk                  <- usl.model@coefficients['beta']  -> real_crosstalk

real_response_time <- data.frame(
  N = 1:length(benchmark$tput),
  R = benchmark$size / benchmark$tput
)
usl_response_time_no_crosstalk_no_contention <- usl_response_time_as_dataframe(coefficient_of_performance, number_of_worker, 0, 0)
usl_response_time_no_crosstalk <- usl_response_time_as_dataframe(coefficient_of_performance, number_of_worker, coefficient_of_contention, 0)
usl_response_time <- usl_response_time_as_dataframe(coefficient_of_performance, number_of_worker, coefficient_of_contention, crosstalk)
ggplot(usl_response_time_no_crosstalk_no_contention, aes(N, R)) +
  geom_smooth(color="blue", method="loess", formula = y ~ x) +
  geom_smooth(color="red",  method="loess", formula = y ~x, data=usl_response_time_no_crosstalk) +
  geom_smooth(color="black",  method="loess", formula = y ~x, data=usl_response_time) +
  geom_point(color="purple", data=real_response_time)


# Response time in constant lineary scalable system
coefficient_of_contention  <- .06
crosstalk                  <- .06
coefficient_of_performance <- 40
throughput = 1:600

linear_system_response_time_no_crosstalk_as_dataframe <- function(coefficient_of_performance, coefficient_of_contention, throughput) {
  data.frame(
    X = throughput,
    R = (coefficient_of_contention - 1)/(coefficient_of_contention*throughput - coefficient_of_performance)
  )
}

linear_system_response_time_no_crosstalk <- linear_system_response_time_no_crosstalk_as_dataframe(coefficient_of_performance, coefficient_of_contention, throughput)

ggplot(linear_system_response_time_no_crosstalk, aes(X, R)) +
  geom_smooth(color="blue", method="loess", formula = y ~ x)

linear_system_response_time_as_dataframe <- function(coefficient_of_performance, coefficient_of_contention, crosstalk, throughput) {
  data.frame(
    X = throughput,
    R = (crosstalk*throughput + coefficient_of_performance - coefficient_of_contention*throughput - sqrt(throughput^2*(crosstalk^2 + 2*crosstalk*(coefficient_of_contention - 2) + coefficient_of_contention^2) + 2*coefficient_of_performance*throughput*(crosstalk - coefficient_of_contention) + coefficient_of_performance^2))/(2*crosstalk*throughput^2)
  )
}

linear_system_response_time <- linear_system_response_time_as_dataframe(coefficient_of_performance, coefficient_of_contention, crosstalk, throughput)

ggplot(linear_system_response_time, aes(X, R)) +
  geom_smooth(color="blue", method="loess", formula = y ~ x)

## Response time is not a simple function of throughput if crosstalk is nonzero

linear_system_throughput_as_dataframe <- function(coefficient_of_performance, coefficient_of_contention, crosstalk, response_time) {
  data.frame(
    R = response_time,
    X = (sqrt(coefficient_of_contention^2 + crosstalk^2 + 2*crosstalk*(2*coefficient_of_performance*response_time + coefficient_of_contention -2)) - crosstalk + coefficient_of_contention)/(2*crosstalk*response_time)
  )
}

response_time <- 1:240/100
linear_system_throughput <- linear_system_throughput_as_dataframe(coefficient_of_performance, coefficient_of_contention, crosstalk, response_time)

ggplot(linear_system_throughput, aes(R, X)) +
  geom_smooth(color="blue", method="loess", formula = y ~ x)

real_response_time_from_throughput <- data.frame(
  X = benchmark$tput,
  R = real_response_time$R
)
response_time_from_throughput <- linear_system_response_time_as_dataframe(real_coefficient_of_performance, real_coefficient_of_contention, real_crosstalk, benchmark$tput)
response_time_from_throughput_no_crosstalk <- linear_system_response_time_no_crosstalk_as_dataframe(real_coefficient_of_performance, real_coefficient_of_contention, benchmark$tput)

ggplot(response_time_from_throughput, aes(X, R)) +
  geom_smooth(color="red", method="loess", formula = y ~ x) +
  geom_smooth(color="black", method="loess", formula = y ~ x, data = response_time_from_throughput_no_crosstalk) +
  geom_point(color="blue", data = real_response_time_from_throughput)

