rm(list=ls())
dev.off(dev.list()["RStudioGD"])

library(ggplot2)
library(quantreg)

library(usl)
benchmark <- read.csv("data/benchmark.txt", sep=" ")


usl_on_ten_points <- usl(tput ~ size, benchmark[1:10,])

peak.scalability(usl_on_ten_points)
coefficient_of_contention <- coef(usl_on_ten_points)["alpha"]
crosstalk <- coef(usl_on_ten_points)["beta"]
coefficient_of_performance <- coef(usl_on_ten_points)["gamma"]

optimum_number_worker <- sqrt((1 - coefficient_of_contention)/crosstalk)
optimum_number_worker_with_usl_package <- peak.scalability(usl_on_ten_points)
max_throughput <- usl_on_ten_points$peak["tput"] 

usl_as_dataframe <- function(usl_model, number_of_worker) {
  data.frame(N = number_of_worker, 
             X = scalability(usl_model)(number_of_worker))
}

ggplot(usl_as_dataframe(usl_on_ten_points, 1:80), aes(N, X)) +
  geom_smooth(color = "black", method="loess", formula = y ~ x) +
  geom_point(data = benchmark[1:10,], color = "blue", mapping = aes(size, tput))

# with all data
ggplot(usl_as_dataframe(usl_on_ten_points, 1:80), aes(N, X)) +
  geom_smooth(color = "black", method="loess", formula = y ~ x) +
  geom_point(data = benchmark, color = "red", mapping = aes(size, tput))

# I agree with practical USL book, never assume more than 2 time your current range.

# TODO: add response time



