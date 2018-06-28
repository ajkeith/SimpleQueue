
library(simmer)
library(tidyverse)
library(stringr)
library(parallel)
library(stats)
library(distr)
library(entropy)
library(ggplot2)


options(scipen = 999)


seed = 1269
set.seed(seed)

settings <- read_csv("design_v18.csv")

n_runs <- length(settings$`Estimation Method`)

time_limit <- 2000000
server_limit <- 19
n_cust <- 1000
lambda <- 20
norm_var <- 3
a <- 1

# rho <- 0.95
# n_server <- 2
# mu <- (lambda/(n_server*rho))
# a_t <- 1
# 
# n_cust = 100000
# x <- 1:n_cust
# d_exp <- rexp(n_cust, lambda)
# d_lognormal <- rlnorm(n_cust, log((1/lambda)) - norm_var/2, sqrt(norm_var))
# d_beta <- rbeta(n_cust, a_t, (a_t / (1/lambda)) - a_t)
# d_unif <- runif(n_cust, 0, 2*(1/lambda))
# df <- as.tibble(data.frame(x, d_exp, d_lognormal, d_beta, d_unif))
# df2 <- df %>%
#   gather('d_exp','d_lognormal','d_beta','d_unif', key = 'distribution', value = 'time')
# mean(d_exp)
# mean(d_beta)
# mean(d_unif)
# mean(d_lognormal)
# 
# df2 %>%
#   ggplot() +
#   geom_density(aes(time, color = distribution)) +
#   scale_x_continuous(limits = c(0,0.3)) +
#   scale_y_continuous(limits = c(0,12)) +
#   labs(title = 'Service Distributions', x = 'Service Time', y = 'Density')

# df2 %>%
#   ggplot() +
#   geom_density(aes(time)) +
#   facet_wrap( ~ distribution, scales = 'free')
# df2 %>%
  # filter(distribution == 'd_lognormal') %>%
  # ggplot() +
  # geom_density(aes(time)) +
  # scale_x_continuous(limits = c(0,0.3))

for (i in 1:n_runs){
  n_server <- settings[i,5][[1]]
  rho <- settings[i,6][[1]]
  mu <- (lambda/(n_server*rho))
  a_l <- 1
  a_m <- 1
  d_c_name <- settings[i,2][[1]]
  d_s_name <- settings[i,3][[1]]
  if (d_c_name == "Exponential"){
    d_cust = rexp(n_cust - 1, lambda) # pre-calc all interarrival times  
  } else if(d_c_name == "LogNormal") {
    d_cust = rlnorm(n_cust - 1, log(1/lambda) - norm_var/2, sqrt(norm_var)) # pre-calc all interarrival times  
  } else if(d_c_name == "Beta") {
    d_cust = rbeta(n_cust - 1, a_l, (a_l / (1/lambda)) - a_l) # pre-calc all interarrival times  
  } else if(d_c_name == "Uniform") {
    d_cust = runif(n_cust - 1, 0, 2*(1/lambda)) # pre-calc all interarrival times  
  } else if(d_c_name == "Deterministic") {
    d_cust = rep(n_cust - 1, 1/lambda) # pre-calc all interarrival times  
  }
  if (d_s_name == "Exponential"){
    d_server = Exp(mu) # create distribution object for online service times
  } else if (d_s_name == "LogNormal") {
    d_server = Lnorm(log(1/mu) - norm_var/2, sqrt(norm_var)) # create distribution object for online service times
  } else if (d_s_name == "Beta") {
    d_server = Beta(a_m, (a_m / (1/mu)) - a_m) # create distribution object for online service times
  } else if (d_s_name == "Uniform") {
    d_server = Unif(0,2*(1/mu)) # create distribution object for online service times
  }
  
  customer <-
    trajectory("customer path") %>%
    seize("server") %>%
    timeout(function() {
      if (d_s_name == "Deterministic"){
        (1/mu)
      } else {r(d_server)(1)}}) %>%
    release("server")
  
  sim <-
    simmer("sim") %>%
    add_resource("server", n_server) %>%
    add_generator("customer", customer, function() {c(0, d_cust, -1)})
  
  sim %>% run(until = time_limit)
  
  result_all <- sim %>% 
    get_mon_arrivals %>%
    as_tibble() %>%
    rename(arrive_time = start_time, depart_time = end_time, service_wait = activity_time) %>%
    mutate(service_time = depart_time - service_wait) %>%
    mutate(queue_wait = depart_time - arrive_time - service_wait) %>%
    separate(name, into = c("name","depart_order"), sep = 8, convert = TRUE) %>%
    mutate(depart_order = depart_order + 1) %>%
    select(arrive_time, service_time, depart_order, depart_time, replication, everything(), -name, -finished, -replication)
  
  d_order <- result_all %>% select(depart_order)
  
  result <- result_all %>%
    arrange(arrive_time) %>%
    mutate(depart_order = d_order[[1]]) %>%
    mutate(arrive_order = 1:length(depart_order))
  
  fn <- str_c("C:/Users/op/Google Drive/02 - School/AFIT/Research/QueueInference.jl/QueueOutv18/out",i,".csv")
  
  write_csv(result,fn)
}
