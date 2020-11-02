library(mvtnorm)
library(lqmm)
library(tidyverse)

# set up empty state df
df <- read.csv("data/full_state_list.csv", stringsAsFactors = F)
ec <- read.csv("data/electoral-college.csv", stringsAsFactors = F)
names(df) <- c("State","abbrev","code","value")

# sample results

df$region <- tolower(df$State)
#df[50,4] <- "Biden"
#df[51,4] <- "Trump"

df$value <- factor(df$value, levels = c("Undecided", "Biden", "Trump"))

choices <- c("Biden","Trump")


# functions first ---------------------------------------------------------
logit <- function(x) log(x/(1-x))

inv_logit <- function(x) 1/(1 + exp(-x))

draw_samples <- function(biden_states = NULL, trump_states = NULL, states = NULL, 
                         upper_biden = NULL, lower_biden = NULL, print_acceptance = FALSE, target_nsim = 1000){
  sim <- matrix(NA, nr = 1, nc = length(mu))
  n <- 0
  while(nrow(sim) < target_nsim){
    # randomly sample from the posterior distribution and reject when constraints are not met
    n <- n + 1
    proposals <- inv_logit(rmvnorm(1e5, mu, Sigma, method = "svd")) # "DC" is pretty much uncorrelated
    colnames(proposals) <- names(mu)
    if (!is.null(biden_states)) proposals[which(proposals[,biden_states] < .5)] <- NA
    if (!is.null(  trump_states)) proposals[which(proposals[,  trump_states] > .5)] <- NA
    if (!is.null(        states)){
      for (s in states){
        proposals[which(proposals[, s] > upper_biden[s] | 
                          proposals[, s] < lower_biden[s])] <- NA
      }
    }
    reject <- apply(proposals, 1, function(x) any(is.na(x)))
    sim <- rbind(sim, proposals[!reject,])
    if (nrow(sim) < target_nsim & nrow(sim)/(nrow(proposals)*n) < 1-99.99/100){
      stop(paste("rmvnorm() is working hard... but more than 99.99% of the samples are rejected; you should relax some contraints.", sep = ""))
    }
  }
  return(list("matrix" = sim[-1,], "acceptance_rate" = nrow(sim)/(nrow(proposals)*n)))
}


update_prob <- function(biden_states = NULL, trump_states = NULL, biden_scores_list = NULL, target_nsim = 1000, show_all_states = TRUE){
  states <- names(biden_scores_list)
  lower_biden <- sapply(biden_scores_list, function(x) x[1]/100)
  upper_biden <- sapply(biden_scores_list, function(x) x[2]/100)
  sim <- draw_samples(biden_states = biden_states, trump_states = trump_states, states = states, 
                      upper_biden = upper_biden, lower_biden = lower_biden, 
                      target_nsim = target_nsim)
  ev_dist <- (sim[["matrix"]] > .5) %*% ev
  state_win <- colMeans(sim[["matrix"]] > .5)
  p <- mean(ev_dist >= 270)
  sd <- sqrt(p*(1-p)/length(ev_dist))
  
  my_results <- list( "biden_prob" = p , "state_prob" = state_win)
  return(my_results)
  # if (show_all_states){
  #   cat("Pr(biden wins) by state, in %:\n")
  #   print(t(round(100*state_win)))
  #   cat("--------\n")
  # }
  # cat(paste("Pr(biden wins the electoral college) = ", round(100*p), "%\n[nsim = ", length(ev_dist), "; se = ", round(sd*100,1), "%]", sep = ""))
  # if (show_all_states) cat("\n--------\n")
  # cat("Uncalled states:\n")
  # cat(colnames(sim$matrix)[!(colnames(sim$matrix) %in%
  #                              c(biden_states,trump_states))])
}


# read data ---------------------------------------------------------------
# get simulations
sim_forecast <- read_csv('https://cdn.economistdatateam.com/us-2020-forecast/data/president/electoral_college_simulations.csv') 

# check initial parameters
# nrow(sim_forecast)
# mean(sim_forecast$dem_ev > 269)

# select relevant columns and make the data frae into a matrix
sim_forecast <- sim_forecast %>% select(4:ncol(.))
sim_forecast <- list(sim_forecast,sim_forecast,sim_forecast,sim_forecast,sim_forecast) %>% bind_rows  %>% as.matrix


# this bit is really hacky
# it make the simulations a little less correlated by add a bunch of random noise
# this helps our live model react to really implausible events that could happen on election night
# but will have the result of pushing the initial pre-results forecasts back toward 50-50
sim_forecast <- sim_forecast + 
  rnorm(nrow(sim_forecast), 0, 0.01) +  # national error component
  replicate(ncol(sim_forecast),rnorm(nrow(sim_forecast),0,0.02)) # state

sim_forecast <- ifelse(sim_forecast <= 0, 0.0001, sim_forecast)
sim_forecast <- ifelse(sim_forecast >= 1, 0.99999, sim_forecast)

sim_forecast <- as_tibble(sim_forecast)

# now, get electoral votes in each state 
# and make sure they're in the right order
#ev_state <- read_csv('data/state_evs.csv')$ev
#names(ev_state) <- read_csv('data/state_evs.csv')$state
ev_state <- read_csv('https://raw.githubusercontent.com/TheEconomist/us-potus-model/master/data/2012.csv')$ev
names(ev_state) <- read_csv('https://raw.githubusercontent.com/TheEconomist/us-potus-model/master/data/2012.csv')$state
ev <- ev_state[colnames(sim_forecast)] 

# check that the EVs and state forecasts add up to the right amounts
sim_evs <- apply(sim_forecast,
                 1,
                 function(x){
                   sum((x > 0.5 )* ev)
                 })

# hist(sim_evs,breaks=538) 
# median(sim_evs)
# mean(sim_evs)
# mean(sim_evs > 269)
# enframe(prop.table(table(sim_evs)),'dem_ev','pct') %>% arrange(desc(pct))


# adding ME1 and ME2, NE1 NE2 to sim_forecast matrix and ev vector
# we do this by adding the average district-level two-party dem presidential vote, relative
# to the state-level dem two-party vote, back to to our state-level forecast

# first, split up EVs
ev["ME"] <- 2
ev["NE"] <- 2
ev <- c(ev, "ME1" = 1, "ME2" = 1, "NE1" = 1, "NE2" = 1, "NE3" = 1)
# sum(ev)

# create simulations for ME and NE districts
me_ne_leans <- politicaldata::pres_results_by_cd %>% filter(year >= 2012, state_abb %in% c("ME","NE")) %>%
  select(-other) %>% 
  rename(state = state_abb) %>%
  group_by(year,state) %>%
  mutate(sum_pct = dem + rep,
         total_votes = total_votes * sum_pct,
         dem = dem /sum_pct,
         rep = rep/sum_pct) %>%
  mutate(dem_vote_state = sum(dem * total_votes) / sum(total_votes)) %>%
  mutate(dem_cd_lean = dem - dem_vote_state) %>%
  group_by(state,district) %>%
  summarise(dem_cd_lean = weighted.mean(dem_cd_lean, c(0.3,0.7)))

# bind new simulation columns for the congressional districts, based on the above
sim_forecast <- bind_cols(
  sim_forecast, 
  tibble(
    ME1 = sim_forecast %>% pull(ME) +
      rnorm(nrow(sim_forecast), 
            me_ne_leans[me_ne_leans$state == "ME" & me_ne_leans$district == 1,]$dem_cd_lean, 
            .0075),
    ME2 = sim_forecast %>% pull(ME) +
      rnorm(nrow(sim_forecast), 
            me_ne_leans[me_ne_leans$state == "ME" & me_ne_leans$district == 2,]$dem_cd_lean, 
            .0075),
    
    NE1 = sim_forecast %>% pull(NE) +
      rnorm(nrow(sim_forecast), 
            me_ne_leans[me_ne_leans$state == "NE" & me_ne_leans$district == 1,]$dem_cd_lean, 
            .0075),
    NE2 = sim_forecast %>% pull(NE) +
      rnorm(nrow(sim_forecast), 
            me_ne_leans[me_ne_leans$state == "NE" & me_ne_leans$district == 2,]$dem_cd_lean, 
            .0075),
    NE3 = sim_forecast %>% pull(NE) +
      rnorm(nrow(sim_forecast), 
            me_ne_leans[me_ne_leans$state == "NE" & me_ne_leans$district == 3,]$dem_cd_lean, 
            .0075) 
  )
)

# sim_forecast


sim_evs <- apply(sim_forecast,
                 1,
                 function(x){
                   sum((x > 0.5 )* ev)
                 })

# colMeans(sim_forecast > 0.5)

# hist(sim_evs,breaks=538)
# median(sim_evs)
# mean(sim_evs)
# mean(sim_evs > 269)
# enframe(prop.table(table(sim_evs)),'dem_ev','pct') %>% arrange(desc(pct))


# final data wrangling -- this stuff getes passed into the update_prob function
# mainly we just want to make sure everything is in the right order with the right names
ev <- ev[colnames(sim_forecast)]

Sigma <- cov(logit(sim_forecast)) 
# Sigma

mu <- colMeans(logit(sim_forecast))
names(mu) <- colnames(sim_forecast)

# run ---------------------------------------------------------------------
# for example...
# raw prob, no constraints
update_prob(biden_states = NULL,
            trump_states = NULL,
            biden_scores_list = NULL)
