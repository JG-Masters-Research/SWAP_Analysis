library(brms)
swap = read.csv("C:/Users/granj/Downloads/analysis.csv")
plot(participants~funds, xlab = "Funding from SWG", ylab = "Participants", pch = 19, col = "purple", data = swap)
hist(swap$participants, xlab = "Number of Participants")

swap$funds_hunthou <- swap$funds/100000
participant_model <- brm(participants~funds_hunthou, family = "poisson", data = swap)
summary(participant_model)
fixef(participant_model)

participant_model_nb <- brm(participants~funds_hunthou, family = "negbinomial", data = swap)
summary(participant_model_nb)
fixef(participant_model_nb)

loo_pois <- loo(participant_model)
loo_nb <- loo(participant_model_nb)
loo_compare(loo_pois, loo_nb)
#elpd_diff of Poisson model is -307.7, indicating overdispersed data; using negbinomial 

pos_samples = data.frame(participant_model_nb)
head(pos_samples)

hist(pos_samples$b_funds_hunthou, breaks = 100, col = "green", xlab = "Plausible values of effect of funding on participation")
plot(density(pos_samples$b_funds_hunthou))
polygon(density(pos_samples$b_funds_hunthou), col = "green", main = "Plausible values for slope parameter")

length(which(pos_samples$b_funds_hunthou>0))
length(which(pos_samples$b_funds_hunthou>0))/length(pos_samples$b_funds_hunthou)

#Posterior Probability of Direction is 36.18%, indicating no clear directional effect
#No strong evidence of any effect of participants on number of SGCN. Too much noise in SGCN data; most likely independent.

plot(conditional_effects(participant_model_nb), points = TRUE)

#At this point, possibly clump states into regions?
