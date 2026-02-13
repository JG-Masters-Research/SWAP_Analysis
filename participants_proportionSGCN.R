library(brms)
swap = read.csv("C:/Users/granj/Downloads/analysis.csv")
plot(sgcn~participants, xlab = "Participants", ylab = "SGCN", pch = 19, col = "purple", data = swap)
swap$participants_10 <- swap$participants / 10
my_model <- brm(sgcn|trials(total_species)~participants_10, family = "binomial", data = swap)

summary(my_model)
fixef(my_model)
hist(swap$participants_10)

pos_samples = data.frame(my_model)
head(pos_samples)
hist(pos_samples$b_participants_10, breaks = 100, col = "green", xlab = "Plausible values of effect of participation on SGCN")

plot(density(pos_samples$b_participants_10))
polygon(density(pos_samples$b_participants_10), col = "green", main = "Plausible values for slope parameter")

length(which(pos_samples$b_participants_10>0))
length(which(pos_samples$b_participants_10>0))/length(pos_samples$b_participants_10)
#Posterior Probability of Direction is about 85%; likely positive, but not high certainty

mean(pos_samples$b_participants_10)
sd(pos_samples$b_participants_10)

quantile(pos_samples$b_participants_10, c(0.1, 0.9))

plot(sgcn/total_species~participants_10, xlab = "Number of Participants in SWAP Development", ylab = "Proportion of SGCN", pch = 19, col = "purple4", data = swap)

median_slope <- median(pos_samples$b_participants_10)
median_intercept <- median(pos_samples$b_Intercept)

curve(plogis(median_intercept+median_slope*x), add=TRUE, col = "orange", lwd = 5)
for(i in 1:length(pos_samples$b_participants_10)){
  curve(plogis(pos_samples$b_Intercept[i]+pos_samples$b_participants_10[i]*x), add = TRUE, lwd = 0.5, col = rgb(0.8, 0, 0,1, alpha = 0.4))
}

curve(plogis(median_intercept + median_slope * x), add = TRUE, col = "pink", lwd = 5)
