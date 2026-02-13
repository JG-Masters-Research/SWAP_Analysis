swap = read.csv("C:/Users/granj/Downloads/analysis.csv")

model <- glm(cbind(sgcn, total_species - sgcn) ~ 1,
             family = binomial,
             data = swap)
baseline_p <- plogis(coef(model))

swap$expected_sgcn <- round(swap$total_species * baseline_p)
swap$residual <- round(residuals(model, type = "pearson"))

outliers <- swap[abs(swap$residual) >2,]
