# Political strongholds and organized violence in Kenya
# Francisco Villamil
# November 2014
# -----------------------------------------------------------
# ANALYSIS: NEGATIVE BINOMIAL MODELS & PREDICTED EFFECTS

# (R code file no. 1)


# NOTE (!): ASSIGN WORKING DIRECTORY TO THE REPLICATION FOLDER
library(MASS)
setwd(...)

# Read data file and limit sample to those constituencies with at least one reported event
data = read.csv("data.csv", header = TRUE)
data = subset(data, events.org == 1)


# Negative binomial models
# ------------------------

# Formulae
kik = formula(fat ~ kikuyu + I(kikuyu^2) + change.pov + odinga + ethnic.het + offset(log(population)))
kik2 = formula(fat ~ kikuyu * ethnic.het + change.pov + odinga + ethnic.het + offset(log(population)))
polco = formula(fat ~ pol.comp * ethnic.het + change.pov + offset(log(population)))

# Models & summaries
m.kik = glm.nb(formula = kik, data = data)
m.kik2 = glm.nb(formula = kik2, data = data)
m.polco = glm.nb(formula = polco, data = data)

summary(m.kik)
summary(m.kik2)
summary(m.polco)

# Variable effects in data frames
coefs.kik = data.frame(
	variables = c("Kikuyu", "Kikuyu^2", "Change in\npoverty", "ODM share", "Ethnic\nheterog."),
	coef = exp(summary(m.kik)$coefficients[2:6, 1]),
	UL = exp(summary(m.kik)$coefficients[2:6, 1] + (1.96 * summary(m.kik)$coefficients[2:6, 2])),
	LL = exp(summary(m.kik)$coefficients[2:6, 1] - (1.96 * summary(m.kik)$coefficients[2:6, 2]))
	)
coefs.kik2 = data.frame(
	variables = c("Kikuyu", "Ethnic\nheterog.", "Change in\npoverty", "ODM share", "Kik:Eth het"),
	coef = exp(summary(m.kik2)$coefficients[2:6, 1]),
	UL = exp(summary(m.kik2)$coefficients[2:6, 1] + (1.96 * summary(m.kik2)$coefficients[2:6, 2])),
	LL = exp(summary(m.kik2)$coefficients[2:6, 1] - (1.96 * summary(m.kik2)$coefficients[2:6, 2]))
	)
coefs.polco = data.frame(
	variables = c("Political\ncompetition", "Ethnic\nheterog.", "Change in\npoverty", "Pol com:Eth het"),
	coef = exp(summary(m.polco)$coefficients[2:5, 1]),
	UL = exp(summary(m.polco)$coefficients[2:5, 1] + (1.96 * summary(m.polco)$coefficients[2:5, 2])),
	LL = exp(summary(m.polco)$coefficients[2:5, 1] - (1.96 * summary(m.polco)$coefficients[2:5, 2]))
	)

# Political competition & ethnic heterogeneity interaction
# --------------------------------------------------------

# New data
newdf = data.frame(
	population = rep(mean(data$population), 41*3),
	change.pov = rep(mean(data$change.pov), 41*3),
	ethnic.het = rep(seq(-1, 1, 0.05),3),
	pol.comp = rep(c(-1, 0, 1), each = 41))

# Predicted effects
pred.pclow = predict(m.polco, newdata = subset(newdf, pol.comp == -1), type = "response", se.fit = TRUE)
pred.pcmean = predict(m.polco, newdata = subset(newdf, pol.comp == 0), type = "response", se.fit = TRUE)
pred.pchigh = predict(m.polco, newdata = subset(newdf, pol.comp == 1), type = "response", se.fit = TRUE)

# Effects and CIs
preds = data.frame(
	ethnic.het = rep(seq(-1, 1, 0.05),3),
	pol.comp = rep(c("Political competition = -1", "Political competition = 0", "Political competition = 1"), each = 41),
	pred = c(pred.pclow$fit, pred.pcmean$fit, pred.pchigh$fit),
	predUL = c(pred.pclow$fit + 1.96 * pred.pclow$se.fit,
			pred.pcmean$fit + 1.96 * pred.pcmean$se.fit,
			pred.pchigh$fit + 1.96 * pred.pchigh$se.fit),
	predLL = c(pred.pclow$fit - 1.96 * pred.pclow$se.fit,
			pred.pcmean$fit - 1.96 * pred.pcmean$se.fit,
			pred.pchigh$fit - 1.96 * pred.pchigh$se.fit)
	)
rm(pred.pclow, pred.pcmean, pred.pchigh, newdf)
