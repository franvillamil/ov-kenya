# Political strongholds and organized violence in Kenya
# Francisco Villamil
# November 2014
# -----------------------------------------------------------
# APPENDIX: ANALYSIS OF ORGANIZED VIOLENCE INCIDENCE

# (R code file no. 4 -- independent)

# NOTE (!): ASSIGN WORKING DIRECTORY TO THE REPLICATION FOLDER
setwd(...)
library(party)
library(ggplot2)

# Load data file
data = read.csv("data.csv", header = TRUE)


# Random forest analysis - variable importance
# ----------------------------------------------

# RF & variable importance
set.seed(233629)
formula = as.formula(factor(events.org) ~ change.pov + log(population) + ethnic.het + kikuyu + pol.comp + odinga)
imp = varimp(cforest(formula, data = data, control = cforest_unbiased(mtry = 3, ntree = 1000)))

# Data frame for plotting
impdf = data.frame(varimp = as.numeric(imp),
	var = c("Change in\npoverty", "Log Population", "Ethnic heterog.", "Kikuyu", "Political\ncompetition", "ODM share"))

# Plotting
var.imp = ggplot(impdf, aes(x = varimp, y = reorder(var, varimp))) +
	geom_point() +
	theme_bw() +
	xlab("\nMean decrease in accuracy") +
	ylab("")
ggsave(var.imp, file = "var-imp.pdf", height = 3, width = 6)


# Logit analysis (same models as in NB analysis)
# ----------------------------------------------

# Formulae
kik = formula(events.org ~ kikuyu + I(kikuyu^2) + change.pov + odinga + ethnic.het + log(population))
kik2 = formula(events.org ~ kikuyu * ethnic.het + change.pov + odinga + ethnic.het + log(population))
polco = formula(events.org ~ pol.comp * ethnic.het + change.pov + log(population))

# Models & summaries
lr.kik = glm(formula = kik, data = data, family = "binomial")
lr.kik2 = glm(formula = kik2, data = data, family = "binomial")
lr.polco = glm(formula = polco, data = data, family = "binomial")

summary(lr.kik)
summary(lr.kik2)
summary(lr.polco)

# Variable effects in data frames
coefs.lkik = data.frame(
	variables = c("Kikuyu", "Kikuyu^2", "Change in\npoverty", "ODM share", "Ethnic\nheterog.", "Log Population"),
	coef = exp(summary(lr.kik)$coefficients[2:7, 1]),
	UL = exp(summary(lr.kik)$coefficients[2:7, 1] + (1.96 * summary(lr.kik)$coefficients[2:7, 2])),
	LL = exp(summary(lr.kik)$coefficients[2:7, 1] - (1.96 * summary(lr.kik)$coefficients[2:7, 2]))
	)
coefs.lkik2 = data.frame(
	variables = c("Kikuyu", "Ethnic\nheterog.", "Change in\npoverty", "ODM share", "Log Population", "Kik:Eth het"),
	coef = exp(summary(lr.kik2)$coefficients[2:7, 1]),
	UL = exp(summary(lr.kik2)$coefficients[2:7, 1] + (1.96 * summary(lr.kik2)$coefficients[2:7, 2])),
	LL = exp(summary(lr.kik2)$coefficients[2:7, 1] - (1.96 * summary(lr.kik2)$coefficients[2:7, 2]))
	)
coefs.lpolco = data.frame(
	variables = c("Political\ncompetition", "Ethnic\nheterog.", "Change in\npoverty", "Log Population", "Pol com:Eth het"),
	coef = exp(summary(lr.polco)$coefficients[2:6, 1]),
	UL = exp(summary(lr.polco)$coefficients[2:6, 1] + (1.96 * summary(lr.polco)$coefficients[2:6, 2])),
	LL = exp(summary(lr.polco)$coefficients[2:6, 1] - (1.96 * summary(lr.polco)$coefficients[2:6, 2]))
	)

# Plotting
plot.lrkik = ggplot(coefs.lkik, aes(x = coef, y = reorder(variables, coef))) + 
	geom_point() + 
	geom_errorbarh(aes(x = coef, xmin = LL, xmax = UL, height = 0)) +
	xlab("\nEffect in Pr(org. violence)") + ylab("") +
	geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
	theme_bw() + ggtitle("AIC = 196.79\n")
plot.lrkik2 = ggplot(coefs.lkik2, aes(x = coef, y = reorder(variables, coef))) + 
	geom_point() + 
	geom_errorbarh(aes(x = coef, xmin = LL, xmax = UL, height = 0)) +
	xlab("\nEffect in Pr(org. violence)") + ylab("") +
	geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
	theme_bw() + ggtitle("AIC = 198.70\n")
plot.lrpolco = ggplot(coefs.lpolco, aes(x = coef, y = reorder(variables, coef))) + 
	geom_point() + 
	geom_errorbarh(aes(x = coef, xmin = LL, xmax = UL, height = 0)) +
	xlab("\nEffect in Pr(org. violence)") + ylab("") +
	geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
	theme_bw() + ggtitle("AIC = 209.66\n")
ggsave(plot.lrkik, file = "lr-kik.pdf", width = 5, height = 5)
ggsave(plot.lrkik2, file = "lr-kik2.pdf", width = 5, height = 5)
ggsave(plot.lrpolco, file = "lr-polco.pdf", width = 5, height = 5)
