# Political strongholds and organized violence in Kenya
# Francisco Villamil
# November 2014
# -----------------------------------------------------------
# PLOTS (figures 3-6)

# (R code file no. 3 -- comes from cv.R)


library(ggplot2)

# Histogram (figure 3)
# --------------------

hist = ggplot(data, aes(x = fat, y = ..count..)) + geom_histogram(stat = "bin") +
	theme_bw() + xlab("\nNumber of fatalities") + ylab("Frequency")
hist.log = ggplot(data, aes(x = fat, y = ..count..)) + geom_histogram(stat = "bin") +
	theme_bw() + xlab("\nNumber of fatalities (log scale)") + ylab("Frequency") + scale_x_log10()
ggsave(plot = hist, file = "hist.pdf", height = 5, width = 5)
ggsave(plot = hist.log, file = "hist-log.pdf", height = 5, width = 5)


# Plotting models (figure 4)
# --------------------------

plot.mkik = ggplot(coefs.kik, aes(x = coef, y = reorder(variables, coef))) + 
	geom_point() + 
	geom_errorbarh(aes(x = coef, xmin = LL, xmax = UL, height = 0)) +
	xlab("\nEffect in fatalities count") + ylab("") +
	geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
	theme_bw() + ggtitle("AIC = 360.52\n")
plot.mkik2 = ggplot(coefs.kik2, aes(x = coef, y = reorder(variables, coef))) + 
	geom_point() + 
	geom_errorbarh(aes(x = coef, xmin = LL, xmax = UL, height = 0)) +
	xlab("\nEffect in fatalities count") + ylab("") +
	geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
	theme_bw() + ggtitle("AIC = 360.10\n")
plot.mpolco = ggplot(coefs.polco, aes(x = coef, y = reorder(variables, coef))) + 
	geom_point() + 
	geom_errorbarh(aes(x = coef, xmin = LL, xmax = UL, height = 0)) +
	xlab("\nEffect in fatalities count") + ylab("") +
	geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
	theme_bw() + ggtitle("AIC = 353.20\n")
ggsave(plot.mkik, file = "eff-kik.pdf", width = 5, height = 5)
ggsave(plot.mkik2, file = "eff-kik2.pdf", width = 5, height = 5)
ggsave(plot.mpolco, file = "eff-polco.pdf", width = 5, height = 5)


# Plotting effect of political competition & ethnic heterogeneity interaction (figure 5)
# --------------------------------------------------------------------------------------

ppplot = ggplot(preds, aes(x = ethnic.het, y = pred)) +
	geom_line() + geom_ribbon(aes(ymin = predLL, ymax = predUL), alpha = .15) +
	facet_wrap(~pol.comp) + scale_x_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
	theme_bw() + xlab("Ethnic heterogeneity") + ylab("Predicted fatalities") + scale_y_continuous(limits = c(0,150))
ggsave(ppplot, file = "pp.pdf", width = 10, height = 3.5)


# Plotting CV predictions against observed values (figure 6)
# ----------------------------------------------------------

plot.kik = ggplot(cvpred.df, aes(y = obs, x = kik)) + geom_point() +
	ylab("Observed") + xlab("Predicted") + ggtitle("RMSE = 18.97 / R = 0.10 (p = 0.50)\n") +
	#geom_text(aes(x = 60, y = 87, label = "RMSE = 18.97"), size = 3) +
	geom_abline(intercept = 0, slope = 1, color = "black", size = 0.2) +
	scale_x_continuous(limits=c(0,80), breaks = c(0,25,50,75)) +
	theme_bw()
plot.kik2 = ggplot(cvpred.df, aes(y = obs, x = kik2)) + geom_point() +
	ylab("Observed") + xlab("Predicted") + ggtitle("RMSE = 18.81 / R = 0.09 (p = 0.55)\n") +
	#geom_text(aes(x = 60, y = 87, label = "RMSE = 18.81"), size = 3) +
	geom_abline(intercept = 0, slope = 1, color = "black", size = 0.2) +
	scale_x_continuous(limits=c(0,80), breaks = c(0,25,50,75)) +
	theme_bw()
plot.polco = ggplot(cvpred.df, aes(y = obs, x = polco)) + geom_point() +
	ylab("Observed") + xlab("Predicted") + ggtitle("RMSE = 17.48 / R = 0.33 (p = 0.02)\n") +
	#geom_text(aes(x = 60, y = 87, label = "RMSE = 17.48"), size = 3) +
	geom_abline(intercept = 0, slope = 1, color = "black", size = 0.2) +
	scale_x_continuous(limits=c(0,80), breaks = c(0,25,50,75)) +
	theme_bw()
ggsave(plot.kik, file = "kik-po.pdf", height = 5, width = 5)
ggsave(plot.kik2, file = "kik2-po.pdf", height = 5, width = 5)
ggsave(plot.polco, file = "polco-po.pdf", height = 5, width = 5)
