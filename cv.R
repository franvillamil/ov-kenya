# Political strongholds and organized violence in Kenya
# Francisco Villamil
# November 2014
# -----------------------------------------------------------
# LEAVE-ONE-OUT CROSS VALIDATION OF THE MODELS

# (R code file no. 2 - comes from analysis.R)


library(caret)
set.seed(121110)

# Cross validation function
cv.pred = function(x, formula) {
	train = subset(cv.data, k != x)
	test = subset(cv.data, k == x)
	test$pred = predict(
					glm.nb(formula, data = train),
					newdata = test,
					type = "response")
	outcome = subset(test, select = c("constituency", "fat", "pred", "k"))
	return(outcome)
	} 

# CV folds - LOOCV
cv.data = data
cv.data$k = createFolds(cv.data$fat, k = 49, list = FALSE)

# Cross-validate the models and calculate predicted values
models = list(kik, kik2, polco)
cvpred.df = data.frame(obs = cv.data$fat)
for (i in models){
	df = data.frame(constituency = c(),
						obs = c(),
						pred = c(),
						k = c())
	for (j in 1:max(cv.data$k)){
		df = rbind(df, cv.pred(j, i))
		}
	cvpred.df = cbind(cvpred.df, df$pred)
	}
names(cvpred.df)[2:4] = c("kik", "kik2", "polco")

# RMSE & Pearson's R
rmse.cv = data.frame(
	kik = round(sqrt((sum((cvpred.df$obs - cvpred.df$kik)^2))/length(cvpred.df$obs)),3),
	kik2 = round(sqrt((sum((cvpred.df$obs - cvpred.df$kik2)^2))/length(cvpred.df$obs)),3),
	polco = round(sqrt((sum((cvpred.df$obs - cvpred.df$polco)^2))/length(cvpred.df$obs)),3)
	)
cor.test(cvpred.df$obs, cvpred.df$kik)
cor.test(cvpred.df$obs, cvpred.df$kik2)
cor.test(cvpred.df$obs, cvpred.df$polco)


