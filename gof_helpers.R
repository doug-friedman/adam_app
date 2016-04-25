# Extracted R2 and RMSE functions directly from caret library because of shinyapps.io size limit
R2 = function (pred, obs, formula = "corr", na.rm = FALSE) 
	{
		n <- sum(complete.cases(pred))
		switch(formula, corr = cor(obs, pred, use = ifelse(na.rm, 
			"complete.obs", "everything"))^2, traditional = 1 - (sum((obs - 
			pred)^2, na.rm = na.rm)/((n - 1) * var(obs, na.rm = na.rm))))
	}
RMSE = function (pred, obs, na.rm = FALSE) { sqrt(mean((pred - obs)^2, na.rm = na.rm)) }

MSWD = function(pred, obs){ sum((obs - pred)^2 / obs)/(length(obs)-1) }