library(SDMTools)
library(DT)
library(highcharter)
library(boot)
library(RcppArmadillo)
library(dplyr, warn.conflicts=F)
source("std_error_helpers.R")
source("gof_helpers.R")

shinyServer(function(input, output, session) {

  clean_data = reactive({
    req(input$datafile); req(input$skip_val); req(input$value_col); req(input$weight_col); 
	datafile = input$datafile

	validate(
	  need(substr(datafile$name, nchar(datafile$name)-3, nchar(datafile$name)) == ".csv", "Sorry, but the uploaded file could not be read. Please try using the template csv file.")
	)

	clean_data = read.csv(datafile$datapath, header=input$header, skip=input$skip_val)
	clean_data = na.omit(clean_data[,c(input$value_col, input$weight_col)])
	
	validate(
	  need(class(clean_data[,1]) %in% c("numeric", "integer"), "Sorry, but the data detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data."),
	  need(class(clean_data[,2]) %in% c("numeric", "integer"), "Sorry, but the weights detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data.")
	)
	
	if(input$weight_calc == "InvSqrt"){ clean_data[,2] = 1/sqrt(clean_data[,2]) }
	
	return(clean_data)
  })
  
  
  clean_data_lm = reactive({
    req(input$datafile_lm); req(input$x_col); req(input$xr_col); req(input$y_col); 
	datafile = input$datafile_lm

	validate(
	  need(substr(datafile$name, nchar(datafile$name)-3, nchar(datafile$name)) == ".csv", "Sorry, but the uploaded file could not be read. Please try using the template csv file.")
	)

	clean_data = read.csv(datafile$datapath, header=input$header, skip=input$skip_val)
	clean_data = na.omit(clean_data[,c(input$x_col, input$xr_col, input$y_col)])
	
	validate(
	  need(class(clean_data[,1]) %in% c("numeric", "integer"), "Sorry, but the X data detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data."),
	  need(class(clean_data[,2]) %in% c("numeric", "integer"), "Sorry, but the X range data detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data."),
	  need(class(clean_data[,3]) %in% c("numeric", "integer"), "Sorry, but the Y data detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data.")
	)
	
	return(reg_sim(clean_data, input$nsim, input$force_origin))
	#return(clean_data)
  })  
  
  output$table_stats = DT::renderDataTable({
    req(input$stderr_calc)
	
    data = clean_data()

    if(input$stderr_calc=="formula"){
	  wse = weighted.var.se(data[,1], data[,2])
	} else {
	  wse = boot.wse(data[,1], data[,2])
	} 

	dt.df = data.frame(
	  Unweighted = c(mean(data[,1]),
	               sd(data[,1]),
				   var(data[,1]),
				   sd(data[,1])/sqrt(nrow(data))),
	  Weighted = c(wt.mean(data[,1], data[,2]),
	               wt.sd(data[,1], data[,2]),
				   wt.var(data[,1], data[,2]),
				   wse)
	)
	
	rownames(dt.df) = c("Mean", 
	                    "Standard Deviation",
						"Variance",
						"Standard Error")
	
	datatable(dt.df, options = list(dom = 't'), list(list(className = 'dt-center', targets = c(1:4))))   
	
  })

  output$table_reg = DT::renderDataTable({

    data = clean_data_lm()$res_sum
	data = data %>% t %>% data.frame
	data[,2] = paste0("(", data[,2], ", ", data[,3], ")")
	data[,3] = NULL
	names(data) = c("Mean", "95% CI")
	rownames(data) = c("Slope", "Intercept", "R-Squared", "Root Mean Square Error", "Mean Square Weighted Deviation")

    datatable(data, options = list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = c(1:2)))))
  })
    
  
  output$dist_chart = renderHighchart({
	data = clean_data()
	
    hchart(data[,1], color="#173656", name = "Data")
	  
  })
  
  reg_sim = function(data, nsim, force_origin){
    progress = Progress$new()
	progress$set(message = "Starting simulation", value = 0)
	on.exit(progress$close())
    names(data) = c("x","x_range","y")

	progress$set(message = "Simulating data", value = 0.3)
	
	gen_unif = function(vec, nsim){ runif(nsim, vec[1]-vec[2], vec[1] + vec[2])  }
	set.seed(7) # KEEPS IT RANDOM BUT REPRODUCIBLE
	sim_data = apply(data[,c("x", "x_range")], 1, gen_unif, nsim=nsim)

	progress$set(message = "Fitting regressions", value = 0.5)
	get_flm = function(x,y, origin=TRUE){  
      this.lm = fastLm(cbind(x,!origin), y) 
	  return(c(unname(coef(this.lm)), this.lm$fitted))
	}

	sim.res = apply(sim_data, 1, get_flm, y=data$y, force_origin) 

	progress$set(message = "Summarizing simulation", value = 0.7)
	sim_sum = function(simmed){ return(round(c(mean(simmed), quantile(simmed, c(0.025, .975), names=F)),3)) }
	coefs.sim = sim.res[c(1:2),] %>% apply(1, sim_sum)
	fits = sim.res[-c(1:2),]

	fit.rsq = apply(fits, 2, R2, obs=data$y)
    fit.rmse = apply(fits, 2, RMSE, obs=data$y)	
	fit.mswd = apply(fits, 2, MSWD, obs=data$y)

	# Get three number summaries
	slopes.sim = coefs.sim[,1]
	ints.sim = coefs.sim[,2]
	
	rsq.sim = sim_sum(fit.rsq)
	rmse.sim = sim_sum(fit.rmse)
	mswd.sim = sim_sum(fit.mswd)

	res = list(
	  res_data = cbind(data, 
	                   coef_mean= round(slopes.sim[1]*data$x + ints.sim[1],3),
	                   coef_lci = round(slopes.sim[2]*data$x + ints.sim[2],3),
					   coef_uci = round(slopes.sim[3]*data$x + ints.sim[3],3),
					   fit_mean = round(apply(fits, 1, mean),3),
					   fit_lci = round(apply(fits, 1, quantile, .025),3),
					   fit_uci = round(apply(fits, 1, quantile, .975),3)
			     ),
	  res_sum = data.frame(slopes.sim, ints.sim, rsq.sim, rmse.sim, mswd.sim)
	)
	progress$set(message = "Sending data to charts and tables", value = 0.9)
    return(res)	
  }  
  
  output$dist_reg = renderHighchart({
	req(input$ci_method)
	dataset = clean_data_lm()
	dataset = dataset$res_data

	#print(dataset)

	# Each dataframe has to be laundered through list.parse3 
	# with the columns assigned to the JS parameters
	# except for the arearange chart which takes list.parse2
	hc_data = list.parse3(data.frame(x=dataset$x, y=dataset$y))
	hc_data2 = list.parse3(data.frame(x=dataset$x, y=dataset$coef_mean))
	
	lower = if(input$ci_method==2){ dataset$fit_lci } else { dataset$coef_lci }
	upper = if(input$ci_method==2){ dataset$fit_uci } else { dataset$coef_uci }
	hc_data3 = data.frame(x=dataset$x, lower, upper) %>% arrange(x) %>% list.parse2


	highchart() %>%
	  hc_add_series(data=hc_data, type="scatter", name="Original Data", color="#000") %>%
	  hc_add_series(data=hc_data2, type="line", name="Regression Line", marker=list(enabled=FALSE), color="#214e6c") %>%
	  hc_add_series(data=hc_data3, type="arearange", name="Bootstrapped 95% CI", fillOpacity=0.3, zIndex=1, dashStyle = "ShortDashDot") %>%
	  hc_plotOptions(animation=FALSE)
  })
  
  output$downloadTemplate = downloadHandler(
   filename = function() {
      paste("Sample", "Template.csv", sep='')
    },
    content = function(file) {
      write.csv(data.frame(value = c(1:10), weights = c(10:1)), file, row.names=F)
    }
  )

  output$downloadTemplate_lm = downloadHandler(
   filename = function() {
      paste("Sample", "Template_Regression.csv", sep='')
    },
    content = function(file) {
      write.csv(data.frame(x = faithful[1:10,1], x_range = iris[1:10,4], y=faithful[1:10,2]), file, row.names=F)
    }
  )
  
})