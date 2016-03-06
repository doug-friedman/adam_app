library(SDMTools)
library(DT)
library(highcharter)
library(boot)
source("std_error_helpers.R")

shinyServer(function(input, output, session) {

  clean_data = reactive({
    req(input$datafile); req(input$skip_val)
	datafile = input$datafile

	validate(
	  need(substr(datafile$name, nchar(datafile$name)-3, nchar(datafile$name)) == ".csv", "Sorry, but the uploaded file could not be read. Please try using the template csv file.")
	)

	clean_data = read.csv(datafile$datapath, header=input$header, skip=input$skip_val)
	clean_data = na.omit(clean_data)
	
	return(clean_data)
  })
  
  output$table_stats = DT::renderDataTable({
    req(input$value_col); req(input$weight_col); req(input$stderr_calc)
	
    data = clean_data()
	
    validate(
	   need(class(data[,input$value_col]) %in% c("numeric", "integer"), "Sorry, but the data detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data."),
	   need(class(data[,input$weight_col]) %in% c("numeric", "integer"), "Sorry, but the weights detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data.")
	)
    
    if(input$stderr_calc=="formula"){
	  wse = weighted.var.se(data[,input$value_col], data[,input$weight_col])
	} else {
	  wse = boot.wse(data[,input$value_col], data[,input$weight_col])
	} 


	dt.df = data.frame(
	  Unweighted = c(mean(data[,input$value_col]),
	               sd(data[,input$value_col]),
				   var(data[,input$value_col]),
				   sd(data[,input$value_col])/sqrt(nrow(data))),
	  Weighted = c(wt.mean(data[,input$value_col], data[,input$weight_col]),
	               wt.sd(data[,input$value_col], data[,input$weight_col]),
				   wt.var(data[,input$value_col], data[,input$weight_col]),
				   wse)
	)
	
	rownames(dt.df) = c("Mean", 
	                    "Standard Deviation",
						"Variance",
						"Standard Error")
	
	datatable(dt.df, options = list(dom = 't'))   
	
  })
  
  
  output$dist_chart = renderHighchart({
    req(input$value_col); req(input$weight_col)
	
	data = clean_data()
	
    validate(
	   need(class(data[,input$value_col]) %in% c("numeric", "integer"), "Sorry, but the value data detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data."),
	   need(class(data[,input$weight_col]) %in% c("numeric", "integer"), "Sorry, but the weights detected contained non-numeric information. Please try using the toggles on the left to focus on the numeric data.")
	)
  
    hchart(data[,input$value_col], color="#173656", name = "Data")
	  
  })
  
  output$downloadTemplate = downloadHandler(
   filename = function() {
      paste("Sample", "Template.csv", sep='')
    },
    content = function(file) {
      write.csv(data.frame(value = c(1:10), weights = c(10:1)), file, row.names=F)
    }
  )

})