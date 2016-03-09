library(SDMTools)
library(DT)
library(highcharter)
library(boot)
source("std_error_helpers.R")

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
	
	datatable(dt.df, options = list(dom = 't'))   
	
  })
  
  
  output$dist_chart = renderHighchart({
	data = clean_data()
	
    hchart(data[,1], color="#173656", name = "Data")
	  
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