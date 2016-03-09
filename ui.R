library(DT)
library(highcharter)

shinyUI(fluidPage(
  
  titlePanel(div(id="title",
             img(src = "whoi_logo.gif", height="75px"),
			 "Adam's WHOI Application"),
			 windowTitle = "Adam's App"
			),

  sidebarLayout(
  
    sidebarPanel(
	  helpText("This is an application designed to aid in performing regular statistical calculations for doctoral students at the Woods Hole Oceanographic Institute. You can upload a csv below and the application will automatically calculate several summary statistics and produce a plot of the data."),

	  br(),
	  helpText("If you are unsure what format your csv should take, you can download and edit a template csv using the link below."),
	  downloadButton("downloadTemplate", "Download Template"),
	  br(),
	  
	  br(),
	  h4("Please specify the following about the file..."),
	  br(),
	  
	  numericInput(
	    inputId = "value_col",
        label = "Which column contains the data?",
		value = 1),
		
	  numericInput(
	    inputId = "weight_col",
        label = "Which column contains the error?",
		value = 2),		  
	  
	  radioButtons(
	     inputId = "weight_calc",
		 label = "How should the weights be calculated from the error?",
		 choices = c("No Calculation"="None", "Inverse of the Square Root (1/SQRT)"="InvSqrt"),
		 selected = "None"
      ),
	  
	  br(),
	  
	  checkboxInput(
	     inputId = "header",
		 label = "My file has column names.",
		 value = TRUE
       ),
	  
	  conditionalPanel(
	    condition = "!input.header",
	    numericInput(
	      inputId = "skip_val",
          label = "What is the first row with data?",
		  value = 0)	
	  ),
	 
	  
	   
	  br(), 	  
	  
      fileInput(
	    inputId = "datafile", 
	    label = "Choose file to upload"
	    )
	),
	
    mainPanel(
	  h3("Distribution Plot"), highchartOutput("dist_chart", height="350px"),
	  h3("Summary Statistics"), DT::dataTableOutput("table_stats"), br(), br(),
	  radioButtons(
	     inputId = "stderr_calc",
		 label = "Choose a method to calculate standard error for the weighted mean.",
		 choices = c("Boostrap"="bootstrap", "Formulaic"="formula"),
		 selected = "bootstrap",
		 inline=TRUE
       ),
	  helpText("The calculation choices for standard error calculation for the weighted mean are taken from this 1995 paper by Donald Gatz and Luther Smith:", a("http://www.cs.tufts.edu/~nr/cs257/archive/donald-gatz/weighted-standard-error.pdf", href="http://www.cs.tufts.edu/~nr/cs257/archive/donald-gatz/weighted-standard-error.pdf"))
    ),
  ),
  h6(a(img(src="github_logo.png", height="20px"), 
	 "Source code available on Github", href="https://github.com/doug-friedman/adam_app")),
  includeCSS("whoi.css")
))