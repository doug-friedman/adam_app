library(DT)
library(highcharter)

shinyUI(fluidPage(
     # CSS was being difficult so I manually appended it to the head
     tags$head(tags$link(rel="stylesheet", type="text/css", href="whoi_style.css")),

     div(style="padding: 1px 0px; width: '100%'",
         titlePanel(div(id="title", style="font-size:25px",
             img(src = "whoi_logo.gif", height="55px"),
			 "Adam's WHOI Application"),
			 windowTitle = "Adam's App"
			)
       ),
  navbarPage("Modules",
      
	  tabPanel("Standard Error", sidebarLayout(
	  
		sidebarPanel(
		  helpText("This is an application designed to aid in performing regular statistical calculations for doctoral students at the Woods Hole Oceanographic Institute. You can upload a csv below and the application will automatically calculate several summary statistics and produce a plot of the data."),

		  br(),
		  helpText("If you are unsure what format your csv should take, you can download and edit a template csv using the link below."),
		  downloadButton("downloadTemplate", "Download Template"),
		  br(), br(),
		  
		  checkboxInput(
			 inputId = "template_se",
			 label = "My file matches the template.",
			 value = TRUE
		   ),
		  br(),
		  
		  conditionalPanel(
		    condition = "!input.template_se",
			  h4("Please specify the following about the file..."),
			  br(),
			  
			  numericInput(
				inputId = "value_col",
				label = "Which column contains the data?",
				value = 1),
				
			  numericInput(
				inputId = "weight_col",
				label = "Which column contains the error?",
				value = 2)	  
		  ),
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
		 "Source code available on Github", href="https://github.com/doug-friedman/adam_app"))
	  
  ),
  tabPanel("Regression", sidebarLayout(  
	sidebarPanel(
	  helpText("This is an application designed to aid in performing regular statistical calculations for doctoral students at the Woods Hole Oceanographic Institute. You can upload a csv below and the application will automatically simulate regression for the associated point ranges."),
	  
	  br(),
	  helpText("If you are unsure what format your csv should take, you can download and edit a template csv using the link below."),
	  downloadButton("downloadTemplate_lm", "Download Template"),
	  br(), br(),
	  
	 checkboxInput(
		 inputId = "template_reg",
		 label = "My file matches the template.",
		 value = TRUE
		   ),
	  br(),
	  
	  conditionalPanel(
	    condition = "!input.template_reg",
		  h4("Please specify the following about the file..."),
		  br(),
		  
		  numericInput(
			inputId = "x_col",
			label = "Which column contains the independent variable or X?",
			value = 1),
			
		  numericInput(
			inputId = "xr_col",
			label = "Which column contains the RANGE OF the indepndent variable or X?",
			value = 2),	
			
		  numericInput(
			inputId = "y_col",
			label = "Which column contains the dependent variable or Y?",
			value = 3)
		),
	  numericInput(
		inputId = "nsim",
		label = "How many simulations should be performed?",
		value = 1000),	

	  checkboxInput(
			 inputId = "force_origin",
			 label = "Force the regression through the origin (0,0).",
			 value = TRUE
		   ), br(),	   
      radioButtons(
	     	 inputId = "ci_method",
			 label = "Choose confidence interval to display.",
			 choices = c("Regression Coefficients"=1, "Fitted Values"=2),
			 selected = 1,
			 inline=TRUE
		   ),
		   
	  br(),
	  
	   fileInput(
			inputId = "datafile_lm", 
			label = "Choose file to upload"
			)
  
	  
	),
	mainPanel(
	  h3("Regression Plot"), highchartOutput("dist_reg"), br(),
	  h3("Simulation Results"), DT::dataTableOutput("table_reg"), br(),
	  helpText("*Note: R-Squared values CANNOT be used to compare no-intercept and intercept models.", br(), "See", a(href="https://stats.stackexchange.com/questions/26176/removal-of-statistically-significant-intercept-term-increases-r2-in-linear-mo", "https://stats.stackexchange.com/questions/26176/removal-of-statistically-significant-intercept-term-increases-r2-in-linear-mo"), "for details."),
	  br(), br()
	)
	))
  )
))