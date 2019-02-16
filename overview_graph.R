#####################################################################################################################################################################
#####################################################################################################################################################################
#
#  ______   ____    ____  _______ .______     ____    ____  __   ___________    __    ____      _______ .______          ___      .______    __    __  
# /  __  \  \   \  /   / |   ____||   _  \    \   \  /   / |  | |   ____\   \  /  \  /   /     /  _____||   _  \        /   \     |   _  \  |  |  |  | 
#|  |  |  |  \   \/   /  |  |__   |  |_)  |    \   \/   /  |  | |  |__   \   \/    \/   /     |  |  __  |  |_)  |      /  ^  \    |  |_)  | |  |__|  | 
#|  |  |  |   \      /   |   __|  |      /      \      /   |  | |   __|   \            /      |  | |_ | |      /      /  /_\  \   |   ___/  |   __   | 
#|  `--'  |    \    /    |  |____ |  |\  \----.  \    /    |  | |  |____   \    /\    /       |  |__| | |  |\  \----./  _____  \  |  |      |  |  |  | 
# \______/      \__/     |_______|| _| `._____|   \__/     |__| |_______|   \__/  \__/         \______| | _| `._____/__/     \__\ | _|      |__|  |__| 
#                                                                                                    
#####################################################################################################################################################################
#####################################################################################################################################################################

library(shiny)
library(cowplot)
library(plyr)

source("complete_plot.R")

overview_graph_UI = function(id){
	ns = NS(id)
	fluidPage(
		# Iput data file
		column(width = 12, class = "well", align = "center",
			fileInput(ns('data'),'Choose file'),
			actionButton(inputId = ns("simulated_data"), "Example simulated dataset"),
			actionButton(inputId = ns("real_data"), "Example real-life dataset")
			),
		# Checkboxes for curve selection
		column(width = 12, class = "well", align = "center",
			   checkboxGroupInput(inputId = ns("custom_selection"), choices = NULL, selected = NULL, label = NULL)
			   ),
		# Select buttons
		column(width = 12,  align = "center", class = "well",
			   actionButton(inputId = ns("select_all"), "Select all"),
			   actionButton(inputId = ns("deselect_all"), "Deselect all")
			   ),
		# Curve manipulation:
		column(width=12, align="center", class = "well",
			   radioButtons(inputId=ns("show_hide_curve_manipulation"), choices=list("Hide","Show"), label = "Show or hide curve manipulation settings", inline=TRUE)
			   ),
		# Data manipulation
		conditionalPanel(
			condition = paste0("input['" , ns("show_hide_curve_manipulation"), "'] == 'Show'"),
			# Data manipulation
			column(width = 3, align="center",
				# Y axis adjustment
				h4("Y-Axis Adjustment"),
				textInput(inputId = ns("y_adjust_value"),"Set to value:",value=1),
				actionButton(ns("y_adjust"),"Y-axis adjustment"),
				fluidPage(
			   	column(width=6, align="center",
					textInput(inputId = ns("plot_ymax"), "Change default ymax",value="default")
					),
				column(width=6, align="center",
			   		textInput(inputId = ns("plot_ymin"), "Change default ymin",value="default")
					),
			   	numericInput(inputId = ns("dead_volume"), "Set dead volume [µl]", 55)
				)
				),
			column(width = 3, align="center",
				# Drift correction
				h4("Drift Correction"),
				radioButtons(inputId=ns("mode_drift_correction"),choices=list("Single","Dual"), label = "Choose mode of drift correction", inline=TRUE),
				# Show if single drift correction was selected
				conditionalPanel(
					condition = paste0("input['", ns("mode_drift_correction"),"'] == 'Single'"),
						actionButton(ns("drift_correction_single"),"Use selected area for drift correction")
					),
				conditionalPanel(
					condition = paste0("input['", ns("mode_drift_correction"),"'] == 'Dual'"),
						actionButton(ns("drift_correction_dual_one"),"Use selected area for drift correction one"),
						actionButton(ns("drift_correction_dual_two"),"Use selected area for drift correction two"),
						actionButton(ns("dual_drift_range"),"Select cross-fade area")
						),
				actionButton(ns("reset"),"Reset to original dataset")
			),
			column(width=3, align="center",
				# Download current Dataset with corrected data
				h4("Download Dataset"),
				downloadButton(outputId = ns("download_dataset"), label = "Download Dataset")
				),
			column(width=3, align="center",
				# Save overview graph menu
			   h4("Save Graph"),
			   radioButtons(inputId = ns("size_change"), choices = list("default","custom"), label="Choose plot size", inline=TRUE),
			   conditionalPanel(
					condition = paste0("input['", ns("size_change"),"'] == 'custom'"),
						textInput(inputId=ns("plot_width"),"Plot width [cm]:", value=75),
						textInput(inputId=ns("plot_height")," Plot height [cm]:", value=""),
						textInput(inputId=ns("plot_dpi"),"Plot dpi (max 2000):", value=300)
					),

			   radioButtons(inputId = ns("save_format"), choices = list("pdf", "png", "jpeg","tiff","ps","bmp"), label = "Select the file type", inline=TRUE ),
			   downloadButton(outputId = ns("download_complete_graph"), label = "Download plot")
			)
			),
		# Complete graph ( overall graph) panel
		column(width = 12, class = "well",
				h4("Overview Graph - Select region"),
				plotOutput(outputId = ns("plot"), brush = brushOpts(id=ns("plot_brush")), click = ns("plot_click")),
				sliderInput(inputId = ns("time_range"), "Time range:",min=0, max = 1, value=c(0,1),step = 1)
#				textOutput(outputId=ns("data_index"))
				)
#		column(width = 12, class = "well",
#			tableOutput(outputId=ns("test_table")),
#			verbatimTextOutput(outputId=ns("test"))
#			)
	 )
}

overview_graph = function(input, output, session){

	test_zone = reactiveValues()

	##########
	# Generate log_file object
	##########

	log_file = reactiveValues()

	##########
	# Generate variable to save all overview graphs and the name of all uploaded datasets and all used datasets
	##########
	save_results = reactiveValues()
	# Values for summery sheet
	save_results$overview_graphs = list()
	save_results$all_used_datasets = list()
	save_results$fit_regions = data.frame()
	# Values for fits sheet
	save_results$fit_graphs = list()
	save_results$fit_results = list()
	save_results$pg1 = list()
	save_results$pg2 = list()
	save_results$pg3 = list()

	
	##########
	# Global Document Functions
	##########

	# SCORE DATA READIN
	score_data = function(path){
		#Load excel worksheet
		wb = loadWorkbook(path)
		df = read.xlsx(wb, sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, check.names=TRUE)
		
		# Getting log file data
		if(length(sheets(wb))>1){
			log_file_raw = read.xlsx(wb, sheet = 2, startRow = 1, colNames = TRUE,rowNames = FALSE, check.names=TRUE)
			
			# Save results into log_file object
			log_file_all = log_file_raw
			log_file_relevant = log_file_raw[,c(1,3,4)]
		}
		else{
			log_file_all = NULL
			log_file_relevant = NULL
		}

		df_names = names(df)
		#Update the Checkboxes for data selection
		updateCheckboxGroupInput(session,inputId = "custom_selection",choices = df_names[2:ncol(df)], selected = df_names[2:ncol(df)] , inline = TRUE)
		#Update data range of time range slider
		updateSliderInput(session,"time_range",
			min = min(df[,1]),
			max = max(df[,1]),
			value = c(min(df[,1]), max(df[,1]))
			)
		df = melt(df, id.vars=names(df)[1])
		names(df) = c("Time","variable","value")
		return_data = list(a=df,b=log_file_all, c=log_file_relevant)
		return(return_data)
	}


	# BIACORE DATA READIN
	biacore_data = function(path){
		# Read txt file of exported biacore data
		data = read.table(path,header=T,sep="\t")
		#Account for  comma dot problem
		if(class(data[,1]) != "numeric"){
			data = read.table(path, header = T, sep="\t", dec=",")
		}
		# select all uneven columns to get x values
		x = data[,c(TRUE,FALSE)]
		# select all even columns to get y values
		y = data[,!c(TRUE,FALSE)]
		# stack all x values to one column
		x = stack(x)
		# stack all y values to one column
		y = stack(y)
		# prepare final dataframe
		df = data.frame(Time = as.numeric(x[,1]), variable = y[,2], value = as.numeric(y[,1]))
		
		df = na.omit(df)

		df_names = levels(df[,"variable"])
		#Update the Checkboxes for data selection
		updateCheckboxGroupInput(session,inputId = "custom_selection",choices = df_names, selected = df_names, inline = TRUE)
		#Update data range of time range slider
		updateSliderInput(session,"time_range",
			min = min(df[,1]),
			max = max(df[,1]),
			value = c(min, max(df[,1]))
			)

		return(df)
	}



	#OCTET DATA READIN
	octet_data = function(path){
		# read preudo xls files (They acutally are tab delimeted files)
		#Account for  comma dot problem
		data = read.table(path,header=T,sep="\t")
		if(class(data[,1]) != "numeric"){
			data = read.table(path, header = T, sep="\t", dec=",")
		}
		#delete columns containing only NAs
		data = data[,colSums(is.na(data)) != nrow(data)]
		# select all uneven columns to get x values
		x = data[,c(TRUE,FALSE)]
		# select all even columns to get y values
		y = data[,!c(TRUE,FALSE)]
		# stack all x values to one column
		x = stack(x)
		# stack all y values to one column
		y = stack(y)
		# prepare final dataframe
		df = data.frame(Time = as.numeric(x[,1]), variable = x[,2], value = as.numeric(y[,1]))
		
		df = na.omit(df)

		df_names = levels(df[,"variable"])
		#Update the Checkboxes for data selection
		updateCheckboxGroupInput(session,inputId = "custom_selection",choices = df_names, selected = df_names, inline = TRUE)
		#Update data range of time range slider
		updateSliderInput(session,"time_range",
			min = min(df[,1]),
			max = max(df[,1]),
			value = c(min, max(df[,1]))
			)

		return(df)
	}



	##########
	# Get and parce data frame given by user
	##########
	data_frame_readin = reactiveValues()
	
	observeEvent(input$data,{
		#Input Form
        file = input$data
		# if condition to provide showing an error if no input is choosen
        if (is.null(file)){
            return(NULL)
        }
        path = file$datapath
		
		# Use the correct function depending on the type of data uploaded
		filename = toupper(file$name)
		
		if(grepl("BIACORE",filename)){
			data_frame_readin$raw_data = biacore_data(path)
		}
		else if(grepl("OCTET",filename) | grepl("BIOFORTE",filename)){
			data_frame_readin$raw_data = octet_data(path)
		}
		else{
			temp = score_data(path)
			data_frame_readin$raw_data = temp$a
			data_frame_readin$log_file_all = temp$b
			data_frame_readin$log_file_relevant = temp$c
		}
		data_frame_readin$filename = filename
		data_hist$df = NULL
	})



	##########
	# Load simulated_data example
	##########

	observeEvent(input$simulated_data, {
		temp = score_data("simulated_data.xlsx")
		data_frame_readin$raw_data = temp$a
		data_frame_readin$log_file_all = temp$b
		data_frame_readin$log_file_relevant = temp$c
		data_frame_readin$filename = "EXAMPLE SIMULATED DATA"
		data_hist$df = NULL
	})


	##########
	# Load Real Data Excample
	##########

	observeEvent(input$real_data, {
		temp = score_data("real_data.xlsx")
		data_frame_readin$raw_data = temp$a
		data_frame_readin$log_file_all = temp$b
		data_frame_readin$log_file_relevant = temp$c
		data_frame_readin$filename = "EXAMPLE REAL LIFE DATA"
		data_hist$df = NULL
	})
			

	##########
	# Select All button
	##########

	observeEvent(input$select_all, {
		df = data_frame_readin$raw_data
		df_names = levels(df[,"variable"])
		updateCheckboxGroupInput(session,inputId="custom_selection",choices = df_names, selected = df_names , inline = TRUE)
	})
	##########
	# Deselect all button
	##########

	observeEvent(input$deselect_all, {
		df = data_frame_readin$raw_data 
		df_names = levels(df[,"variable"])
		updateCheckboxGroupInput(session,inputId="custom_selection",choices = df_names, inline = TRUE)
	})

	##########
	# Y-Adjustment
	##########

	observeEvent(input$y_adjust,{
		mdf = brushedPoints(data_frame(), input$plot_brush)
		data_manip$y_adjust_brush = input$plot_brush
		df = droplevels(mdf)
		# Find the minimum x Values of selected curves as they will be set to the given value
		data_manip$y_adjust = dlply(df, .(variable), function(df) {
			as.numeric(input$y_adjust_value) - mean(df[,3])
				})
	})


	##########
	# Index output
	##########

#	output$data_index = renderText({
#		if(!is.null(data_frame_readin$raw_data) & !is.null(input$plot_click)){
#			filename = data_frame_readin$filename
#			if(!grepl("BIACORE",filename) & !(grepl("OCTET",filename) | grepl("BIOFORTE",filename))){
#				paste0("Data Index (click into overview graph):")
#				temp = dlply(data_frame_readin$raw_data ,.(variable),function(df) index=which.min(abs(df[,1] - as.numeric(input$plot_click$x)))) 
#				paste0("X-Value selected: ", floor(input$plot_click$x) ,"  Index:" , min(unlist(temp)))
#			}
#		}
#	})


	##########
	# Chek if drift correction was set
	##########
	
	data_manip = reactiveValues()
	data_manip$drift_single = NULL
	data_manip$drift_dual_one = NULL
	data_manip$drift_dual_two = NULL
	data_manip$Tmin = NULL
	data_manip$Tmax = NULL
	data_manip$y_adjust = NULL
	data_manip$y_adjust_brush = NULL

	##########
	# Single drift correction
	##########
	observeEvent(input$drift_correction_single, {
		# only excecute when one or both of the single drift corrections are not set
		if(is.null(data_manip$drift_dual_one) | is.null(data_manip$drift_dual_two)){
			if(!is.null(input$plot_brush)){
				if(is.null(data_manip$drift_single)){
					mdf = brushedPoints(data_frame(), input$plot_brush)
					df = droplevels(mdf)
					# Do a linear regresssion for every variable in the dataframe mdf
					data_manip$drift_single = dlply(df, .(variable), function(df) {
						fit = lm(value~df[,1], data=df)
						return(fit$coef)
					})
				}
			}
		}
	})

	##########
	# Dual drift correction one
	##########
	observeEvent(input$drift_correction_dual_one, {
		# Only execute when single drift is not set
		if(is.null(data_manip$drift_single)){
			if(!is.null(input$plot_brush)){
				if(is.null(data_manip$drift_dual_one)){
					mdf = brushedPoints(data_frame(), input$plot_brush)
					df = droplevels(mdf)
					# Do a linear regresssion for every variable in the dataframe mdf
					data_manip$drift_dual_one = dlply(df, .(variable), function(df) {
						fit = lm(value~df[,1], data=df)
						return(fit$coef)
					})
				}
			}
		}
	})
	
	##########
	# Dual drift correction two
	##########
	observeEvent(input$drift_correction_dual_two, {
		# Only execute when single drift is not set
		if(is.null(data_manip$drift_single)){
			if(!is.null(input$plot_brush)){
				if(is.null(data_manip$drift_dual_two)){
					mdf = brushedPoints(data_frame(), input$plot_brush)
					df = droplevels(mdf)
					# Do a linear regresssion for every variable in the dataframe mdf
					data_manip$drift_dual_two = dlply(df, .(variable), function(df) {
						fit = lm(value~df[,1], data=df)
						return(fit$coef)
					})
				}
			}
		}
	})

	##########
	# Define cross-fade area for dual drift correction
	##########
	observeEvent(input$dual_drift_range, {
		brush = input$plot_brush
		if(!is.null(brush)){
			data_manip$Tmin = brush$xmin
			data_manip$Tmax = brush$xmax
		}
	})


	##########
	# Reset Drift correction and y adjustment
	##########

	observeEvent(input$reset, {
		data_manip$drift_single = NULL
		data_manip$drift_dual_one = NULL
		data_manip$dirft_dual_two = NULL
		data_manip$Tmin = NULL
		data_manip$Tmax = NULL
		data_hist$df = data_frame_readin$raw_data
	})


	##########
	# Define a Dataframe that will store the manipulated dataset
	##########

	data_hist = reactiveValues()
	data_hist$df = NULL


	##########
	# Perform Y-Adjustment
	##########

	observeEvent(data_manip$y_adjust,{
		# If no manipulated df exits:
		if(!is.null(data_frame_readin$raw_data) & is.null(data_hist$df)){
			df = data_frame_readin$raw_data
		}
		# If there already is a manipulated df:
		else if(!is.null(data_hist$df)){
			df = data_hist$df
		}
		###
		# If Y-Adjustment was chosen:
		###
		if(!is.null(data_manip$y_adjust)){
			# for every variable in y-adjust
			for( i in 1:length(names(data_manip$y_adjust))){
				name = names(data_manip$y_adjust)[i]
				df_selection = subset(df,df$variable %in% name)
				y = NULL
				y = df_selection[,3] + data_manip$y_adjust[[i]]
				# edit the old dataset
				df[which(df$variable == name),]$value = y
			}
		}
		# Write or overrite existing manipulated dataframe
			data_hist$df = df		
	})

	##########
	# Perform single drift correction
	##########

	observeEvent(data_manip$drift_single,{
		# If no manipulated df exits:
		if(!is.null(data_frame_readin$raw_data) & is.null(data_hist$df)){
			df = data_frame_readin$raw_data
		}
		# If there already is a manipulated df:
		else if(!is.null(data_hist$df)){
			df = data_hist$df
		}
		# Single drift correction
		if(input$mode_drift_correction == "Single"){
			# only exec if there is data in drift single
			if(!is.null(data_manip$drift_single)){
				# For every dataset in drift_single
				for(i in 1:length(names(data_manip$drift_single))){
					name = names(data_manip$drift_single)[i]
					df_selection = subset(df, df$variable %in% name)
					# calculate y - m1*t for every datapoint
					y = df_selection$value - (data_manip$drift_single[[i]][2] * df_selection[,1])

					# edit the old dataset
					df[which(df$variable == name),]$value = y
				}
			# Write or overrite existing manipulated dataframe
			data_hist$df = df
			}
		}
	})

	##########
	# Perform dual drift correction
	##########

	observeEvent({
		data_manip$drift_dual_one
		data_manip$drift_dual_two
		data_manip$Tmin
		data_manip$Tmax
		input$mode_drift_correction 
	},{
		# If no manipulated df exits:
		if(!is.null(data_frame_readin$raw_data) & is.null(data_hist$df)){
			df = data_frame_readin$raw_data
		}
		# If there already is a manipulated df:
		else if(!is.null(data_hist$df)){
			df = data_hist$df
		}
		# Dual drift correction
		if(input$mode_drift_correction == "Dual"){
			# Only execute if both drift corrections are set
			if(!is.null(data_manip$drift_dual_one) & !is.null(data_manip$drift_dual_two) & !is.null(data_manip$Tmin) & !is.null(data_manip$Tmax)){
				# only execute if the same number of curves have been selected for drift correction
				if(length(names(data_manip$drift_dual_one)) == length(names(data_manip$drift_dual_two))){
					# Calculate correction for every curve
					for(i in 1:length(names(data_manip$drift_dual_one))){
						name = names(data_manip$drift_dual_one)[i]
						df_selection = subset(df, df$variable %in% name)
						y = NULL
						m1 = data_manip$drift_dual_one[[i]][2]
						m2 = data_manip$drift_dual_two[[i]][2]
						To = data_manip$Tmin
						Tmax = data_manip$Tmax
						# Calculate new y values for t<To
						temp = df_selection[which(df_selection[,1] <= To),]
						y = temp$value - m1*temp[,1]
						# calculate new y values for t>To and t<T
						temp = df_selection[which(df_selection[,1] > To & df_selection[,1] < Tmax),]
						values = temp$value - ((m1*temp[,1]) + (((temp[,1]-To)/(Tmax-To))*(m2*temp[,1] - m1*temp[,1])))
						#append the calculated values to the existing y values
						y = c(y,values)
						# calculate new y values for t>T
						temp = df_selection[which(df_selection[,1] >= Tmax),]
						values = temp$value - m2*temp[,1]
						#append the calculated values to the existing y values
						y = c(y,values)

						#edit the old dataset
						df[which(df$variable == name),]$value = y
					}

					# Write or overrite existing manipulated dataframe
					data_hist$df = df
				}
			}
		}
	})

	

	##########
	# defining the working data frame
	##########
	data_frame = reactive({
		
		if(is.null(data_hist$df)){
			# takes the data frame which was selected by user
			df = data_frame_readin$raw_data
		}
		else{
		df = data_hist$df
		}
		if(!is.null(data_frame_readin$raw_data)){
			# Update the time range according to slider input
			time_min = as.numeric(input$time_range[1])
			time_max = as.numeric(input$time_range[2])
	  
			df = df[which(df[[1]] >time_min & df[[1]] <time_max),]	
			# Generate the working data frame from selection
			selection = input$custom_selection
			df = subset(df, df[[2]] %in% selection)
		}
	})
	
	
	##########
	# Execute and Output First Plot
	##########

	output$plot = renderPlot({
		# Execute the function above which generated the overview plot
		module_output$complete_plot =  complete_plot(data_frame(), data_frame_readin$log_file_relevant,input$plot_ymax,input$plot_ymin, input$dead_volume)
		module_output$complete_plot
	})



	##########
	# Download current dataset
	##########

	output$download_dataset = downloadHandler(
			filename = function(){
				filename = data_frame_readin$filename
					filename = data_frame_readin$filename
					paste(filename, ".txt", sep=".")
			},
			content = function(file){
				filename = data_frame_readin$filename
					temp = dcast(data_frame(), data_frame()[,1] ~ data_frame()[,2])
					names(temp)[1] = "Time"
					write.csv(temp, file = file, row.names=FALSE)
			}
		)

	##########
	#Download first plot
	##########

	output$download_complete_graph = downloadHandler(
	filename = function(){
		paste("overview_graph", input$save_format, sep=".")
	},
	content = function(file){
	if(input$size_change == "default"){
			ggsave(file, plot = module_output$complete_plot, device = input$save_format, width=75, units=c("cm"))
		}
		else if(as.numeric(input$plot_dpi) < 2001){
			ggsave(file, plot = module_output$complete_plot, device = input$save_format, width=as.numeric(input$plot_width), height=as.numeric(input$plot_height), units=c("cm"),dpi=as.numeric(input$plot_dpi))
		}
	}
	)

#	output$test_table = renderTable({
#	})
#
#	output$test = renderPrint({
#		print()
#	})

	##########
	# Generate and Return all output
	##########
	
	module_output = reactiveValues()

	reactive_module_output = reactive({
		
		data_frame = data_frame()
		log_file_relevant = log_file$relevant
		plot_brush = input$plot_brush
		list("data_frame" = data_frame, "log_file_relevant" = log_file$relevant, "plot_brush" = plot_brush, "complete_plot" = module_output$complete_plot, "data_frame_readin" = data_frame_readin$raw_data , "time_range" = input$time_range , "plot_ymax" = input$plot_ymax, "plot_ymin" = input$plot_ymin , "data" = input$data , "dead_volume" = input$dead_volume, "data_hist_df" = data_hist$df, "custom_selection" = input$custom_selection, "data_frame_readin_filename" = data_frame_readin$filename)
	})

	return(reactive_module_output)
	

}
