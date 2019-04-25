#####################################################################################################################################################################
#####################################################################################################################################################################
#________    ______     ______   .___  ___.         ___      .__   __.  _______      _______  __  .___________.
#|       /   /  __  \   /  __  \  |   \/   |        /   \     |  \ |  | |       \    |   ____||  | |           |
#`---/  /   |  |  |  | |  |  |  | |  \  /  |       /  ^  \    |   \|  | |  .--.  |   |  |__   |  | `---|  |----`
#   /  /    |  |  |  | |  |  |  | |  |\/|  |      /  /_\  \   |  . `  | |  |  |  |   |   __|  |  |     |  |     
#  /  /----.|  `--'  | |  `--'  | |  |  |  |     /  _____  \  |  |\   | |  '--'  |   |  |     |  |     |  |     
# /________| \______/   \______/  |__|  |__|    /__/     \__\ |__| \__| |_______/    |__|     |__|     |__|    
#
#####################################################################################################################################################################
#####################################################################################################################################################################                                    

##################################################################################################################################################################### 
# UI
##################################################################################################################################################################### 

zoom_and_fit_UI = function(id){
	ns = NS(id)
	fluidPage(
	# Zoomed Graph panel
	column(width = 6, class = "well",
		   h4("Zoomed Graph - Select region for fit"),
		   plotOutput(outputId = ns("plot_zoomed"), brush = brushOpts(id=ns("analyse_brush"))),
		   textOutput(outputId=ns("chosen_fitting_range"))
		   ),
	# Fit panel
	column(width = 6, class = "well",
		   h4("Fit"),
		   plotOutput(outputId=ns("plot_analysis"))
		   ),
	# Checkbox to chow or hide
	column(width = 12, align = "center", class = "well",
		   radioButtons(inputId = ns("show_hide_curve_analysis"), choices = list("Hide","Show"), selected="Hide", label = "Assisting analysis graphs", inline=TRUE)
		   ),
	conditionalPanel(
		condition = paste0("input['",ns("show_hide_curve_analysis"),"'] == 'Show'"),
		# Fild to insert binning
		column(width = 12, align="center",
			  numericInput(inputId=ns("curve_smooth"),value = 1, label="Number of average points for curve smoothing:", width=300)
			  ),
		# Curve progression analysis 1 panel
		column(width = 4, #class = "well",
			   h4("Deviation Plot"),
			   plotOutput(outputId=ns("plot_progression_1"))
			   ),
		# Curve progression analysis 2 panel
		column(width = 4, #class = "well",
			   h4("Self-Exponential Plot"),
			   plotOutput(outputId=ns("plot_progression_2"))
			   ),
		# Residual plot panel
		column(width = 4, #class = "well",
			   h4("Residual Plot"),
			   plotOutput(outputId=ns("residual_plot"))
			   )
		),
		# Fit statistic table
		column(width = 12,
			   h4("Fit Statistics"),
			   tableOutput(ns("fit_results"))
			   )
		# test output
#		column(width=12,class="well",
#			   verbatimTextOutput(outputId=ns("test"))
#			   )
	)

}

##################################################################################################################################################################### 
# SERVER
##################################################################################################################################################################### 

zoom_and_fit = function(input, output, session, output_overview_graph){


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
	test_zone = reactiveValues()


	##########
	# Plot of selection from first plot ( zoomed plot)
	##########
	output$plot_zoomed = renderPlot({
		brush = output_overview_graph()$plot_brush
		if(!is.null(brush)){
			# getting the selected points using the brushedPoints method
			mdf = brushedPoints(output_overview_graph()$data_frame, brush)
			mdf = droplevels(mdf)
			p = ggplot(data=mdf, aes(x=Time, y=value, group = variable, colour = variable)) + geom_line() + background_grid(major = "xy", minor = "xy")
			# Do not show legend for more then 20 data curves
			if(length(levels(mdf[,2]))>20){
				p = p + theme(legend.position="none")
			}
			p

		}
	})

	##########
	# Plot showing selection from zoomed plot as well as the fitted curves
	##########
    output$plot_analysis = renderPlot({
		if(!is.null(input$analyse_brush)){
			shiny::validate(
                need(model$results!=0,"Unable to calculate fit. Please chance area!")
            )
			# Getting the data points used in the model (see model variable)
			mdf = model$source
			# Getting the predicted values by the model for drawing the fitting curves
			plot_model = model$predict
			# Plotting the data points
			p = ggplot(data=mdf, aes(x=Time, y=value, group = variable, colour = variable)) +
			geom_point(alpha=0.3, size=0.3)+
			# Plotting the fitted lines
			geom_line(data=plot_model, aes(x=Time, y=value, group=variable, colour=variable),size=0.8) + background_grid(major = "xy", minor = "xy") 
			# Do not show legend for more then 20 data curves
			if(length(levels(mdf[,2]))>20){
				p = p + theme(legend.position="none")
			}
			# store the plot temporally for the final results file
			save_results$fit_graphs_temp = p
			p
		}
	})

	##########
	# Showing the chosen time range from brush
	##########

    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 3), "   xmax=", round(e$xmax, 3), 
             "   ymin=", round(e$ymin, 3), "   ymax=", round(e$ymax, 3))
    }

	output$chosen_fitting_range = renderText({
		if(!is.null(input$analyse_brush)){
			x=xy_range_str(input$analyse_brush)
			save_results$temp_chosen_fitting_range = x
			x
		}
	})

	##########
	# function to calculate a running avarage
	##########
	running_average = function(x_list,n){
		as.vector(filter(x_list,rep(1/n,n),side=2))
	}
	
	##########
	# Curve progression Plot 1
	##########
	
	curve_progression_1 = function(input_analyse_brush,input_plot_brush,source_data_frame){
		if(!is.null(input_analyse_brush) & !is.null(output_overview_graph()$custom_selection)){
				# Getting the selected data points from the zoomed in plot
				# Dataset containing the chosen datapoints for the fit
				fit_data_frame = brushedPoints(source_data_frame, input_analyse_brush)
				fit_data_frame = droplevels(fit_data_frame)
				if(nrow(fit_data_frame) != 0){
					# Dataset containing all datapoints form the zoomed in region
					data_frame = brushedPoints(source_data_frame, input_plot_brush)
					data_frame = droplevels(data_frame)
					names(data_frame) = c("Time","variable","value")
					
					# spit temp 2 according to selected curves. chosen_range is now an object containing a dataset for every curve variable
					chosen_range = split(fit_data_frame,fit_data_frame[,2])
					
					# For the dataframe containing all dataspots
					# Calculate (y+1) - (y) for every selected column of data except the time column
					new_data_frame = data.frame()
					for(name in 1:length(levels(fit_data_frame$variable))){
						temp=data.frame()
						data_frame_subset = droplevels(subset(data_frame,data_frame$variable == levels(fit_data_frame$variable)[name]))
						for(entry in 2:nrow(data_frame_subset)){
							temp[entry-1,1] = data_frame_subset$value[entry] - data_frame_subset$value[(entry-1)]
						}

						#calculating the running average for temp ( the calculated y values)

						temp[,1] = running_average(temp[,1],input$curve_smooth)

						
						#replacing the initial data with the calculated differences
						if(nrow(new_data_frame) == 0){
							new_data_frame = data.frame(data_frame_subset$Time[2:nrow(data_frame_subset)], data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(new_data_frame) = c("Time", "variable", "value")
						}
						else{
							x = data.frame(data_frame_subset$Time[2:nrow(data_frame_subset)], data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(x) = c("Time","variable","value")
							new_data_frame = rbind(new_data_frame,x)
						}
					}
					data_frame = new_data_frame
					
					# For the dataframe containing only the fit dataspots
					# Calculate (y+1) - (y) for every selected column of data except the time column
					new_data_frame = data.frame()
					for(name in 1:length(levels(fit_data_frame$variable))){
						temp=data.frame()
						data_frame_subset = droplevels(subset(fit_data_frame,fit_data_frame$variable == levels(fit_data_frame$variable)[name]))
						for(entry in 2:nrow(data_frame_subset)){
							temp[entry-1,1] = data_frame_subset$value[entry] - data_frame_subset$value[(entry-1)]
						}

						#calculating the running average for temp ( the calculated y values)

						temp[,1] = running_average(temp[,1],input$curve_smooth)

						#replacing the initial data with the calculated differences
						if(nrow(new_data_frame) == 0){
							new_data_frame = data.frame(data_frame_subset$Time[2:nrow(data_frame_subset)], data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(new_data_frame) = c("Time", "variable", "value")
						}
						else{
							x = data.frame(data_frame_subset$Time[2:nrow(data_frame_subset)], data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(x) = c("Time","variable","value")
							new_data_frame = rbind(new_data_frame,x)
						}
					}
					fit_data_frame = new_data_frame

					p=ggplot(data=data_frame, aes(x=data_frame[,1], y=value, group = variable, colour = variable)) + geom_line(alpha=0.3) + 
					geom_line(data=fit_data_frame, aes(x=fit_data_frame[,1], y=value, group = variable, colour = variable), size=1.1) + 
					labs(x="Time", y="delta(value)") + theme(legend.position = "top") + background_grid(major = "xy", minor = "xy") 
					# Do not show legend for more then 20 data curves
					if(length(levels(data_frame[,2]))>20){
						p = p + theme(legend.position="none")
					}
					return(p)
				}
		}
	}

	##########
	# Execute curve progression plot 1
	##########
		
	output$plot_progression_1 = renderPlot({
		p=curve_progression_1(input$analyse_brush, output_overview_graph()$plot_brush, output_overview_graph()$data_frame)
		save_results$pg1_temp = p
		p
	})

	##########
	# Curve progression Plot 2
	##########
	
	plot_progression_2 = function(input_analyse_brush,input_plot_brush,source_data_frame){
		if(!is.null(input_analyse_brush) & !is.null(output_overview_graph()$custom_selection)){
				# Getting the selected data points from the zoomed in plot
				# Dataset containing the chosen datapoints for the fit
				fit_data_frame = brushedPoints(source_data_frame, input_analyse_brush)
				fit_data_frame = droplevels(fit_data_frame)
				if(nrow(fit_data_frame) != 0){
					# Dataset containing all datapoints form the zoomed in region
					data_frame = brushedPoints(source_data_frame, input_plot_brush)
					data_frame = droplevels(data_frame)
					
					
					# Calculate (y+1) - (y) for every selected column of data except the time column
					
					new_data_frame = data.frame()
					x_correction = data.frame()
					for(name in 1:length(levels(data_frame$variable))){
						temp = data.frame()
						data_frame_subset = subset(data_frame,data_frame$variable == levels(data_frame$variable)[name])
						data_frame_subset = droplevels(data_frame_subset)
						for(entry in 2:nrow(data_frame_subset)){
							temp[entry-1,1] = data_frame_subset$value[entry] - data_frame_subset$value[(entry-1)]
						}
						# Calculating a evently distributed x axis:
						x = data_frame_subset$value[2:nrow(data_frame_subset)]
						# Saving xmin and xmax for later use:
						x_correction[name,1] = min(x)
						x_correction[name,2] = max(x)

						x = ((x-min(x))/(max(x)-min(x)))*100

						#calculating the running average for temp ( the calculated y values)

						temp[,1] = running_average(temp[,1],input$curve_smooth)

						#replacing the initial data with the calculated differences
						if(nrow(new_data_frame) == 0){
							new_data_frame = data.frame(x, data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(new_data_frame) = c("y-value", "variable", "value")
						}
						else{
							x = data.frame(x, data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(x) = c("y-value","variable","value")
							new_data_frame = rbind(new_data_frame,x)
						}
					}
					data_frame = new_data_frame
					
					# Calculate (y+1)-y for the fit dataframe
					
					new_data_frame = data.frame()

					for(name in 1:length(levels(fit_data_frame$variable))){
						temp = data.frame()
						data_frame_subset = droplevels(subset(fit_data_frame,fit_data_frame$variable == levels(fit_data_frame$variable)[name]))
						for(entry in 2:nrow(data_frame_subset)){
							temp[entry-1,1] = data_frame_subset$value[entry] - data_frame_subset$value[(entry-1)]
						}

						#calculating the running average for temp ( the calculated y values)

						temp[,1] = running_average(temp[,1],input$curve_smooth)

						# Calculating a evently distributed x axis:
						x = data_frame_subset$value[2:nrow(data_frame_subset)]
						x = ((x-x_correction[name,1])/(x_correction[name,2]-x_correction[name,1]))*100
						
						#replacing the initial data with the calculated differences
						if(nrow(new_data_frame) == 0){
							new_data_frame = data.frame(x, data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(new_data_frame) = c("y-value", "variable", "value")
						}
						else{
							x = data.frame(x, data_frame_subset$variable[2:nrow(data_frame_subset)], temp)
							names(x) = c("y-value","variable","value")
							new_data_frame = rbind(new_data_frame,x)
						}
					}

					fit_data_frame = new_data_frame

					p=ggplot(data=data_frame, aes(x=data_frame[,1], y=value, group = variable, colour = variable)) + geom_line(alpha=0.3) + 
					geom_line(data=fit_data_frame, aes(x=fit_data_frame[,1], y=value, group = variable, colour = variable), size=1.1) + 
					labs(x="normalised value", y="delta(value)") + theme(legend.position = "top") + background_grid(major = "xy", minor = "xy")
					# Do not show legend for more then 20 data curves
					if(length(levels(data_frame[,2]))>20){
						p = p + theme(legend.position="none")
					}
					return(p)

				}
		}
	}

	##########
	# Execute curve progression plot 2
	##########

	output$plot_progression_2 = renderPlot({
		p = plot_progression_2(input$analyse_brush, output_overview_graph()$plot_brush, output_overview_graph()$data_frame)
		save_results$pg2_temp = p
		p
	})

	##########
	# Residual Plot
	##########

	residual_plot = function(input_analyse_brush,model_residual){
		if(!is.null(input_analyse_brush) & (!is.null(model$residual)) & !is.null(output_overview_graph()$custom_selection)){
				# Getting the residual dataframe used in the model (see local_fit_data) and directly melt the data for plotting
				mdf  = model_residual	
				# Plot the data
				p =ggplot(data=mdf, aes(x=Time, y=value, group = variable, colour = variable)) + geom_point()+ stat_smooth(method="loess") + geom_hline(yintercept=0) + theme(legend.position = "top") + background_grid(major = "xy", minor = "xy") 
				density= ggplot(data=mdf, aes(x=value, group=variable,colour=variable)) + geom_density() + coord_flip() + theme(legend.position="none",axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.y=element_blank())  
				# Save legend
				legend = get_legend(p)
				# remove legend
				p = p + theme(legend.position="none")
				# Create blank plot
				blank_plot <- ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()
				# Do not show legend for more then 20 curves
				if(length(levels(mdf[,2]))<=20){
					# arrange Plot grid
					grid.arrange(legend, blank_plot,p, density, ncol=2, nrow=2, widths = c(4, 1), heights = c(0.2, 2.5))
				}
				else{
					# arrange Plot grid
					grid.arrange(p, density, ncol=2, widths = c(4, 1))
				}	
		}
	}

	##########
	# Execute residual plot
	##########

	output$residual_plot = renderPlot({
		p = residual_plot(input$analyse_brush,model$residual)
		save_results$pg3_temp = p
		p
	})

	##########
	# Defining the model object for storing all model related data into it
	##########
	model = reactiveValues()
	model$predict = 0
	model$residual = 0
	model$source = 0
	model$results = NULL
	
	##########
	# Fitting a model to the selected curves from the zoomed plot
	##########
	local_fit_data_function <- function(source_data_frame,input_analyse_brush){
		#Get the data selected in the zoomed plot
		data = brushedPoints(source_data_frame, input_analyse_brush)
		data = droplevels(data)
		
		if(nrow(data) != 0){
			#Definde an empty data frame for temporarily storage of the modeling results
			results = data.frame()
			predicted_data = data.frame()
			model_resid = data.frame()
			data_source = data.frame()

			# Calculate the model for every user selected curve
			for (i in 1:length(levels(data$variable))){
				# subsetting the dataset
				data_sub = subset(data, data$variable == levels(data$variable)[i])
				# defining x (substracting the first x value from all other values to shift the curve to 0) and y inputs for the model
				x = data_sub[,1] - data_sub[1,1]
				y = data_sub[,3]
				start = min(y[1:3])
				stop = max(y[(length(y)-3):length(y)])
				# guessing model constants
				c= stop
				a = start - stop
				#guessing b (kobs)
				v = (stop-start)*0.6 + start
				q = y-v
				index = which.min(abs(q))
				tv = x[index]
				# at 1/e: b*tv=1
				b = 1/tv
				
				fm = try({
					# perform fit:
					nls(y ~ a * exp(-b * x) + c, start = list(a=a,b=b,c=c), control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1e-50, printEval = FALSE, warnOnly = TRUE))
				})
				if(class(fm)!="try-error"){
					# Saving model statistics and coefficients and generating results dataframe
					# Reagent concentration
					results[i,1] = "NA"
					# Spot names
					results[i,2] = levels(data$variable)[i]
					#A0
					results[i,3] = summary(fm)$coefficients[1, 1]
					# StErr(A0)
					results[i,4] = summary(fm)$coefficients[1, 2]
					# kobs
					results[i,5] = summary(fm)$coefficients[2, 1]
					# StErr(kobs)
					results[i,6] = summary(fm)$coefficients[2, 2]
					# y0
					results[i,7] = summary(fm)$coefficients[3, 1]
					# StErr (y0)
					results[i,8] = summary(fm)$coefficients[3, 2]
					# Comments
					results[i,9] = "NA"
					
					# Predict y-values according to the model for the used times
					predicted_values = predict(fm)
					# Getting the model residuals for every datapoint used
					resid_values = residuals(fm)
					# Saving the results in a dataframe (one column for every user chosen curve)
					
					temp_predict = data.frame(x,levels(data$variable)[i],predicted_values)
					names(temp_predict) = c("Time" , "variable" , "value")
					predicted_data = rbind(predicted_data,temp_predict)
					
					temp_resid = data.frame(x,levels(data$variable)[i],resid_values)
					names(temp_resid) = c("Time" , "variable" , "value")
					model_resid = rbind(model_resid,temp_resid)

					temp_source = data.frame(x,levels(data$variable)[i],y)
					names(temp_source) = c("Time" , "variable" , "value")
					data_source = rbind(data_source,temp_source)
				}
				else{
					results[i,1] = NA
					results[i,2] = levels(data$variable)[i]
					results[i,3:9] = NA
				}

			}	
			names(results) = c("c(Reagent) [M]","Spot","A0","StErr(A0)","kobs [1/t]",
							   "StErr(kobs) [1/t]","y0","StErr(y0)", "Comments")

			if(length(results)==0){
				predicted_data = 0
				model_resid = NULL
				data_source = 0
				results = 0
			}
		}
		else{
			predicted_data = 0
			model_resid = NULL
			data_source = 0
			results = 0
		}


		return_data = list(a=predicted_data,b=model_resid, c=data_source, d=results)
		
		return(return_data)

		}


    local_fit_data <- reactive({
        if(!is.null(input$analyse_brush) & !is.null(output_overview_graph()$custom_selection)){
			fit = local_fit_data_function(output_overview_graph()$data_frame,input$analyse_brush)
			model$predict = fit$a
			model$residual = fit$b
			model$source = fit$c
			model$results = fit$d
		}
	})

	##########
	# Fit results output
	##########
	output$fit_results <- renderTable( digits = 6, {    
		if(!is.null(input$analyse_brush)){
			local_fit_data()
			shiny::validate(
				need(model$results!=0,"Unable to calculate fit. Please chance area!")
			)
			model$results
		}
	})

	values <- reactiveValues()
	values$all_fit_results = NULL

	
#	output$test <- renderPrint({
#		print()
#	})


	##########
	# Module output
	##########

	reactive_module_output = reactive({
		list("local_fit_data" = local_fit_data(), "model_results" = model$results, "fit_graphs_temp" = save_results$fit_graphs_temp, "analyse_brush" = input$analyse_brush, "pg1_temp" = save_results$pg1_temp, "pg2_temp" = save_results$pg2_temp, "pg3_temp" = save_results$pg3_temp ,"show_hide_curve_analysis" = input$show_hide_curve_analysis)
	})

	return(reactive_module_output)

}
