#####################################################################################################################################################################
#####################################################################################################################################################################
#
# __  ___   ______   .______        _______.    __       __  .__   __.  _______ .______       __       _______.     ___   .___________. __    ______   .__   __. 
#|  |/  /  /  __  \  |   _  \      /       |   |  |     |  | |  \ |  | |   ____||   _  \     |  |     /       |    /   \  |           ||  |  /  __  \  |  \ |  | 
#|  '  /  |  |  |  | |  |_)  |    |   (----`   |  |     |  | |   \|  | |  |__   |  |_)  |    |  |    |   (----`   /  ^  \ `---|  |----`|  | |  |  |  | |   \|  | 
#|    <   |  |  |  | |   _  <      \   \       |  |     |  | |  . `  | |   __|  |      /     |  |     \   \      /  /_\  \    |  |     |  | |  |  |  | |  . `  | 
#|  .  \  |  `--'  | |  |_)  | .----)   |      |  `----.|  | |  |\   | |  |____ |  |\  \----.|  | .----)   |    /  _____  \   |  |     |  | |  `--'  | |  |\   | 
#|__|\__\  \______/  |______/  |_______/       |_______||__| |__| \__| |_______|| _| `._____||__| |_______/    /__/     \__\  |__|     |__|  \______/  |__| \__| 
#                                                                                                                                                                
#
#####################################################################################################################################################################
#####################################################################################################################################################################

source("overview_graph.R")
source("zoom_and_fit.R")

# Load dependent R Functions

source("complete_plot.R")
source("generate_graph.R")


`%then%` <- shiny:::`%OR%`

kobs_lin_UI = function(id){
	ns = NS(id)
	tabPanel("Evaluation Method 1: kobs Linearisation",
	fluidPage(
	#++++++++++++++++++++
	#####################
	# Calculating Message
	#####################
	#++++++++++++++++++++
			 tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 50px;
               right: 0px;
               padding: 10px 15px 10px 15px;
			   border-radius: 15px;
               text-align: center;
               font-size: 18px;
               color: #000000;
			   background: #ECECEC;
               z-index: 105;
             }
          ")),
	conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div("Calculating...",id="loadmessage")),

	#++++++++++++++++++++
	#####################
	# MODULE UI
	#####################
	#++++++++++++++++++++
		column(width=12,align="center",
		h1("kobs linearisation")
		),
		overview_graph_UI(ns("kobs_overview_graph")),
		zoom_and_fit_UI(ns("kobs_zoom_and_fit")),

	#++++++++++++++++++++
	#####################
	# ALL OTHER UI
	#####################
	#++++++++++++++++++++
	column(width=12,
					# Save button
			column(width = 12,align = "center",
				   actionButton(ns("save_button"),"Save Results")
				   ),
			# Fit statistic table
			column(width = 12,
				   h4("Fit Statistics"),
				   tableOutput(ns("fit_results"))
				   ),
			sidebarLayout(
				sidebarPanel(
					tabsetPanel(
						tabPanel("Edit",
								 # Input fields
								 textInput(inputId = ns("fit_results_row_number"),label = "Row ID", value = ""),
								 textInput(inputId = ns("fit_results_name"), label = "Spot Name", value = ""),
								 textInput(inputId = ns("fit_results_reagent_concentration"),label = "Reagent Concentration", value = ""), 
								 radioButtons(inputId=ns("reagent_concentration"),choices=list("nM","µM","mM","M"),inline=T,label="Reagent Concentration"),
								 textInput(inputId = ns("fit_results_comments"), label = "Comments", value = ""),
								 # Action Buttons
								 actionButton(ns("fit_results_update"),"Update Table")
								 ),
						tabPanel("Column selection",
								 checkboxGroupInput(inputId= ns("fit_results_table_column_selection"), choices = NULL, selected = NULL, label = NULL)
								 )
						)
					),
				mainPanel(
					column(width=12, class = "well",
					   h4("All Saved Fit Results"),
					   DT::dataTableOutput(outputId=ns("all_fit_results"))
					   )
					)
				),
			# Settings for kobs plot
			column(width = 12, align="center",  class = "well",
				   textInput(inputId=ns("binding_spot_name"),width=400, label="Name", value=""),
				   radioButtons(inputId=ns("kobs_lin_mod"),choices=list("Single", "Automatically by Name"),inline=T,label="Select mode")
				   ),
			# Plot for fitting Kobs values
			column(width = 12, class = "well",
				   h4 = "Calulate kobs or kdis",
				   plotOutput(outputId=ns("binding_constants_plot"))
				   ),
			# Show table with calculated kass and kdis
			column(width = 12, align = "center", class = "well",
				   h4 = "Calculates binding constants from fit",
				   tableOutput(outputId=ns("table_binding_constants")),
				   actionButton(ns("save_kobs_fit"),"Save Fit")
				   ),
			# Summerise all saved kass and kdis values
			column(width = 12, align="center",
				   h4 = "Summary of all saved binding constants",
				   tableOutput(outputId=ns("all_binding_constants"))
				   ),
			column(width=12, align="center", class="well",
				actionButton(ns("generate_download_file"), "Generate Result File"),
				downloadButton(outputId = ns("download_result_file"), label="Download Result File")
				)
		   )
#			  	column(width = 12, class = "well",
#					tableOutput(outputId=ns("test_table")),
#					verbatimTextOutput(outputId=ns("test"))
#					)


		)
	)
}

kobs_lin = function(input, output, session){

	#++++++++++++++++++++
	#####################
	# LOADING OVERVIEW GRAPH MOD
	#####################
	#++++++++++++++++++++

	output_overview_graph = callModule(overview_graph,"kobs_overview_graph")

	#++++++++++++++++++++
	#####################
	# LOADING ZOOM AND FIT MOD
	#####################
	#++++++++++++++++++++
	
	output_zoom_and_fit = callModule(zoom_and_fit,"kobs_zoom_and_fit", output_overview_graph)

	#++++++++++++++++++++
	#####################
	# MODULE SPECIFIC CODE
	#####################
	#++++++++++++++++++++
	
	test = reactiveValues()
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
	
	# Values for kobs_lin sheet
	save_results$kobs_df = list()

	# Save overview graphs for every fit
	save_results$complete_graph = list()

	save_results$wb = NULL

	
	values <- reactiveValues()
	values$all_fit_results = NULL
	values$fit_number = 1

	##########
	# Yank the Fitresults for later use -> Transferes the results to the editable dataframe (DT:: Dataframe)
	# Also save the overview plot with all curves to save it in the excel file at the end; this is done here because if fit data was saved, than the corresponding dataset will definitly be used in the analysis
	# This will ensure that no overview plot will be saved from a dataset that was uploaded accidently
	##########

	fit_data <- observeEvent(input$save_button,{
		# Write fit number and ID number to the local fit data
		# Give a running row number to the current fit results
		
		fit_results_row_number = output_zoom_and_fit()$local_fit_data
		if(is.null(values$all_fit_results)){
			a = 1
		}
		else{						 
			a = nrow(values$all_fit_results)+1
		}
		b = a-1+nrow(fit_results_row_number)
		fit_results_row_number$row_ID = c(seq(a,b))
		# Add clumn containing the fit number
		fit_results_row_number$fit_ID = values$fit_number
		# increase the fit number for the next fit
		values$fit_number = values$fit_number + 1
		# Save the added fit results to the editable dataframe
		values$all_fit_results = rbind(values$all_fit_results,fit_results_row_number)

		# Save overview plot
		# CODE IS THE SAME AS FOR data_frame() BUT NO CUSTOM SELECTION IS ALLOWED. INSTEAD EVERYTHING IS SELECTED TO GENERATE A REPRESANTIVE OWERVIEW PLOT	
		# Prevent showing error
		# takes the data frame which was selected by user
		if(is.null(output_overview_graph()$data_hist_df)){
			df = output_overview_graph()$data_frame_readin
		}
		else{
			df = output_overview_graph()$data_hist_df
		}


		# Read the uploaded file name
		filename = output_overview_graph()$data_frame_readin_filename
		
		if(!(grepl("BIACORE",filename) | grepl("OCTET",filename))){
			# Update the time range according to slider input
			time_min = as.numeric(output_overview_graph()$time_range[1])
			time_max = as.numeric(output_overview_graph()$time_range[2])

			df = df[which(df[[1]] >time_min & df[[1]] <time_max),]	
		}		
		
		###
		#Save overview Graphs and file names
		# generate the overview plot
		plot = complete_plot(df,output_overview_graph()$log_file_relevant , output_overview_graph()$plot_ymax , output_overview_graph()$plot_ymin, output_overview_graph()$dead_volume)
		
		# Store the plot and the corresponding filename
		if(length(save_results$overview_graphs) == 0){
			save_results$overview_graphs[[1]] = plot
			save_results$all_used_datasets[[1]] = filename
		}
		else if(save_results$all_used_datasets[[length(save_results$all_used_datasets)]] != filename){
			save_results$overview_graphs[[length(save_results$overview_graphs)+1]] = plot
			save_results$all_used_datasets[[length(save_results$all_used_datasets)+1]] = filename
		}

		###
		# Save all fit graphs and fit results
		if(length(save_results$fit_results) == 0){
			save_results$fit_graphs[[1]] = output_zoom_and_fit()$fit_graphs_temp
			save_results$fit_results[[1]] = output_zoom_and_fit()$local_fit_data
			save_results$complete_graph[[1]] = plot

			###
			# Save the coordinates of the chosen fitting area
			fitting_area = output_zoom_and_fit()$analyse_brush
			save_results$fit_regions[1,1] = fitting_area$xmin
			save_results$fit_regions[1,2] = fitting_area$xmax
			save_results$fit_regions[1,3] = fitting_area$ymin
			save_results$fit_regions[1,4] = fitting_area$ymax
		
			# Save the graph number to which the region belongs
			save_results$fit_regions[1,5] = length(save_results$all_used_datasets)
		}
		else if(identical(save_results$fit_results[[length(save_results$fit_results)]],output_zoom_and_fit()$local_fit_data) != TRUE){
			save_results$fit_graphs[[length(save_results$fit_graphs)+1]] = output_zoom_and_fit()$fit_graphs_temp
			save_results$fit_results[[length(save_results$fit_results)+1]] = output_zoom_and_fit()$local_fit_data
			save_results$complete_graph[[length(save_results$complete_graph)+1]] = plot

			###
			# Save the coordinates of the chosen fitting area
			fitting_area = output_zoom_and_fit()$analyse_brush
			row_number = nrow(save_results$fit_regions)+1
			save_results$fit_regions[row_number,1] = fitting_area$xmin
			save_results$fit_regions[row_number,2] = fitting_area$xmax
			save_results$fit_regions[row_number,3] = fitting_area$ymin
			save_results$fit_regions[row_number,4] = fitting_area$ymax
			# Save the graph number to which the region belongs
			save_results$fit_regions[row_number,5] = length(save_results$all_used_datasets)
		}
		###
		# Save all curve progression plots if calculated
		if(length(save_results$pg1) == 0){
			if(output_zoom_and_fit()$show_hide_curve_analysis == 'Show'){
				save_results$pg1[[1]] = output_zoom_and_fit()$pg1_temp
				save_results$pg2[[1]] = output_zoom_and_fit()$pg2_temp
				save_results$pg3[[1]] = output_zoom_and_fit()$pg3_temp
			}
			else{
				save_results$pg1[[1]] = NA
				save_results$pg2[[1]] = NA
				save_results$pg3[[1]] = NA
			}
		}
		else{
			if(output_zoom_and_fit()$show_hide_curve_analysis == 'Show'){
				save_results$pg1[[length(save_results$pg1)+1]] = output_zoom_and_fit()$pg1_temp
				save_results$pg2[[length(save_results$pg2)+1]] = output_zoom_and_fit()$pg2_temp
				save_results$pg3[[length(save_results$pg3)+1]] = output_zoom_and_fit()$pg3_temp
			}
			else{
				save_results$pg1[[length(save_results$pg1)+1]] = NA
				save_results$pg2[[length(save_results$pg2)+1]] = NA
				save_results$pg3[[length(save_results$pg3)+1]] = NA
			}
		}

	})


	##########
	# Editable Dataframe holding all stored yanked fit results
	##########
	output$all_fit_results <- DT::renderDataTable({
		df = values$all_fit_results
		df_names = names(df)
		# Define which columns should be displayed
		selection = input$fit_results_table_column_selection
		# when the first fit results are saved, selection is NULL and therefore it selects the default columns which are:
		if(is.null(selection)){
			# Defauld column selection
			updateCheckboxGroupInput(session, "fit_results_table_column_selection", choices = df_names,
								 	selected = c("c(Reagent) [M]","Spot","kobs [1/t]","y0","A0","Comments"))
		}
		# Use the selection onto the dataframe
		df = df[,input$fit_results_table_column_selection]
		# Output the dataframe and set the number of entries per page to 100
		DT::datatable(df, options = list( lengthMenu = c(100,500,1000)))
	})
	
	##########
	# Editing the Saved Fit results
	##########
	# Event is triggert through button
	fit_results_update <- observeEvent(input$fit_results_update, {
		# Load the data
		df = values$all_fit_results
		
		# Editing function if a comma sign is given with the row ID field
		comma_update = function(row_field, field, df, column_edit_number,input_reagent_concentration,concentration=F){
			#takes the row id input and splits the entry according to comma
			temp = strsplit(row_field,",")
			# exctract the single list
			temp = as.numeric(temp[[1]])
			# Execute if the editing input is neither empty nor contains a comma. Therefore only execute when one input is given
			if(field != "" & !grepl(",",field)){
				# Edit every row id with the same, single entry from the other field
				if(concentration==F){
					for(i in 1:length(temp)){
						df[temp[i],column_edit_number] = field
					}
				}
				# if the field is the concentration field
				else{
					if(input_reagent_concentration == "M"){
						for(i in 1:length(temp)){
							df[temp[i],column_edit_number] = as.numeric(field)
						}
					}
					else if(input_reagent_concentration == "mM"){
						for(i in 1:length(temp)){
							df[temp[i],column_edit_number] = as.numeric(field)/1000
						}
					}
					else if(input_reagent_concentration == "µM"){
						for(i in 1:length(temp)){
							 df[temp[i],column_edit_number] = as.numeric(field)/1000000
						}
					}
					else if(input_reagent_concentration == "nM"){
						for(i in 1:length(temp)){
							df[temp[i],column_edit_number] = as.numeric(field)/1000000000
						}
					}
				}

			}
			# Execute if the editing input is empty, contains a comma and has as many entries as the row id field
			else if(field != "" & grepl(",",field) & (length(strsplit(field,",")[[1]]) == length(temp))){
				#Edit every row with the corresponding entry
				if(concentration==F){
					for(i in 1:length(temp)){
						df[temp[i],column_edit_number] = (strsplit(field,",")[[1]])[i]
					}
				}
				# if the field is the concentration field
				else{
					if(input_reagent_concentration == "M"){
						for(i in 1:length(temp)){
							df[temp[i],column_edit_number] = as.numeric((strsplit(field,",")[[1]])[i])
						}
					}
					else if(input_reagent_concentration == "mM"){
						for(i in 1:length(temp)){
							df[temp[i],column_edit_number] = as.numeric((strsplit(field,",")[[1]])[i])/1000
						}
					}
					else if(input_reagent_concentration == "µM"){
						for(i in 1:length(temp)){
							df[temp[i],column_edit_number] = as.numeric((strsplit(field,",")[[1]])[i])/1000000
						}
					}
					else if(input_reagent_concentration == "nM"){
						for(i in 1:length(temp)){
							df[temp[i],column_edit_number] = as.numeric((strsplit(field,",")[[1]])[i])/1000000000
						}
					}
				}
			}
			# return the edited dataframe
			return(df)
		}
		# Editing function if a minus sign is present in the row id field
		minus_update = function(row_field, field, df, column_edit_number,input_reagent_concentration,concentration=F){
			##takes the row id input and splits the entry according to the minus sign
			temp = strsplit(row_field,"-")
			#Extracts the single list
			temp = as.numeric(temp[[1]])
			# Makes sure the only 2 numbers are given (from to)
			if(length(temp) == 2){
				# Extract the start and end id values
				start = as.numeric(temp[1])
				end = as.numeric(temp[2])
				# Change data entries if a entry is present according to the given Row ID number
				#Edit the dataframe
				if(field != "" & !grepl(",",field)){
					if(concentration==F){
					df[start:end,column_edit_number] = field
					}
					# Correct the concentration to M if the given field is the Reagent concentration field
					else{
						if(input_reagent_concentration == "M"){
							df[start:end,column_edit_number] = as.numeric(field)
						}
						else if(input_reagent_concentration == "mM"){
							df[start:end,column_edit_number] = as.numeric(field)/1000
						}
						else if(input_reagent_concentration == "µM"){
							df[start:end,column_edit_number] = as.numeric(field)/1000000
						}
						else if(input_reagent_concentration == "nM"){
							df[start:end,column_edit_number] = as.numeric(field)/1000000000
						}
					}

				}
				# If values in the field are also comma seperated
				if(field != "" & grepl(",",field) & (length(strsplit(field,",")[[1]]) == length(start:end))){
					if(concentration==F){
						df[start:end,column_edit_number] = (strsplit(field,",")[[1]])
					}
					# Correct the concentration to M if the given field is the Reagent concentration field
					else{
						if(input_reagent_concentration == "M"){
							df[start:end,column_edit_number] = as.numeric((strsplit(field,",")[[1]]))
						}
						else if(input_reagent_concentration == "mM"){
							df[start:end,column_edit_number] = as.numeric((strsplit(field,",")[[1]]))/1000
						}
						else if(input_reagent_concentration == "µM"){
							df[start:end,column_edit_number] = as.numeric((strsplit(field,",")[[1]]))/1000000
						}
						else if(input_reagent_concentration == "nM"){
							df[start:end,column_edit_number] = as.numeric((strsplit(field,",")[[1]]))/1000000000
						}
					}
				}
			}
			return(df)
		}

		# If something was written into the row ID Field
		if(input$fit_results_row_number != ""){
			if(!is.na(as.numeric(input$fit_results_row_number))){
				# Change data entries if a entry is present according to the given Row ID number
				#Edit the Spotname
				if(input$fit_results_name != ""){
					df[input$fit_results_row_number,2] = input$fit_results_name
				}
				#Edit the reagent concentration
				if(input$fit_results_reagent_concentration != ""){
					if(input$reagent_concentration == "M"){
						df[input$fit_results_row_number,1] = as.numeric(input$fit_results_reagent_concentration)
					}
					else if(input$reagent_concentration == "mM"){
						df[input$fit_results_row_number,1] = as.numeric(input$fit_results_reagent_concentration)/1000
					}
					else if(input$reagent_concentration == "µM"){
						df[input$fit_results_row_number,1] = as.numeric(input$fit_results_reagent_concentration)/1000000
					}
					else if(input$reagent_concentration == "nM"){
						df[input$fit_results_row_number,1] = as.numeric(input$fit_results_reagent_concentration)/1000000000
					}
				}
				#Edit the comments column
				if(input$fit_results_comments != ""){
					df[input$fit_results_row_number,9] = input$fit_results_comments
				}
			}
			else if(grepl("-",input$fit_results_row_number)){
				df = minus_update(input$fit_results_row_number,input$fit_results_name,df,2,input$reagent_concentration)
				df = minus_update(input$fit_results_row_number,input$fit_results_reagent_concentration,df,1,input$reagent_concentration,concentration=T)
				df = minus_update(input$fit_results_row_number,input$fit_results_comments,df,9,input$reagent_concentration)
			}
			else if(grepl(",",input$fit_results_row_number)){
				df = comma_update(input$fit_results_row_number,input$fit_results_name,df,2,input$reagent_concentration)
				df = comma_update(input$fit_results_row_number,input$fit_results_reagent_concentration,df,1,input$reagent_concentration,concentration=T)
				df = comma_update(input$fit_results_row_number,input$fit_results_comments,df,9,input$reagent_concentration)
			}
		}
		# Return edited dataframe
		values$all_fit_results = df
	})
	
	# Reactive object for storing the binding constants
	binding_constants <- reactiveValues()
	binding_constants$statistics = data.frame()
	##########
	# Kobs plot from dataframe selection
	##########
	
	test = reactiveValues()

	output$binding_constants_plot <- renderPlot({
		if(!is.null(values$all_fit_results)){
			if(input$kobs_lin_mod == "Single"){
				temp = suppressWarnings(na.omit(as.numeric(values$all_fit_results[input$all_fit_results_rows_selected,"c(Reagent) [M]"])))
				shiny::validate(
					need(length(input$all_fit_results_rows_selected)>=3,"Choose 3 or more datapoints from table in order to produce a fit!") %then%
					need(length(temp) >= 3, "Provide at least 3 c(Reagent) values with the selected datapoints!")
					)
				if(length(input$all_fit_results_rows_selected) >= 3){
					#Get all selected rows from the DT:: dataframe
					selected_rows = input$all_fit_results_rows_selected
					# Load the fit results dataframe
					df = values$all_fit_results
					all_selected_rows = df[selected_rows,]
					x_values = df[selected_rows,"c(Reagent) [M]"]
					x_values = as.numeric(x_values)
					y_values = df[selected_rows,"kobs [1/t]"]
					y_values = as.numeric(y_values)
					row_id = df[selected_rows,"row_ID"]
					fit_id = df[selected_rows,"fit_ID"]
					s_kobs = df[selected_rows,"StErr(kobs) [1/t]"]
					plot_df = na.omit(data.frame(x_values,y_values))
					# Fit model to the selected data points
					# We expect the data to be a linear regression and therefore a glm (generelised linear model) was used
					fit = glm(plot_df[,2] ~ plot_df[,1])
					# Calsulate y-Values according to fit
					predicted_values = predict(fit)
					# Calculate kdis (Intercept)
					kdiss = summary(fit)$coefficients[1,1]
					sterr_kdis = summary(fit)$coefficients[1,2]
					# Calculate kass (Slope)
					kass = summary(fit)$coefficients[2,1]
					sterr_kass = summary(fit)$coefficients[2,2]
					# Storing the constants in a global variable
					temp_df = data.frame()
					if(input$binding_spot_name == ""){
						updateTextInput(session, "binding_spot_name", label="Name", value=all_selected_rows[1,2])
						temp_spot_name = all_selected_rows[1,2]
					}
					else{
						temp_spot_name = input$binding_spot_name
					}

					save_df = na.omit(data.frame(temp_spot_name,x_values,y_values,s_kobs,row_id,fit_id))
					names(save_df) = c("Name","c(Reagent) [M]","kobs [1/t]","StErr(kobs) [1/t]","row_ID","fit_ID")
					save_results$kobs_df_temp = save_df

					temp_df[1,1] = temp_spot_name
					temp_df[1,2] = kass
					temp_df[1,3] = sterr_kass
					temp_df[1,4] = kdiss
					temp_df[1,5] = sterr_kdis
					temp_df[1,6] = kdiss/kass
					temp_df[1,7] = temp_df[1,6] * sqrt(((temp_df[1,5]^2)/(temp_df[1,4]^2)) + ((temp_df[1,3]^2)/(temp_df[1,2]^2))) 
					names(temp_df) = c("Name","kass [1/t*M]","StErr(kass) [1/t*M]","kdiss [1/t]","StErr(kdiss) [1/t]","KD [M]", "StErr(KD) [M]")
					binding_constants$statistics = temp_df
					#update the spot name text field with a guessed spot name


					# Plot the selected datapoints
					binding_constants$kobs_plots_temp = ggplot(data=plot_df, aes(x=x_values,y=y_values)) + labs(x="c(Reagent) [M]", y="kobs [1/t]")  + geom_point() +
					geom_line(data=plot_df, aes(x=x_values , y=predicted_values)) + background_grid(major = "xy", minor = "xy") 
					binding_constants$kobs_plots_temp
				}
				else{
					binding_constants$statistics = data.frame()
				}
			}
			#automatic row selection
			else{
				df=values$all_fit_results
				spots = unique(df[,2])
				temp = data.frame()
				binding_constants$all_predicted_values = data.frame()
				mdf=data.frame()
				all_predicted_values = data.frame()
				kobs_df_temp = data.frame()
				for(i in 1:length(spots)){
					single_spot=spots[i]
					spot_data=df[which(df[,2]==single_spot),]
					x_values = spot_data[,"c(Reagent) [M]"]
					x_values = as.numeric(x_values)
					y_values = spot_data[,"kobs [1/t]"]
					y_values = as.numeric(y_values)
					row_id = spot_data[,"row_ID"]
					fit_id = spot_data[,"fit_ID"]
					s_kobs = spot_data[,"StErr(kobs) [1/t]"]
					
					plot_df = na.omit(data.frame(x_values,y_values))
					
					save_df = na.omit(data.frame(single_spot,x_values,y_values,s_kobs,row_id,fit_id))
					names(save_df) = c("Name","c(Reagent) [M]","kobs [1/t]","StErr(kobs) [1/t]","row_ID","fit_ID")

					shiny::validate(
						need(nrow(plot_df)>=3,"One or more of the spots have 2 or less given c(Reagents)")
						)
					plot_df[,3] = single_spot
					# Fit model to the selected data points
					# We expect the data to be a linear regression and therefore a glm (generelised linear model) was used
					fit = glm(plot_df[,2] ~ plot_df[,1])
					# Calsulate y-Values according to fit
					predicted_values = predict(fit)
					predicted_values = data.frame(predicted_values)
					# Calculate kdis (Intercept)
					kdiss = summary(fit)$coefficients[1,1]
					sterr_kdis = summary(fit)$coefficients[1,2]
					# Calculate kass (Slope)
					kass = summary(fit)$coefficients[2,1]
					sterr_kass = summary(fit)$coefficients[2,2]
					# Storing the constants in a global variable
					temp_df = data.frame()
					temp_df[1,1] = single_spot
					temp_df[1,2] = kass
					temp_df[1,3] = sterr_kass
					temp_df[1,4] = kdiss
					temp_df[1,5] = sterr_kdis
					temp_df[1,6] = kdiss / kass
					temp_df[1,7] = temp_df[1,6] * sqrt(((temp_df[1,5]^2)/(temp_df[1,4]^2)) + ((temp_df[1,3]^2)/(temp_df[1,2]^2))) 
					names(temp_df) = c("Name","kass [1/t*M]","StErr(kass) [1/t*M]","kdiss [1/t]","StErr(kdiss) [1/t]","KD [M]","StErr(KD) [M]")
					mdf = rbind(mdf,plot_df)
					all_predicted_values = rbind(all_predicted_values, predicted_values)
					temp = rbind(temp,temp_df)
					kobs_df_temp = rbind(kobs_df_temp,save_df)
				}
				binding_constants$statistics = temp
				binding_constants$kobs_plots_temp = ggplot(data=mdf, aes(x=x_values, y=y_values, col = V3))  + labs(x="c(Reagent) [M]", y="kobs [1/t]")  + geom_point()+
				geom_line(data=mdf, aes(x=x_values , y=all_predicted_values[,1])) + background_grid(major = "xy", minor = "xy") 
				save_results$kobs_df_temp = kobs_df_temp
				binding_constants$kobs_plots_temp
			}
		}
	})
	
	##########
	# Binding Constants Output
	##########
	output$table_binding_constants <- renderTable(digits = -3, {
		binding_constants$statistics
	})

	##########
	# Save the kobs fit results
	##########

	binding_constants$all_kobs_fit_statistics = NULL

	save_kobs_results = observeEvent(input$save_kobs_fit,{
		binding_constants$all_kobs_fit_statistics = rbind(binding_constants$all_kobs_fit_statistics, binding_constants$statistics)
		if(length(binding_constants$kobs_plots) == 0){
			binding_constants$kobs_plots[[1]] = binding_constants$kobs_plots_temp
			binding_constants$single_statistics[[1]] = binding_constants$statistics
			save_results$kobs_df[[1]] = save_results$kobs_df_temp
		}
		else{
			binding_constants$kobs_plots[[length(binding_constants$kobs_plots) + 1]] = binding_constants$kobs_plots_temp
			binding_constants$single_statistics[[length(binding_constants$single_statistics)+1]]=binding_constants$statistics
			save_results$kobs_df[[length(save_results$kobs_df)+1]] = save_results$kobs_df_temp
		}
	})

	##########
	# Output summary of all saved binding constants
	##########

	output$all_binding_constants = renderTable(digits=-3, {
		binding_constants$all_kobs_fit_statistics
	})

	##########
	# Generate Download File
	##########
	
	observeEvent(input$generate_download_file, {
		
    	# Create a Progress object
    	progress <- shiny::Progress$new()

    	progress$set(message = "Picking Flowers", value = 0)
    	on.exit(progress$close())
		
		progress$inc(1/5)
		wb <- createWorkbook()
		#Create the summery Sheet
		addWorksheet(wb, "summery")
		for(entry in 1:length(save_results$overview_graphs)){
			if(entry == 1){
				name_position = 1
				position_start = 3
				position_end = 35
			}
			else{
				name_position = position_end + 2
				position_start = position_start + 37
				position_end = position_end + 37
			}
			p = save_results$overview_graphs[[entry]]
			# Add all the regions into the overview plot that were used for fitting
			for(i in 1:nrow(save_results$fit_regions)){
				# Make sure only regions of the corresponding dataset is added
				if(save_results$fit_regions[i,5] == entry){
					p = p + geom_rect(data=save_results$fit_regions[i,], inherit.aes=FALSE, aes(xmin=V1, xmax=V2, ymin=V3, ymax=V4), alpha=0, fill="blue", color="black", size=0.3)
					fit_number = paste("#",i,sep="")
					p = p + annotate("text",x=save_results$fit_regions[i,1], y=save_results$fit_regions[i,4], label=fit_number)
				}
			}
			complete_plot_name = paste0("complete_plot",session$token,as.character(entry),".png")
			ggsave(complete_plot_name, plot = p, device = "png", width=50, units = c("cm"))
			insertImage(wb, "summery", complete_plot_name, startRow = position_start, startCol = 2, width = 48.61,height=17.46,units="cm")
			writeData(wb,"summery",save_results$all_used_datasets[[entry]],startRow=name_position, startCol=2)
		}

		progress$inc(1/5)
		progress$set(message = "Counting stars (may take a while...)")	
		#Create and write the Fits Sheet
		addWorksheet(wb,"fits")
		for(entry in 1:length(save_results$fit_graphs)){
			if(entry == 1){
				fits_line_start = 2
			}
			else{
				if(nrow(save_results$fit_results[[entry-1]]) > 41){
					fits_line_start = fits_line_start + nrow(save_results$fit_results[[entry-1]]) + 3
				}
				else{
					fits_line_start = fits_line_start + 49
				}
			}
			# Space for fitting plot
			fit_plot_name = paste0("fit_plot",session$token,as.character(entry),".png")
			ggsave(fit_plot_name, plot= save_results$fit_graphs[[entry]], device = "png", width=25, units = c("cm"))
			if(!is.na(save_results$pg1[entry])){
				pg1_plot_name = paste0("pg1_plot",session$token,as.character(entry),".png")
				ggsave(pg1_plot_name, plot=save_results$pg1[[entry]], device = "png", width=25, units = c("cm"))
				pg2_plot_name = paste0("pg2_plot",session$token,as.character(entry),".png")
				ggsave(pg2_plot_name, plot=save_results$pg2[[entry]], device = "png", width=25, units = c("cm"))
				pg3_plot_name = paste0("pg3_plot",session$token,as.character(entry),".png")
				ggsave(pg3_plot_name, plot=save_results$pg3[[entry]], device = "png", width=25, units = c("cm"))
			}
			
			# Generate and create complete plot with fitting regions
			p = save_results$complete_graph[[entry]]
			p = p + geom_rect(data=save_results$fit_regions[entry,], inherit.aes=FALSE, aes(xmin=V1, xmax=V2, ymin=V3, ymax=V4), alpha=0, fill="blue", color="black", size=0.3)
			fit_number = paste("#",entry,sep="")
			p = p + annotate("text",x=save_results$fit_regions[entry,1], y=save_results$fit_regions[entry,4], label=fit_number)
			
			complete_fit_plot_name = paste0("complete_fit_plot",session$token,as.character(entry),".png")
			ggsave(complete_fit_plot_name, plot= p, device = "png", width=35, units = c("cm"))

			#Fit plot
			insertImage(wb,"fits",fit_plot_name, startRow=fits_line_start, startCol=2,width=14.96,height=12,7,units="cm")
			#Fit number
			writeData(wb,"fits",paste("#",entry,sep=""),startRow=fits_line_start, startCol=1)
			#Fit results
			writeData(wb,"fits",save_results$fit_results[[entry]][2:8],startRow=fits_line_start, startCol=11)
			#Fit regions
			fit_regions = save_results$fit_regions[entry,1:4]
			names(fit_regions) = c("xmin","xmax","ymin","ymax")
			region_title = "Chosen fitting range:"
			writeData(wb,"fits",region_title,startRow=fits_line_start + 25, startCol=2)
			writeData(wb,"fits",fit_regions,startRow=fits_line_start + 26, startCol=2)

			if(!is.na(save_results$pg1[entry])){
			# Curve progression plots
			insertImage(wb,"fits",pg1_plot_name, startRow=fits_line_start, startCol=27,width=13.09,height=12.7,units="cm")
			insertImage(wb,"fits",pg2_plot_name, startRow=fits_line_start, startCol=35,width=14.96,height=12.7,units="cm")
			insertImage(wb,"fits",pg3_plot_name, startRow=fits_line_start, startCol=44,width=14.96,height=12.7,units="cm")

			}
			
			# Add complete plot
			insertImage(wb,"fits",complete_fit_plot_name, startRow=fits_line_start + 29, startCol=2 ,width=14.97,height=6.88,units="cm")
		}
		
		progress$inc(1/5)
		progress$set(message = "Defeat the Evil")	
		
		# Create all_fit_results sheet
		if(nrow(values$all_fit_results) != 0){
			addWorksheet(wb,"all_fit_results")
			writeData(wb,"all_fit_results",values$all_fit_results,startRow=1, startCol=2)
		}
		
		progress$inc(1/5)

		#Create kobs linerisation fits sheet
		if(length(binding_constants$kobs_plots) != 0){
			addWorksheet(wb,"all_kobs_fits")
			for(entry in 1:length(binding_constants$kobs_plots)){
				if(entry == 1){
					fits_line_start = 2
				}
				else{
					if(length(binding_constants$single_statistics[[entry-1]]) > 23){
						fits_line_start = fits_line_start + length(save_results$fit_results[[entry-1]]) + 3
					}
					else{
						fits_line_start = fits_line_start + 26
					}
				}
				
				kobs_plot_name = paste0("kobs_plot",session$token,as.character(entry),".png")
				ggsave(kobs_plot_name, plot= binding_constants$kobs_plots[[entry]], device = "png", width=30, units = c("cm"))					
				
				insertImage(wb,"all_kobs_fits",kobs_plot_name,startRow=fits_line_start, startCol=2,width=18.7,height=12.7,units="cm")
				writeData(wb,"all_kobs_fits",paste(binding_constants$single_statistics[[entry]][,1],collapse=", "), startRow=fits_line_start-1, startCol=1)
				writeData(wb,"all_kobs_fits",binding_constants$single_statistics[[entry]],startRow=fits_line_start, startCol=13)
				writeData(wb,"all_kobs_fits",save_results$kobs_df[[entry]],startRow=(fits_line_start + nrow(binding_constants$single_statistics[[entry]]) + 2), startCol=13)
			}

			# Create binding constants summery sheet
			addWorksheet(wb,"all_binding_constants")
			writeData(wb,"all_binding_constants",binding_constants$all_kobs_fit_statistics,startRow=1, startCol=2)
		}
		
		progress$inc(1/5)
		progress$set(message = "You Are Welcome!")
		Sys.sleep(1)

		save_results$wb = wb
	})

	##########
	# Save result file
	##########
	output$download_result_file <- downloadHandler(
		
		filename = function() { paste(paste("kobs_lin", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") },
		content = function(file) {
			filename = paste(file,"xlsx",sep=".")
			saveWorkbook(save_results$wb, file = paste(paste("kobs_lin", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep="."), overwrite = TRUE)
			file.rename(paste(paste("kobs_lin", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep="."),file)
		}
	)

#	output$test = renderPrint({
#	})

	##########
	# Module output
	##########

	reactive_module_output = reactive({
		list("fit_results" = binding_constants$all_kobs_fit_statistics)
	})

	return(reactive_module_output)

}

