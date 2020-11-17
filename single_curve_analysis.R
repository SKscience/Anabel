#####################################################################################################################################################################
#####################################################################################################################################################################
#
#     _______. __  .__   __.   _______  __       _______      ______  __    __  .______     ____    ____  _______    
#    /       ||  | |  \ |  |  /  _____||  |     |   ____|    /      ||  |  |  | |   _  \    \   \  /   / |   ____|   
#   |   (----`|  | |   \|  | |  |  __  |  |     |  |__      |  ,----'|  |  |  | |  |_)  |    \   \/   /  |  |__      
#    \   \    |  | |  . `  | |  | |_ | |  |     |   __|     |  |     |  |  |  | |      /      \      /   |   __|     
#.----)   |   |  | |  |\   | |  |__| | |  `----.|  |____    |  `----.|  `--'  | |  |\  \----.  \    /    |  |____    
#|_______/    |__| |__| \__|  \______| |_______||_______|    \______| \______/  | _| `._____|   \__/     |_______|   
#                                                                                                                    
#     ___      .__   __.      ___       __      ____    ____  _______. __       _______.                             
#    /   \     |  \ |  |     /   \     |  |     \   \  /   / /       ||  |     /       |                             
#   /  ^  \    |   \|  |    /  ^  \    |  |      \   \/   / |   (----`|  |    |   (----`                             
#  /  /_\  \   |  . `  |   /  /_\  \   |  |       \_    _/   \   \    |  |     \   \                                 
# /  _____  \  |  |\   |  /  _____  \  |  `----.    |  | .----)   |   |  | .----)   |                                
#/__/     \__\ |__| \__| /__/     \__\ |_______|    |__| |_______/    |__| |_______/       
#
#####################################################################################################################################################################
#####################################################################################################################################################################


source("overview_graph.R")
source("zoom_and_fit.R")

# Loading dependent R functions

source("complete_plot.R")

single_curve_analysis_UI = function(id){
	ns = NS(id)
	tabPanel("Evaluation method 2: Single Curve Analysis",
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
		h1("single curve analysis")
		),
		overview_graph_UI(ns("single_overview_graph")),
		zoom_and_fit_UI(ns("single_zoom_and_fit")),

	#++++++++++++++++++++
	#####################
	# ALL OTHER UI
	#####################
	#++++++++++++++++++++
		# Buttons to Yank temporarry results
		column(width = 6, align="center", class = "well",
			   actionButton(ns("sca_yank_ass"),"Yank association fit")
			   ),
		column(width = 6, align="center", class = "well",
			   actionButton(ns("sca_yank_diss"),"Yank dissociation fit")
			   ),
		# Tables to show Temp results
		column(width = 6, align = "center",
			   tableOutput(outputId=ns("sca_ass_table"))
			   ),
		column(width = 6, align = "center",
			   tableOutput(outputId=ns("sca_diss_table"))
			   ),
		# Concentration
		column(width = 12, align="center", class = "well",
			textInput(inputId = ns("sca_reagent_concentration"),label = "Reagent Concentration", value = ""), 
			radioButtons(inputId=ns("sca_concentration"),choices=list("nM","µM","mM","M"),inline=T,label="Reagent concentration"),
			# Button to save the analysis
			actionButton(ns("sca_save_results"),"Analyse yanked fit results")
		),
		column(width = 12, align="center",
			tableOutput(outputId=ns("sca_results_table"))
			),
		column(width=12, align="center", class="well",
			actionButton(ns("generate_download_file"), "Generate Result File"),
			downloadButton(outputId = ns("sca_download_result_file"), label="Download Result File")
			)
		# test output
#		column(width=12,class="well",
#			   verbatimTextOutput(outputId=ns("test"))
#			   )
	)
	)
}

single_curve_analysis = function(input, output, session){

	#++++++++++++++++++++
	#####################
	# LOADING OVERVIEW GRAPH MOD
	#####################
	#++++++++++++++++++++

	output_overview_graph = callModule(overview_graph,"single_overview_graph")

	#++++++++++++++++++++
	#####################
	# LOADING ZOOM AND FIT MOD
	#####################
	#++++++++++++++++++++
	
	output_zoom_and_fit = callModule(zoom_and_fit,"single_zoom_and_fit", output_overview_graph)

	#++++++++++++++++++++
	#####################
	# MODULE SPECIFIC CODE
	#####################
	#++++++++++++++++++++

	generate_graph = function(){
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
		plot = complete_plot(df,output_overview_graph()$log_file_relevant,output_overview_graph()$plot_ymax,output_overview_graph()$plot_ymin, output_overview_graph()$dead_volume)
		
		return(plot)
	}
	
	
	sca_save_results = reactiveValues()

	# Values for summery sheet
	sca_save_results$overview_graphs = list()
	sca_save_results$all_used_datasets = list()
	sca_save_results$ass_fit_regions = data.frame()
	sca_save_results$diss_fit_regions = data.frame()
	# Values for fits sheet
	sca_save_results$ass_fit_graphs = list()
	sca_save_results$diss_fit_graphs = list()
	sca_save_results$ass_fit_results = list()
	sca_save_results$diss_fit_results = list()
	sca_save_results$ass_pg1 = list()
	sca_save_results$diss_pg1 = list()
	sca_save_results$ass_pg2 = list()
	sca_save_results$diss_pg2 = list()
	sca_save_results$ass_pg3 = list()
	sca_save_results$diss_pg3 = list()
	sca_save_results$complete_ass_graphs = list()
	sca_save_results$complete_diss_graphs = list()
	sca_save_results$delete_images = c()
	
	test_zone = reactiveValues()

	##########
	# Yank Fit statistics for assosiation
	##########
	
	sca_table = reactiveValues()

	observeEvent(input$sca_yank_ass,{
		if(!is.null(output_zoom_and_fit()$model_results)){
			if(!is.null(output_zoom_and_fit()$analyse_brush)){
				temp = data.frame()
				x = output_zoom_and_fit()$model_results
				x = x[,2:8]
				sca_table$ass = x

				# Save relevent plots and tables for the download file
				
				sca_save_results$ass_fit_results_temp = x
				sca_save_results$ass_fit_graph_temp = output_zoom_and_fit()$fit_graphs_temp
				sca_save_results$ass_analyse_brush = output_zoom_and_fit()$analyse_brush
				sca_save_results$complete_ass_graph_temp = generate_graph()
				
				# Save analysis plots if calculated
				if(output_zoom_and_fit()$show_hide_curve_analysis == "Show"){
					sca_save_results$ass_pg1_temp = output_zoom_and_fit()$pg1_temp
					sca_save_results$ass_pg2_temp = output_zoom_and_fit()$pg2_temp
					sca_save_results$ass_pg3_temp = output_zoom_and_fit()$pg3_temp
				}
				else{
					sca_save_results$ass_pg1_temp = NA
					sca_save_results$ass_pg2_temp = NA
					sca_save_results$ass_pg3_temp = NA
				}
			}
		}
	})

	##########
	# Output current ass statistics
	##########
	output$sca_ass_table = renderTable(digits=-3,{
		if(length(sca_table$ass) != 0){
			sca_table$ass
		}
	})

	##########
	# Yank Fit statistics for dissosiation
	##########
	
	observeEvent(input$sca_yank_diss,{
		if(!is.null(output_zoom_and_fit()$model_results)){
			if(!is.null(output_zoom_and_fit()$analyse_brush)){
				temp = data.frame()
				x = output_zoom_and_fit()$model_results
				x = x[,2:8]
				names(x) = c("Spot","A0","StErr(A0)","kdiss","StErr(kdiss)","y0","StErr(y0)")
				sca_table$diss = x
				
				sca_save_results$diss_fit_results_temp = x
				sca_save_results$diss_fit_graph_temp = output_zoom_and_fit()$fit_graphs_temp
				sca_save_results$diss_analyse_brush = output_zoom_and_fit()$analyse_brush
				sca_save_results$complete_diss_graph_temp = generate_graph()
				
				if(output_zoom_and_fit()$show_hide_curve_analysis == "Show"){	
					sca_save_results$diss_pg1_temp = output_zoom_and_fit()$pg1_temp
					sca_save_results$diss_pg2_temp = output_zoom_and_fit()$pg2_temp
					sca_save_results$diss_pg3_temp = output_zoom_and_fit()$pg3_temp
				}
				else{
					sca_save_results$diss_pg1_temp = NA
					sca_save_results$diss_pg2_temp = NA
					sca_save_results$diss_pg3_temp = NA
				}
			}
		}
	})
	
	##########
	# Output current diss statistics
	##########

	output$sca_diss_table = renderTable(digits=-3,{
		if(length(sca_table$diss) != 0){
			sca_table$diss
		}
	})

	##########
	# Do analysis and save results
	##########
	
	observeEvent(input$sca_save_results,{
		if(input$sca_reagent_concentration != ""){
			if(input$sca_concentration == "µM"){
				reagent_concentration = as.numeric(input$sca_reagent_concentration)/1000000
			}
			else if(input$sca_concentration == "mM"){
				reagent_concentration = as.numeric(input$sca_reagent_concentration)/1000
			}
			else if(input$sca_concentration == "M"){
				reagent_concentration = as.numeric(input$sca_reagent_concentration)
			}
			else if(input$sca_concentration =="nM"){
				reagent_concentration = as.numeric(input$sca_reagent_concentration)/1000000000
			}
		}

		if((length(sca_table$diss) != 0) & (length(sca_table$ass) != 0) & (input$sca_reagent_concentration != "")){
			temp = data.frame()
			ass = sca_table$ass
			diss = sca_table$diss
			if(nrow(ass) > nrow(diss)){
				for(i in 1:nrow(ass)){
					cur_ass = ass[i,]
					cur_ass = droplevels(cur_ass)
					cur_diss = diss[which( diss[,1] == ass[i,1]),]
					cur_diss = droplevels(cur_diss)

					if(nrow(cur_diss) != 0){
						# adding Spot
						temp[i,1] = cur_ass[,1]
						# adding the provided reagent concentration
						temp[i,2] = reagent_concentration
						# Adding kobs
						temp[i,3] = cur_ass[,4]
						# Adding StErr kobs
						temp[i,4] = cur_ass[,5]
						# Adding k diss
						temp[i,5] = cur_diss[,4]
						# Adding StErr kdiss
						temp[i,6] = cur_diss[,5]
						# Calculating and adding kass
						temp[i,7] = (temp[i,3]-temp[i,5])/reagent_concentration
						# Calculating and adding StErr kass
						temp[i,8] = (1/reagent_concentration) * sqrt((cur_ass[,5]^2) + (cur_diss[,5]^2))
						# Calculating and adding Kd
						temp[i,9] = temp[i,5] / temp[i,7]
						# Calculating and adding StErr kd
						temp[i,10] = temp[i,9] * sqrt((((cur_diss[,5])^2)/((cur_diss[,4])^2)) + (((temp[i,8])^2) / ((temp[i,7])^2)))
					}
				}
			}
			else {
				for(i in 1:nrow(diss)){
					cur_diss = diss[i,]
					cur_diss = droplevels(cur_diss)
					cur_ass = ass[which( ass[,1] == diss[i,1]),]
					cur_ass = droplevels(cur_ass)

					if(nrow(cur_ass) != 0){
						# adding Spot
						temp[i,1] = cur_ass[,1]
						# adding the provided reagent concentration
						temp[i,2] = reagent_concentration
						# Adding kobs
						temp[i,3] = cur_ass[,4]
						# Adding StErr kobs
						temp[i,4] = cur_ass[,5]
						# Adding k diss
						temp[i,5] = cur_diss[,4]
						# Adding StErr kdiss
						temp[i,6] = cur_diss[,5]
						# Calculating and adding kass
						temp[i,7] = (temp[i,3]-temp[i,5])/reagent_concentration
						# Calculating and adding StErr kass
						temp[i,8] = (1/reagent_concentration) * sqrt((cur_ass[,5]^2) + (cur_diss[,5]^2))
						# Calculating and adding Kd
						temp[i,9] = temp[i,5] / temp[i,7]
						# Calculating and adding StErr kd
						temp[i,10] = temp[i,9] * sqrt((((cur_diss[,5])^2)/((cur_diss[,4])^2)) + (((temp[i,8])^2) / ((temp[i,7])^2)))
					}
				}
			}

			temp = na.omit(temp)


			# Altering Names
			names(temp) = c("Spot","c(Reagent) [M]","kobs","StErr[Kobs]","kdiss","StErr[kdiss]","kass [1/M]","StErr(kass) [1/M]","KD [M]","StErr(KD) [M]")
			
			sca_table$all_results = rbind(sca_table$all_results,temp)

			plot = generate_graph()

			filename = output_overview_graph()$data_frame_readin_filename
			
			# Store the plot and the corresponding filename
			if(length(sca_save_results$overview_graphs) == 0){
				sca_save_results$overview_graphs[[1]] = plot
				sca_save_results$all_used_datasets[[1]] = filename
			}
			else if(sca_save_results$all_used_datasets[[length(sca_save_results$all_used_datasets)]] != filename){
				sca_save_results$overview_graphs[[length(sca_save_results$overview_graphs)+1]] = plot
				sca_save_results$all_used_datasets[[length(sca_save_results$all_used_datasets)+1]] = filename
			}

			###
			# Save the fit results for the assosiation fit
			###
			# Save all fit graphs and fit results
			if(length(sca_save_results$ass_fit_results) == 0){
				sca_save_results$ass_fit_graphs[[1]] = sca_save_results$ass_fit_graph_temp
				sca_save_results$complete_ass_graph[[1]] = sca_save_results$complete_ass_graph_temp
				sca_save_results$ass_fit_results[[1]] = sca_save_results$ass_fit_results_temp

				###
				# Save the coordinates of the chosen fitting area
				fitting_area = sca_save_results$ass_analyse_brush
				sca_save_results$ass_fit_regions[1,1] = fitting_area$xmin
				sca_save_results$ass_fit_regions[1,2] = fitting_area$xmax
				sca_save_results$ass_fit_regions[1,3] = fitting_area$ymin
				sca_save_results$ass_fit_regions[1,4] = fitting_area$ymax
				# Save the graph number to which the region belongs
				sca_save_results$ass_fit_regions[1,5] = length(sca_save_results$all_used_datasets)
			}
			else if(identical(sca_save_results$ass_fit_results[[length(sca_save_results$ass_fit_results)]],sca_save_results$ass_fit_results_temp) == FALSE){
				sca_save_results$ass_fit_graphs[[length(sca_save_results$ass_fit_graphs)+1]] = sca_save_results$ass_fit_graph_temp
				sca_save_results$complete_ass_graph[[length(sca_save_results$complete_ass_graph)+1]] = sca_save_results$complete_ass_graph_temp
				sca_save_results$ass_fit_results[[length(sca_save_results$ass_fit_results)+1]] = sca_save_results$ass_fit_results_temp

				###
				# Save the coordinates of the chosen fitting area
				fitting_area = sca_save_results$ass_analyse_brush
				row_number = nrow(sca_save_results$ass_fit_regions)+1
				sca_save_results$ass_fit_regions[row_number,1] = fitting_area$xmin
				sca_save_results$ass_fit_regions[row_number,2] = fitting_area$xmax
				sca_save_results$ass_fit_regions[row_number,3] = fitting_area$ymin
				sca_save_results$ass_fit_regions[row_number,4] = fitting_area$ymax
				# Save the graph number to which the region belongs
				sca_save_results$ass_fit_regions[row_number,5] = length(sca_save_results$all_used_datasets)
			}

			if(length(sca_save_results$ass_pg1) == 0){
				sca_save_results$ass_pg1[[1]] = sca_save_results$ass_pg1_temp
				sca_save_results$ass_pg2[[1]] = sca_save_results$ass_pg2_temp
				sca_save_results$ass_pg3[[1]] = sca_save_results$ass_pg3_temp
			}
			else{
				l = length(sca_save_results$ass_pg1) +1
				sca_save_results$ass_pg1[[l]] = sca_save_results$ass_pg1_temp
				sca_save_results$ass_pg2[[l]] = sca_save_results$ass_pg2_temp
				sca_save_results$ass_pg3[[l]] = sca_save_results$ass_pg3_temp
			}

			###
			# Save the fit results for the dissosiation fit
			###
			# Save all fit graphs and fit results
			if(length(sca_save_results$diss_fit_results) == 0){
				sca_save_results$diss_fit_graphs[[1]] = sca_save_results$diss_fit_graph_temp
				sca_save_results$complete_diss_graph[[1]] = sca_save_results$complete_diss_graph_temp
				sca_save_results$diss_fit_results[[1]] = sca_save_results$diss_fit_results_temp

				###
				# Save the coordinates of the chosen fitting area
				fitting_area = sca_save_results$diss_analyse_brush
				sca_save_results$diss_fit_regions[1,1] = fitting_area$xmin
				sca_save_results$diss_fit_regions[1,2] = fitting_area$xmax
				sca_save_results$diss_fit_regions[1,3] = fitting_area$ymin
				sca_save_results$diss_fit_regions[1,4] = fitting_area$ymax
				# Save the graph number to which the region belongs
				sca_save_results$diss_fit_regions[1,5] = length(sca_save_results$all_used_datasets)
			}
			else if(identical(sca_save_results$diss_fit_results[[length(sca_save_results$diss_fit_results)]],sca_save_results$diss_fit_results_temp) == FALSE){
				sca_save_results$diss_fit_graphs[[length(sca_save_results$diss_fit_graphs)+1]] = sca_save_results$diss_fit_graph_temp
				sca_save_results$complete_diss_graph[[length(sca_save_results$complete_diss_graph)+1]] = sca_save_results$complete_diss_graph_temp
				sca_save_results$diss_fit_results[[length(sca_save_results$diss_fit_results)+1]] = sca_save_results$diss_fit_results_temp

				###
				# Save the coordinates of the chosen fitting area
				fitting_area = sca_save_results$diss_analyse_brush
				row_number = nrow(sca_save_results$diss_fit_regions)+1
				sca_save_results$diss_fit_regions[row_number,1] = fitting_area$xmin
				sca_save_results$diss_fit_regions[row_number,2] = fitting_area$xmax
				sca_save_results$diss_fit_regions[row_number,3] = fitting_area$ymin
				sca_save_results$diss_fit_regions[row_number,4] = fitting_area$ymax
				# Save the graph number to which the region belongs
				sca_save_results$diss_fit_regions[row_number,5] = length(sca_save_results$all_used_datasets)
			}

			if(length(sca_save_results$diss_pg1) == 0){
				sca_save_results$diss_pg1[[1]] = sca_save_results$diss_pg1_temp
				sca_save_results$diss_pg2[[1]] = sca_save_results$diss_pg2_temp
				sca_save_results$diss_pg3[[1]] = sca_save_results$diss_pg3_temp
			}
			else{
				l = length(sca_save_results$diss_pg1) +1
				sca_save_results$diss_pg1[[l]] = sca_save_results$diss_pg1_temp
				sca_save_results$diss_pg2[[l]] = sca_save_results$diss_pg2_temp
				sca_save_results$diss_pg3[[l]] = sca_save_results$diss_pg3_temp
			}
			
		}
	})
	
	##########
	# Show Table with all results
	##########

	output$sca_results_table = renderTable(digits=-3,{
		sca_table$all_results
	})

	##########
	# Generate Download File
	##########

	observeEvent(input$generate_download_file,{
		
		# Create a Progress object
		progress <- shiny::Progress$new()

		progress$set(message = "Picking Flowers", value = 0)
		on.exit(progress$close())
	
		progress$inc(1/4)
		wb <- createWorkbook()
		#Create the summery Sheet
		addWorksheet(wb, "summery")
		for(entry in 1:length(sca_save_results$overview_graphs)){
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
			p = sca_save_results$overview_graphs[[entry]]

			complete_plot_name = paste0("complete_plot",session$token,as.character(entry),".png")
			ggsave(complete_plot_name, plot = p, device = "png", width=50, units = c("cm"))
			insertImage(wb, "summery", complete_plot_name, startRow = position_start, startCol = 2, width = 48.61,height=17.46,units="cm")
			writeData(wb,"summery",sca_save_results$all_used_datasets[[entry]],startRow=name_position, startCol=2)
		}

		progress$inc(1/4)
		progress$set(message = "Counting stars (may take a while...)")	
		
		
		#Create and write the assosiation Fits Sheet
		addWorksheet(wb, "association_fits")
		for(entry in 1:length(sca_save_results$ass_fit_graphs)){
			if(entry == 1){
				fits_line_start = 2
			}
			else{
				if(nrow(sca_save_results$ass_fit_results[[entry-1]]) > 41){
					fits_line_start = fits_line_start + nrow(sca_save_results$ass_fit_results[[entry-1]]) + 3
				}
				else{
					fits_line_start = fits_line_start + 49
				}
			}
			
			fit_plot_ass_name = paste0("fit_plot_ass",session$token,as.character(entry),".png")
			ggsave(fit_plot_ass_name, plot= sca_save_results$ass_fit_graphs[[entry]], device = "png", width=25, units = c("cm"))
			if(!is.na(sca_save_results$ass_pg1[entry])){
				pg1_plot_name = paste0("pg1_plot",session$token,as.character(entry),".png")
				ggsave(pg1_plot_name, plot=sca_save_results$ass_pg1[[entry]], device = "png", width=25, units = c("cm"))
				
				pg2_plot_name = paste0("pg2_plot",session$token,as.character(entry),".png")
				ggsave(pg2_plot_name, plot=sca_save_results$ass_pg2[[entry]], device = "png", width=25, units = c("cm"))
				
				pg3_plot_name = paste0("pg3_plot",session$token,as.character(entry),".png")
				ggsave(pg3_plot_name, plot=sca_save_results$ass_pg3[[entry]], device = "png", width=25, units = c("cm"))
			}

			# Generate and create complete plot with fitting regions
			p = sca_save_results$complete_ass_graph[[entry]]
			p = p + geom_rect(data=sca_save_results$ass_fit_regions[entry,], inherit.aes=FALSE, aes(xmin=V1, xmax=V2, ymin=V3, ymax=V4), alpha=0, fill="blue", color="black", size=0.3)
			fit_number = paste("#",entry,"Ass",sep="")
			p = p + annotate("text",x=sca_save_results$ass_fit_regions[entry,1], y=sca_save_results$ass_fit_regions[entry,4], label=fit_number)
			
			complete_ass_plot_name = paste0("complete_ass_plot",session$token,as.character(entry),".png")			
			ggsave(complete_ass_plot_name, plot= p, device = "png", width=35, units = c("cm"))

			#Fit plot
			insertImage(wb,"association_fits",fit_plot_ass_name, startRow=fits_line_start, startCol=2,width=14.96,height=12,7,units="cm")			
			#Fit number
			writeData(wb,"association_fits",paste("#",entry,sep=""),startRow=fits_line_start, startCol=1)			
			#Fit results
			writeData(wb,"association_fits",sca_save_results$ass_fit_results[[entry]],startRow=fits_line_start, startCol=11)			

			#Fit regions
			fit_regions = sca_save_results$ass_fit_regions[entry,1:4]
			names(fit_regions) = c("xmin","xmax","ymin","ymax")
			region_title = "Chosen fitting range:"
			writeData(wb,"association_fits",region_title,startRow=fits_line_start + 25, startCol=2)
			writeData(wb,"association_fits",fit_regions,startRow=fits_line_start + 26, startCol=2)
			
			if(!is.na(sca_save_results$ass_pg1[entry])){
			# Curve progression plots
			insertImage(wb,"association_fits",pg1_plot_name, startRow=fits_line_start, startCol=27,width=13.09,height=12.7,units="cm")
			insertImage(wb,"association_fits",pg2_plot_name, startRow=fits_line_start, startCol=35,width=14.96,height=12.7,units="cm")
			insertImage(wb,"association_fits",pg3_plot_name, startRow=fits_line_start, startCol=44,width=14.96,height=12.7,units="cm")
			}

			# Add complete plot
			insertImage(wb,"association_fits",complete_ass_plot_name,startRow=fits_line_start + 29, startCol=2 ,width=14.97,height=6.88,units="cm")

		}


		progress$inc(1/4)
		progress$set(message = "Defeat the Evil")	

		#Create and write the dissosiation Fits Sheet
		addWorksheet(wb,"dissociation_fits")
		for(entry in 1:length(sca_save_results$diss_fit_graphs)){
			if(entry == 1){
				fits_line_start = 2
			}
			else{
				if(nrow(sca_save_results$diss_fit_results[[entry-1]]) > 41){
					fits_line_start = fits_line_start + nrow(sca_save_results$diss_fit_results[[entry-1]]) + 3
				}
				else{
					fits_line_start = fits_line_start + 49
				}
			}
			fit_plot_name = paste0("fit_diss_plot",session$token,as.character(entry),".png")	
			ggsave(fit_plot_name, plot= sca_save_results$diss_fit_graphs[[entry]], device = "png", width=25, units = c("cm"))
			
			if(!is.na(sca_save_results$diss_pg1[entry])){
				pg1_plot_name = paste0("pg1_diss_plot",session$token,as.character(entry),".png")	
				ggsave(pg1_plot_name, plot=sca_save_results$diss_pg1[[entry]], device = "png", width=25, units = c("cm"))
			
				pg2_plot_name = paste0("pg2_diss_plot",session$token,as.character(entry),".png")	
				ggsave(pg2_plot_name, plot=sca_save_results$diss_pg2[[entry]], device = "png", width=25, units = c("cm"))
			
				pg3_plot_name = paste0("pg3_diss_plot",session$token,as.character(entry),".png")	
				ggsave(pg3_plot_name, plot=sca_save_results$diss_pg3[[entry]], device = "png", width=25, units = c("cm"))
			}

			# Generate and create complete plot with fitting regions
			p = sca_save_results$complete_diss_graph[[entry]]
			p = p + geom_rect(data=sca_save_results$diss_fit_regions[entry,], inherit.aes=FALSE, aes(xmin=V1, xmax=V2, ymin=V3, ymax=V4), alpha=0, fill="blue", color="black", size=0.3)
			fit_number = paste("#",entry,"Diss",sep="")
			p = p + annotate("text",x=sca_save_results$diss_fit_regions[entry,1], y=sca_save_results$diss_fit_regions[entry,4], label=fit_number)
			
			complete_diss_plot_name = paste0("complete_diss_plot",session$token,as.character(entry),".png")	
			ggsave(complete_diss_plot_name, plot= p, device = "png", width=35, units = c("cm"))

			#Fit plot
			insertImage(wb,"dissociation_fits",fit_plot_name, startRow=fits_line_start, startCol=2,width=14.96,height=12,7,units="cm")			
			#Fit number
			writeData(wb,"dissociation_fits",paste("#",entry,sep=""),startRow=fits_line_start, startCol=1)						
			#Fit results
			writeData(wb,"dissociation_fits",sca_save_results$diss_fit_results[[entry]],startRow=fits_line_start, startCol=11)						

			#Fit regions
			fit_regions = sca_save_results$diss_fit_regions[entry,1:4]
			names(fit_regions) = c("xmin","xmax","ymin","ymax")
			region_title = "Chosen fitting range:"
			writeData(wb,"dissociation_fits",region_title,startRow=fits_line_start + 25, startCol=2)
			writeData(wb,"dissociation_fits",fit_regions,startRow=fits_line_start + 26, startCol=2)			

			if(!is.na(sca_save_results$diss_pg1[entry])){
			# Curve progression plots
			insertImage(wb,"dissociation_fits",pg1_plot_name, startRow=fits_line_start, startCol=27,width=13.09,height=12.7,units="cm")
			insertImage(wb,"dissociation_fits",pg2_plot_name, startRow=fits_line_start, startCol=35,width=14.96,height=12.7,units="cm")
			insertImage(wb,"dissociation_fits",pg3_plot_name, startRow=fits_line_start, startCol=44,width=14.96,height=12.7,units="cm")
			}
			# Add complete plot
			insertImage(wb,"dissociation_fits",complete_diss_plot_name,startRow=fits_line_start + 29, startCol=2 ,width=14.97,height=6.88,units="cm")			
		}


		progress$inc(1/4)
		progress$set(message = "You Are Welcome!")
		
		#Create the Sheet containing all the results
		addWorksheet(wb,"all_binding_constants")
		writeData(wb,"all_binding_constants",sca_table$all_results,,startRow=1, startCol=2)

		sca_save_results$wb = wb
	})

	##########
	# Save result file
	##########
	output$sca_download_result_file <- downloadHandler(
		filename = function() { paste(paste("sca", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") },
		content = function(file) {
			
			filename = paste(file,"xlsx",sep=".")
			saveWorkbook(sca_save_results$wb, file = paste(paste("sca", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") , overwrite = TRUE)
			file.rename(paste(paste("sca", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") ,file)
		})

#	output$test <- renderPrint({
#		print(test_zone$test)
#	})

	##########
	# Module output
	##########

	reactive_module_output = reactive({
		list("fit_results" = sca_table$all_results)
	})

	return(reactive_module_output)

}

