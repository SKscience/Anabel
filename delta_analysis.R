#####################################################################################################################################################################
#####################################################################################################################################################################
# _______   _______  __      .___________.    ___              ___      .__   __.      ___       __      ____    ____  _______. __       _______.
#|       \ |   ____||  |     |           |   /   \            /   \     |  \ |  |     /   \     |  |     \   \  /   / /       ||  |     /       |
#|  .--.  ||  |__   |  |     `---|  |----`  /  ^  \          /  ^  \    |   \|  |    /  ^  \    |  |      \   \/   / |   (----`|  |    |   (----`
#|  |  |  ||   __|  |  |         |  |      /  /_\  \        /  /_\  \   |  . `  |   /  /_\  \   |  |       \_    _/   \   \    |  |     \   \    
#|  '--'  ||  |____ |  `----.    |  |     /  _____  \      /  _____  \  |  |\   |  /  _____  \  |  `----.    |  | .----)   |   |  | .----)   |   
#|_______/ |_______||_______|    |__|    /__/     \__\    /__/     \__\ |__| \__| /__/     \__\ |_______|    |__| |_______/    |__| |_______/    
#
#####################################################################################################################################################################
#####################################################################################################################################################################                                    

##################################################################################################################################################################### 
# UI
##################################################################################################################################################################### 

tba_UI = function(id){
	ns = NS(id)
	fluidPage(
		column(width=12, align="center", class = "well",
			   radioButtons(inputId=ns("show_hide_TBA"), choices=list("Hide","Show"), label = "Show or hide menue for total binding analysis", inline=TRUE)
			   ),
		conditionalPanel(
			condition = paste0("input['" , ns("show_hide_TBA"), "'] == 'Show'"),
	# Zoomed Graph panel
				column(width = 6,align="center",
					   numericInput(inputId = ns("tba_range"), "Number of average points:", 10),
					   column( width=6, align="center",
							  textInput(inputId=ns("t_before"), "Timepoint before binding:", value = "Selection")
							  ),
					   column(width=6, align="center",
							  textInput(inputId=ns("t_after"), "Timepoint after binding:", value = "Selection")
							  )
					   ),
				column(width = 6, align="center",
					   radioButtons(inputId=ns("tba_download_type"), choices=list("Complete","Plots only"), label = "Download:", inline=TRUE),
					   downloadButton(outputId = ns("download_tba"), label = "Analyse and Download")
					   ),
				column(width = 12, style='padding-bottom:20px',
			   			h4("TBA Results"),
			   			DT::dataTableOutput(outputId=ns("TBA_results"))
			   )
			)
	)
}

##################################################################################################################################################################### 
# SERVER
##################################################################################################################################################################### 

tba = function(input, output, session, output_overview_graph){


	generate_graph = function(){
		if(is.null(output_overview_graph()$data_hist_df)){
			df = output_overview_graph()$data_frame_readin
		}
		else{
			df = output_overview_graph()$data_hist_df
		}
		# Read the uploaded file name
		filename = output_overview_graph()$data_frame_readin_filename
#		if(!(grepl("BIACORE",filename) | grepl("OCTET",filename))){
#			# Update the time range according to slider input
#			time_min = as.numeric(output_overview_graph()$time_range[1])
#			time_max = as.numeric(output_overview_graph()$time_range[2])
#
#			df = df[which(df[[1]] >time_min & df[[1]] <time_max),]	
#		}
		###
		#Save overview Graphs and file names
		# generate the overview plot
		plot = complete_plot(df,output_overview_graph()$log_file_relevant,output_overview_graph()$plot_ymax,output_overview_graph()$plot_ymin, output_overview_graph()$dead_volume)
		
		return(plot)
	}

	##########
	# Perform and Download TBA
	##########

	output$download_tba = downloadHandler(
		filename = function() {
			if(input$tba_download_type=="Complete"){
				paste(paste(output_overview_graph()$data_frame_readin_filename, "TBA", sep="_"),"xlsx",sep=".")
			}
			else{
				paste(paste(output_overview_graph()$data_frame_readin_filename, "TBA", "graph", sep="_"),"pdf",sep=".")
			}
		},
		content = function(file) {
	
			brush = output_overview_graph()$plot_brush
			
			if(suppressWarnings(!is.na(as.numeric(input$t_before))) & suppressWarnings(!is.na(as.numeric(input$t_after) != "Default"))){
				mdf = output_overview_graph()$data_frame_readin
				mdf = mdf[which(mdf$Time>=as.numeric(input$t_before) & mdf$Time <= as.numeric(input$t_after)),]
			}
			
			else if(!is.null(brush)){
				# getting the selected points using the brushedPoints method
				mdf = brushedPoints(output_overview_graph()$data_frame, brush)
			}
			before = mdf[which(mdf$Time <= min(mdf$Time) + input$tba_range ),]
			after = mdf[which(mdf$Time >= max(mdf$Time) - input$tba_range ),]
			before_mean = aggregate(value~variable,before,mean)
			before_std = aggregate(value~variable,before,sd)
			before = merge(before_mean, before_std, by="variable")
			after_mean = aggregate(value~variable,after,mean)
			after_std = aggregate(value~variable,after,sd)
			after = merge(after_mean, after_std, by="variable")

			tba = merge(before, after, by="variable")
			names(tba) = c("Name","Mean_before","STD_before","Mean_after","STD_after")
			tba$t_min = min(mdf$Time)
			tba$t_max = max(mdf$Time)
			tba$n_average_points = input$tba_range
			tba$delta = tba$Mean_after - tba$Mean_before
			tba$STD_delta = sqrt(tba$STD_before^2 + tba$STD_after^2)
			
			tba$group = gsub("\\.\\d+", "", tba$Name)
			box = ggplot(tba, aes(x=group, y=delta, colour=group)) + geom_boxplot() + theme_cowplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position = "none") 
			violin = suppressWarnings(ggplot(tba, aes(x=group, y=delta, colour=group)) + geom_violin() + geom_boxplot(width=0.05) + theme_cowplot()) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position = "none") 
			scatter = ggplot(tba, aes(x=group, y=delta, colour=group)) + geom_point() + theme_cowplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position = "none") 
			
			overview = generate_graph() + geom_vline(xintercept = c(min(mdf$Time),  min(mdf$Time) + input$tba_range, max(mdf$Time) - input$tba_range ,  max(mdf$Time)) )
			

			if(input$tba_download_type=="Complete"){
				wb <- createWorkbook()
				addWorksheet(wb, "TBA")
				writeData(wb,"TBA",tba,startRow=1, startCol=1)
				
				box_name = paste0("box",session$token,".png")
				violin_name = paste0("violin",session$token,".png")
				scatter_name = paste0("scatter",session$token,".png")
				overview_name = paste0("overview",session$token,".png")
				ggsave(box_name, plot= box, device = "png", width=50, units = c("cm"))
				ggsave(violin_name, plot= violin, device = "png", width=50, units = c("cm"))
				ggsave(scatter_name, plot= scatter, device = "png", width=50, units = c("cm"))
				ggsave(overview_name, plot= overview, device = "png", width=50, units = c("cm"))
				
				addWorksheet(wb, "Box_Plot")
				insertImage(wb,"Box_Plot",box_name, startRow=1, startCol=2,width=50,height=17.8 , units="cm")
				addWorksheet(wb, "Violin_Plot")
				insertImage(wb,"Violin_Plot",violin_name, startRow=1, startCol=2,width=50,height=17.8 , units="cm")
				addWorksheet(wb, "Scatter_Plot")
				insertImage(wb,"Scatter_Plot",scatter_name, startRow=1, startCol=2,width=50,height=17.8 , units="cm")
				addWorksheet(wb, "Overview_Plot")
				insertImage(wb,"Overview_Plot",overview_name, startRow=1, startCol=2,width=50,height=17.8 , units="cm")

				filename = paste(file,"xlsx",sep=".")
				saveWorkbook(wb, file = paste(paste("tba", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") , overwrite = TRUE)
				file.rename(paste(paste("tba", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") ,file)
			}
			else if(input$tba_download_type=="Plots only"){
			
				pdf(file)
				print(box)
				print(violin)
				print(scatter)
				dev.off()

#				ggsave(file, plot= violin, device = "pdf", width=50, units = c("cm"))
#			}
#			else if(input$tba_download_type=="Box Plot"){
#				ggsave(file, plot= box, device = "pdf", width=50, units = c("cm"))
#			}
#			else if(input$tba_download_type=="Scatter Plot"){
#				ggsave(file, plot= scatter, device = "pdf", width=50, units = c("cm"))
			}
			
		}
	)


}
