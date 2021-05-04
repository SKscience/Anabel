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
				column(width = 3,align="center",
					   numericInput(inputId = ns("tba_range"), "Number of average points:", 10)
					   ),
				column(width = 3, align="center",
					radioButtons(inputId=ns("tba_graphs"), choices=list("Boxplot","Violin Plot", "None"), label = "Include Graph:", inline=TRUE)
					),
				column(width = 3, align="center",
					   downloadButton(outputId = ns("download_tba"), label = "Analyse and Download")
					   ),
				column(width = 3, align="center",
					   downloadButton(outputId = ns("download_tba_graph"), label = "Download Graph only")
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


	output$tba_test = renderTable({
		brush = output_overview_graph()$plot_brush
		mdf = brushedPoints(output_overview_graph()$data_frame, brush)
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
		tba$delta = tba$Mean_after - tba$Mean_before
		tba
	})



	##########
	# Perform and Download TBA
	##########

	output$download_tba = downloadHandler(
		filename = function() { paste(paste(output_overview_graph()$data_frame_readin_filename, "TBA", sep="_"),"xlsx",sep=".") },
		content = function(file) {
			brush = output_overview_graph()$plot_brush
			if(!is.null(brush)){
				# getting the selected points using the brushedPoints method
				mdf = brushedPoints(output_overview_graph()$data_frame, brush)
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
				tba$delta = tba$Mean_after - tba$Mean_before
				tba$STD_delta = sqrt(tba$STD_before^2 + tba$STD_after^2)
				wb <- createWorkbook()
				addWorksheet(wb, "TBA")
				writeData(wb,"TBA",tba,startRow=1, startCol=1)
				if(input$tba_graphs != "None"){
					tba$group = gsub("\\.\\d+", "", tba$Name)
					if(input$tba_graphs == "Boxplot"){
						p = ggplot(tba, aes(x=group, y=delta, colour=group)) + geom_boxplot() + theme_cowplot()
					}
					else{
						p = ggplot(tba, aes(x=group, y=delta, colour=group)) + geom_violin() + geom_boxplot(width=0.05) + theme_cowplot()
					}
					tba_plot_name = paste0("tba_plot",session$token,".png")	
					ggsave(tba_plot_name, plot= p, device = "png", width=50, units = c("cm"))
					addWorksheet(wb, "TBA_Plot")
					insertImage(wb,"TBA_Plot",tba_plot_name, startRow=1, startCol=2,width=50,height=17.8 , units="cm")
				}
			filename = paste(file,"xlsx",sep=".")
			saveWorkbook(wb, file = paste(paste("tba", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") , overwrite = TRUE)
			file.rename(paste(paste("tba", output_overview_graph()$data_frame_readin_filename, sep="_"),"xlsx",sep=".") ,file)
			}
		}
	)

	##########
	# Download Graph only
	##########

	output$download_tba_graph = downloadHandler(
		filename = function() { paste(paste(output_overview_graph()$data_frame_readin_filename, "TBA", "graph", sep="_"),"pdf",sep=".") },
		content = function(file) {
			brush = output_overview_graph()$plot_brush
			if(!is.null(brush)){
				# getting the selected points using the brushedPoints method
				mdf = brushedPoints(output_overview_graph()$data_frame, brush)
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
				tba$delta = tba$Mean_after - tba$Mean_before
				tba$STD_delta = sqrt(tba$STD_before^2 + tba$STD_after^2)
				if(input$tba_graphs != "None"){
					tba$group = gsub("\\.\\d+", "", tba$Name)
					if(input$tba_graphs == "Boxplot"){
						p = ggplot(tba, aes(x=group, y=delta, colour=group)) + geom_boxplot() + theme_cowplot()
					}
					else{
						p = ggplot(tba, aes(x=group, y=delta, colour=group)) + geom_violin() + geom_boxplot(width=0.05) + theme_cowplot()
					}
				}
			ggsave(file, plot= p, device = "pdf", width=50, units = c("cm"))
			}
		}
	)
	


}
