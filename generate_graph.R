source("complete_plot.R")

generate_graph = function(){
	if(is.null(output_overview_graph()$data_hist_df)){
		df = output_overview_graph()$data_frame_readin
	}
	else{
		df = output_overview_graph()$data_hist_df
	}
	# Read the uploaded file name
	file = output_overview_graph()$data
	filename = toupper(file$name)
	
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
