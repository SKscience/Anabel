##########
# Function to generate the first plot showing the whole time range 
#########
complete_plot = function(data_frame,log_file_relevant,input_plot_ymax,input_plot_ymin,input_dead_volume){

# Only execute when logfile is present
if(!is.null(log_file_relevant)){
	# get relevant variales from logfile
	reagent = as.data.frame(log_file_relevant[2:nrow(log_file_relevant),1])
	flow_rate = as.data.frame(log_file_relevant[2:nrow(log_file_relevant),2])
	priming_time = as.data.frame(log_file_relevant[2:nrow(log_file_relevant),3])
	# Read Dead volume from UI
	dead_volume = input_dead_volume
	###
	# Start calculating the plot areas
	###
	sec_counter = 0
	# Empty dataframe for storing the calculated boundaries
	boundaries = data.frame()
	# loop through every reagent used in the experiment
	for(step in 1:nrow(reagent)){
		# calculate the dead time of every step except for the last step
		if(step < nrow(reagent)){
			# additional time the reagent is inside the flowchamber until the next reagent comes in (although the next step already started)
			dead_time_actual_step = dead_volume / (as.numeric(as.character(flow_rate[step+1,1]))/60)
		}
		# If it is the last reagent step, set the dead time to 0
		else{
			dead_time_actual_step = 0
		}
		# If it is the first step, than the left and thereby the first boundary is set to 0 (only correct if the first step is a system buffer step, but thats usually the case)
		if(step == 1){
			left_boundary = 0
		}
		# For every other step the new left boundary is of course the right boundary from the previous step. right boundary is calculated underneath so naturally it still caries the value ffrom the previous for loop cycle
		else{
			left_boundary = right_boundary
		}
		# Calculate the right boundary for the actual step
		# Hereby, sec_counter represents the theoretical boundary without dead volume correction from the previous step.
		# the time of the present step gets added so that this will represent the actual theoretical boundary without correction for the actual step.
		# This boundary is now corrected by the dead_time_actual_step explained above. So the actual step takes longer that in theory caused by the dead volume. Of course, this value then becomes the new left boundary of the next step.
		right_boundary = sec_counter + as.numeric(as.character(priming_time[step,1])) + dead_time_actual_step
		# The calculated boundary is saved for later use
		boundaries[step,1] = right_boundary
		# Sec counter is prepared for the next step
		sec_counter = sec_counter + as.numeric(as.character(priming_time[step,1]))
	}
	
}
# Do not execute if nothing was uploaded
if(!is.null(data_frame)){
	# melt data frame for plotting
	mdf <- data_frame
	# remove not selected dfs from mdf
	mdf = droplevels(mdf)
	#Draw the basic plot using the uploaded data
	p = ggplot(data=mdf, aes(x=Time, y=value, group = variable, colour = variable)) + geom_line() + background_grid(major = "xy", minor = "xy") 
	# DO not show lagend with more then 20 selected datasets
	if(length(levels(mdf[,"variable"])) > 20){
		p = p + theme(legend.position="none")
	}
	# Do not execute this code if no log sheet is provided
	if(!is.null(log_file_relevant)){
		# Get the axis ranges from the drawn plot
		y_max = ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]
		y_min = ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]
		x_min = ggplot_build(p)$layout$panel_scales_x[[1]]$range$range[1]
		x_max = ggplot_build(p)$layout$panel_scales_x[[1]]$range$range[2]
		# Calculate the rectangle dimensions that are used to display the experiment steps (boundaries) that have been calculated above
		rectangle = data.frame()
		# Set the left side of the very left rectangle to -Infinitive to prevent a gap on the left side
		rectangle[1,1] = -Inf
		#Calculate rectangle indices from the boundaries indices calculated above (rectangle needs the coordinates of all four edges)
		for(i in 1:nrow(boundaries)){
			if( i%%2 == 0){
				rectangle[i+1,1] = boundaries[i,1]
			}
			if( i%%2 == 1){
				rectangle[i,2] = boundaries[i,1]
			}
		}
		
		rectangle = rectangle[rowSums(is.na(rectangle)) == 0,]
		rectangle[,3] = -Inf
		rectangle[,4] = Inf
#			rectangle[,5] = 1
#			rectangle[,6] = 1
		#Set the last rectangle to infinite when the max t value is smaller than the actual end of that rectangle
		suppressWarnings(
		if(rectangle[nrow(rectangle),2] >= max(mdf[,1])){
			rectangle[nrow(rectangle),2] = Inf
		}
		)
		# Calculate coordinates for text label
		text_lable = data.frame()
		suppressWarnings(
		for(i in 1:nrow(boundaries)){
			if(i == 1){
				if(min(mdf[,1]) > 0 & min(mdf[,1]) < boundaries[1,1]){
				   text_lable[1,1] = min(mdf[,1])
				}
				else if(min(mdf[,1])==0){
					text_lable[1,1] = 50
				}
			}
			else{
					text_lable[i,1] = boundaries[i-1,1]+50
			}
		}
		)
		shiny::validate(
			need(y_max,'')
			)
		if(input_plot_ymax == "default"){
			p = p + annotate(geom="text", x = text_lable[[1]], y =y_max , label = reagent[[1]], hjust = 0)
		}
		else{
			p = p + annotate(geom="text", x = text_lable[[1]], y = as.numeric(input_plot_ymax) , label = reagent[[1]], hjust = 0)
		}
		#insert the calculated rectangles
		p = p + geom_rect(data=rectangle, inherit.aes=FALSE, aes(xmin=rectangle[[1]], xmax=rectangle[[2]], ymin=rectangle[[3]], ymax=rectangle[[4]]), alpha=0.1)+
		scale_x_continuous(limits=c(min(mdf[,1]),max(mdf[,1])),expand = c(0.01,0.01))
		if(input_plot_ymax == "default" & input_plot_ymin =="default"){
			p
		}
		else if(input_plot_ymax != "default" & input_plot_ymin == "default"){
			p+ ylim(y_min,as.numeric(input_plot_ymax))
		}
		else if(input_plot_ymax == "default" & input_plot_ymin != "default"){
			p + ylim(as.numeric(input_plot_ymin),y_max)
		}
		else {
			p + ylim(as.numeric(input_plot_ymin),as.numeric(input_plot_ymax))
		}
	}
	else{
		# this code gets executed if no log file is present. It simply returns the plot p as defined in the very beginning (Just the data, nothing more).
		p
	}

}
}

