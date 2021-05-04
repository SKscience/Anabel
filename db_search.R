#####################################################################################################################################################################
#####################################################################################################################################################################
#
# _______       ___   .___________.    ___      .______        ___           _______. _______         _______. _______     ___      .______        ______  __    __  
#|       \     /   \  |           |   /   \     |   _  \      /   \         /       ||   ____|       /       ||   ____|   /   \     |   _  \      /      ||  |  |  | 
#|  .--.  |   /  ^  \ `---|  |----`  /  ^  \    |  |_)  |    /  ^  \       |   (----`|  |__         |   (----`|  |__     /  ^  \    |  |_)  |    |  ,----'|  |__|  | 
#|  |  |  |  /  /_\  \    |  |      /  /_\  \   |   _  <    /  /_\  \       \   \    |   __|         \   \    |   __|   /  /_\  \   |      /     |  |     |   __   | 
#|  '--'  | /  _____  \   |  |     /  _____  \  |  |_)  |  /  _____  \  .----)   |   |  |____    .----)   |   |  |____ /  _____  \  |  |\  \----.|  `----.|  |  |  | 
#|_______/ /__/     \__\  |__|    /__/     \__\ |______/  /__/     \__\ |_______/    |_______|   |_______/    |_______/__/     \__\ | _| `._____| \______||__|  |__| 
#                                                                                                                                                                    
#                                                                                                    
#####################################################################################################################################################################
#####################################################################################################################################################################

db_search_UI = function(id){
	ns = NS(id)
	tabPanel("KOFFI Database Analysis",
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
	# ALL OTHER UI
	#####################
	#++++++++++++++++++++
	column(width=12, align="center",	
		h1("Compare your results with KOFFI")
	),

	##########
	# Transfer of upload results for db analysis
	##########

	column(width =12, class = "well",
		column(width = 6,
			h3("Transfer your results from method 1 or 2"),
			actionButton(inputId = ns("transfer_data"), "Get Data")
			),
		column(width = 6,
			h3("Upload an Anabel results file (Excel)"),
			fileInput(ns('upload_data'),'Choose file')
			)
	),
	column(width = 10, offset = 1, align="center",
		DT::dataTableOutput(outputId=ns("fit_results_table"))
	),

	column(width=12, align="center", class = "well",
		radioButtons(inputId=ns("db_search_mode"), choices=list("Normal Search","Advanced Search"), label = "Compare your results with our KOFFI database", inline=TRUE),
		conditionalPanel(
			condition = paste0("input['" , ns("db_search_mode"), "'] == 'Normal Search'"),
			column(width=12, align="left",
				textInput(inputId = ns("search_input"),"Search:")
			)
		),
		conditionalPanel(
			condition =  paste0("input['" , ns("db_search_mode"), "'] == 'Advanced Search'"),
			column(width=12,
				column(width=2,offset=1, align="left",
					selectInput(ns("search_field_filter"), "Select search field", choices=list(), selected = NULL)
					),
				column(width=8, align="left",
					textInput(inputId = ns("search_field"),"Term:")
					),
				column(width=1, align="left",
					actionButton(inputId = ns("add_search_field"), "Add Search Field")
					)
			),
			column(width=12,
				column(width=1, align="left",
					tags$div(id = 'search_field_logic_placeholder')
					),
				column(width=2, align="left",
					tags$div(id = 'input_field_placeholder')
					),
				column(width=8, align="left",
					tags$div(id = 'field_search_placeholder')
					)
			)
		),
		column(width=12,
			actionButton(inputId = ns("search"), "Search")
		),
		# Illustrate Query results
		sidebarLayout(
			sidebarPanel(width=2,
				checkboxGroupInput(inputId= ns("query_results_table_column_selection"), choices = NULL, selected = NULL, label = NULL)
			),
			mainPanel(width=10,
				DT::dataTableOutput(outputId=ns("query_results_table"))
				)
		)
	),
	
	sidebarLayout(
		sidebarPanel(width=2,
			selectInput(ns("db_group"),"Group database results by:", choices=list(), selected = NULL),
			textInput(inputId = ns("kd_steps"),"KD line steps:", value ="10"),
# Experimental Adding Mean and Median points to plot. Difficult as ggoplot only allows two color aesthetics.
#			radioButtons(inputId = ns("kd_means"),label="Show mean values", choices=list("No","Yes"),selected="No"),
#			radioButtons(inputId = ns("kd_medians"),label="Show median values", choices=list("No","Yes"),selected="No"),
		    radioButtons(inputId = ns("save_format"), choices = list("pdf", "png", "jpeg","tiff","ps","bmp"), label = "Select the file type", inline=TRUE ),
		    downloadButton(outputId = ns("download_kdiss_kass_plot"), label = "Download plot")	
			),
			mainPanel(width=10,
				plotOutput(outputId=ns("kdiss_kass_plot"))
			)
		),
	column(width=12, align="center",
		downloadButton(outputId = ns("download_result_file"), label = "Download all results")
		)
#	column(width = 12, class = "well",
#		tableOutput(outputId=ns("test_table")),
#		verbatimTextOutput(outputId=ns("test"))
#		)
			
	)
)

}


db_search = function(input, output, session, output_kobs_lin, output_sca){

	##########
	# Reactive Values
	##########

	values <- reactiveValues()
	values$filter = c("method","device","chip","software","evaluation","model","partner_A","partner_A_longname","partner_A_information","partner_A_subtype","partner_A_maintype","partner_A_sequence","partner_A_species","partner_B","partner_B_longname","partner_B_information","partner_B_subtype","partner_B_maintype","partner_B_sequence","partner_B_species")
	values$method = c("icontains")
	values$logic = c("AND","OR","NOT")
	values$advanced_search_logic = list()
	values$advanced_search_filter = list()
	values$advanced_search = list()
	values$kdiss_kass_plot = NULL

	results_data = reactiveValues()

	test <- reactiveValues()



	##########
	# Use existing data
	##########

	observeEvent(input$transfer_data,{
		df_kobs = output_kobs_lin()$fit_results
		df_sca = output_sca()$fit_results
		if(!is.null(df_kobs)& is.null(df_sca)){
			results_data$df = df_kobs
		}
		else if(is.null(df_kobs)& !is.null(df_sca)){
			results_data$df = df_sca
		}
	})


	##########
	# Upload existing results file
	##########

	observeEvent(input$upload_data, {
		#Input Form
        file = input$upload_data
        if (is.null(file)){
            return(NULL)
        }
        path = file$datapath
		
		wb = loadWorkbook(path)
		df = read.xlsx(wb, sheet = "all_binding_constants", startRow = 1, colNames = TRUE, rowNames = FALSE, check.names=TRUE)
		
		results_data$df = df
	})

	##########
	# Show user fit results (imported or transfered into module)
	##########

	output$fit_results_table <- DT::renderDataTable({
		results_data$df
	})



	##########
	# Update filter fields of advanced search
	##########
	observe({
		x = values$filter
		updateSelectInput(session, "search_field_filter", choices = x)
	})


	##########
	# Add fileds for advances API Search
	##########

    observeEvent(input$add_search_field, {
		add = input$add_search_field
		search_field_logic_id = paste0("search_field_logic",add)
		search_field_filter_id = paste0("input_field",add)
		search_field_id = paste0("field_search",add)
	   insertUI(
		 selector = '#search_field_logic_placeholder',
		 ui = selectInput(session$ns(search_field_logic_id), 'Select logic', choices=list("AND","OR","NOT"), selected = NULL)
	   )
	   insertUI(
		 selector = '#input_field_placeholder',
		 ui = selectInput(session$ns(search_field_filter_id), 'Select search field', choices=values$filter, selected = NULL)
	   )
	   insertUI(
		 selector = '#field_search_placeholder',
		 ui = textInput(session$ns(search_field_id), 'Term:')
	   )
	   # Generate reactive objects for newly generated ui elements and save their inputs in the reactive value object
		observeEvent(input[[search_field_logic_id]],{
			values$advanced_search_logic[[add]] = input[[search_field_logic_id]]
	   })
		observeEvent(input[[search_field_filter_id]],{
			values$advanced_search_filter[[add]] = input[[search_field_filter_id]]
	   })
		observeEvent(input[[search_field_id]],{
			values$advanced_search[[add]] = input[[search_field_id]]
	   })

    })

	##########
	# Perform API database search
	##########

	observeEvent(input$search, {
		# Generate basic API url
		url = "http://koffidb.org/api/interactions/?format=json&page_size=100&filters="
		
		###
		# Unspecific search
		###
		if(input$db_search_mode == "Normal Search"){
			query = input$search_input
			# Apply filter
			filter = values$filter 
			method = values$method
			filter_method = paste(filter,method, sep="__")
			#Generate API query and put together to complete url call
			query = paste(filter_method,query,sep="=")
			query = paste("(",query,")",sep="")
			call = paste(url,paste(query,collapse="|"),sep="")
			values$call = call
			# Qet API results
			x = GET(call)
			temp =suppressMessages(content(x,"text"))
			# rende json format to data_frame
			df = fromJSON(temp, flatten = TRUE)$results
			# Save rendered Data frame
			values$query_results = df
		}
		# Advanced search
		else{
			query = input$search_field
			filter = input$search_field_filter
			method = values$method
			filter_method = paste(filter,method, sep="__")
			query = paste(filter_method,query,sep="=")
			query = paste("(",query,")",sep="")
			call = paste(url,query,sep="")
			
			if(length(values$advanced_search)!=0){
				for(i in 1:length(values$advanced_search)){
					if(values$advanced_search[[i]]!=""){
						logic_value = values$advanced_search_logic[[i]]
						filter = values$advanced_search_filter[[i]]
						query = values$advanced_search[[i]]
						method = values$method
						filter_method = paste(filter,method, sep="__")
						query_final = paste(filter_method,query,sep="=")

						if(logic_value == "AND"){
							logic = "%26"
						}
						else if(logic_value == "OR"){
							logic = "|"
						}
						else{
							query_final = paste(filter_method,query,sep="!=")
							logic="%26"
						}

						query_final = paste("(",query_final,")",sep="")
						call = paste(call,query_final,sep=logic)
					}
				}
			}
			values$test = call
			x = GET(call)
			temp = content(x,"text")
			df = fromJSON(temp, flatten=TRUE)$results
			values$query_results=df
		}
	})


	##########
	# Editable Dataframe holding query search results
	##########




	output$query_results_table <- DT::renderDataTable({

		shiny::validate(
			need(length(values$query_results) >0, "No database search results")
			)

		df = values$query_results
		df_names = names(df)
		default_selection = c(names(df)[1], names(df)[2],names(df)[3], names(df)[8], names(df)[15], names(df)[22], names(df)[23], names(df)[24], names(df)[25], names(df)[26], names(df)[27])
		# Define which columns should be displayed
		selection = input$query_results_table_column_selection
		# when the first fit results are saved, selection is NULL and therefore it selects the default columns which are:
		if(is.null(selection)){
			# Defauld column selection
			updateCheckboxGroupInput(session, "query_results_table_column_selection", choices = df_names,selected = default_selection)
			
			# Update dropdown menue selector for kdiss kass plot
			updateSelectInput(session, "db_group", choices = df_names,selected = "method" )
			
		}
		# Use the selection onto the dataframe
		df = df[,input$query_results_table_column_selection]
		# Output the dataframe and set the number of entries per page to 100
		DT::datatable(df, options = list( lengthMenu = c(20,500,1000)))
	})

	##########
	# Generate Kdis Kass plot
	##########

	output$kdiss_kass_plot = renderPlot({

		shiny::validate(
			need(length(values$query_results) >0 | length(results_data$df) >0 , "Transfer / upload data or perform a KOFFI database search to generate the plot.")
			)
		

		kd_lines = function(plot,kd_max, kd_min, kass_max, kdiss_max){
	
			kd_min = 10 ^ log10(kd_min)
			kd_max = 10 ^ log10(kd_max)

			kd_step = as.numeric(input$kd_steps)		
			
			kd_break = 10 ^ceiling(log10(kdiss_max/kass_max))
			

			kd = c(kd_break)

			kd_high = as.numeric(kd_break)
			
			cat(kd_high)
			cat(kd_max)

			while(kd_high < as.numeric(kd_max)){
				kd_high = kd_high * kd_step
				kd = c(kd,kd_high)
			}

			kd_low = as.numeric(kd_break)
			cat(kd_low)
			while(kd_low > as.numeric(kd_min)){
				kd_low = kd_low / kd_step
				kd = c(kd,kd_low)
			}

			kd = sort(kd)
			
			
			p = plot + geom_abline(intercept=log10(1/kd), slope=1, colour="gray")
			# Add annotation for KD lines
			for(i in kd){
					if(i<kd_break){
						p = p + annotate(geom="text",label=paste("KD",formatC(i, format = "e", digits = 2),sep="\n"),x=log10(i*kass_max*1.5),y=log10(kass_max*1.5), color="darkgrey")
					}
					else{
						p =  p + annotate(geom="text",label=paste("KD",formatC(i, format = "e", digits = 2),sep="\n"),x=log10(kdiss_max*1.5),y=log10(kdiss_max/i*1.5), color="darkgrey")
					}

			}
			return(p)
		}

#####
# Experimental adding Mean and Median points to plot
#####

#		# Functions to add means or medians to plot
#		add_df_stats = function(p, df, fun){
#			if(fun == "mean"){
#				df_means = aggregate(df[, c(5,7)], list(df$Spot), mean, na.rm=TRUE)
#				p = p + geom_point(data=df_means,  aes(x=log10(df_means[,2]), y=log10(df_means[,3]), fill= df_means[,1]),colour="black",size=6,shape=22)
#			}
#			else if(fun == "median"){
#				df_medians = aggregate(df[, c(5,7)], list(df$Spot), median, na.rm=TRUE)				
#				p = p + geom_point(data=df_medians,  aes(x=log10(df_medians[,2]), y=log10(df_medians[,3]), fill= df_medians[,1]), colour="black",size=6,shape=23)
#			}
#			return(p)
#		}
#
#		add_db_stats = function(p, db, fun){
#			if(fun == "mean"){
#				db_means = aggregate(db[, c("koff","kon")], list(db[,input$db_group]), mean, na.rm=TRUE)
#				p = p + geom_point(data=db_means,  aes(x=log10(koff), y=log10(kon), fill= db_means[,1]),colour="black",size=6,shape=22)
#			}
#			else if( fun == "median"){
#				db_medians = aggregate(db[, c("koff","kon")], list(db[,input$db_group]), median, na.rm=TRUE)
#				p = p + geom_point(data=db_medians,  aes(x=log10(koff), y=log10(kon), fill= db_medians[,1]),colour="black",size=6,shape=23)
#			}
#			return(p)
#		}



		db_rows = input$query_results_table_rows_selected
		df_rows = input$fit_results_table_rows_selected
		
		df = results_data$df
		db = values$query_results

		# Set df or db NULL if there are are 0 rows of data present. Needed for further validation
		if(length(df)==0){
			df = NULL
		}
		if(length(db)==0){
			db= NULL
		}


		if(length(db_rows)>0){
			db = values$query_results[db_rows, ]
		}
		if(length(df_rows)>0){
			df = results_data$df[df_rows, ]
		}
		
		if(!is.null(db)){
			db = db[complete.cases(db[,c("koff","kon")]),]
		}
		
		kd_steps = as.numeric(input$kd_steps)/10

		# Draw plot with KD lines
		if(!is.null(db) & !is.null(df)){
			# Check if the selection already updated. Otherwise, R will throw error to command line
			shiny::validate(
				need(input$db_group,"")
				)
			
			
			kass_max = suppressWarnings(max(c(max(na.omit(db$kon)),max(na.omit(df[,7])))))
			kass_min = suppressWarnings(min(c(min(na.omit(db$kon)),min(na.omit(df[,7])))))
			kdiss_min = suppressWarnings(min(c(min(na.omit(db$koff)),min(na.omit(df[,5])))))
			kdiss_max = suppressWarnings(max(c(max(na.omit(db$koff)),max(na.omit(df[,5])))))
			

			kd_db = (db$koff/db$kon)
			kd_df = df[,5]/df[,7]
			kd = c(kd_db, kd_df)
			kd_min = min(na.omit(kd))
			kd_max = max(na.omit(kd))
			
			# Database to plot
			p = ggplot(data = db, aes(x=log10(koff), y=log10(kon), label=db$id, color = db[,input$db_group] )) + geom_point(shape=18, size = 4, color="black")+ geom_text_repel(size = 5, segment.color = "grey", segment.size=1)
			# dataframe of own data to plot
			p = p + geom_point(data = df, aes(x=log10(df[,5]), y=log10(df[,7]), fill=Spot), inherit.aes = FALSE, size = 4, shape=21) + geom_text_repel(size=5, segment.color="grey", segment.size=1, data=df, aes(x=log10(df[,5]), y=log10(df[,7]),label=Spot),inherit.aes = FALSE, show.legend = FALSE)
			# Add Kd lines to plot
			p = kd_lines(p,kd_max, kd_min, kass_max, kdiss_max)


#			if(input$kd_means == "Yes"){
#				p = add_db_stats(p,db,"mean")
#				p = add_df_stats(p,df,"mean")
#			}
#			if(input$kd_medians == "Yes"){
#				p = add_db_stats(p,db,"median")
#				p = add_df_stats(p,df,"median")
#			}
			
			p = p + labs(x="log10(kdiss [1/t])", y="log10(kass [1/t*M])", color="KOFFI-DB ID", fill="Anabel Results")

			

		}
		else if(!is.null(db) & is.null(df)){
			# Check if the selection already updated. Otherwise, R will throw error to command line
			shiny::validate(
				need(input$db_group,"")
				)
			
			kass_max = suppressWarnings(max(na.omit(db$kon)))
			kass_min = suppressWarnings(min(na.omit(db$kon)))
			kdiss_min = suppressWarnings(min(na.omit(db$koff)))
			kdiss_max = suppressWarnings(max(na.omit(db$koff)))
			
			
			kd = (db$koff/db$kon)
			kd_min = min(na.omit(kd))
			kd_max = max(na.omit(kd))
			

			p = ggplot(data = db, aes(x=log10(koff), y=log10(kon), label=db$id,color = db[,input$db_group])) +geom_point(shape=18, size = 4, color="grey")+ geom_text_repel(size = 5, segment.color = "grey", segment.size=1, show.legend = FALSE)
		
			p = kd_lines(p,kd_max, kd_min, kass_max, kdiss_max)

#			if(input$kd_means == "Yes"){
#				p = add_db_stats(p , db, "mean")
#			}
#			if(input$kd_medians == "Yes"){
#				p = add_db_stats(p, db, "median")
#			}
			
			p = p + labs(x="log10(kdiss [1/t])", y="log10(kass [1/t*M])", colour="KOFFI-DB ID", fill="Mean (Square) \nMedian (Rhombus)")

			
		}

		else if(is.null(db) & !is.null(df)){
			kass_max = suppressWarnings(max(na.omit(df[,7])))
			kass_min = suppressWarnings(min(na.omit(df[,7])))
			kdiss_min = suppressWarnings(min(na.omit(df[,5])))
			kdiss_max = suppressWarnings(max(na.omit(df[,5])))

			kd = df[,5]/df[,7]
			kd_min = min(na.omit(kd))
			kd_max = max(na.omit(kd))


			p = ggplot(data = df, aes(x=log10(df[,5]), y=log10(df[,7]), fill=Spot, label=Spot)) + geom_point(size = 4, shape=21) + geom_text_repel(size = 5, segment.color = "grey", segment.size=1, show.legend = FALSE)
			
			p = kd_lines(p,kd_max, kd_min, kass_max, kdiss_max)
			
#			if(input$kd_means == "Yes"){
#				p = add_df_stats(p, df, "mean")
#			}
#			if(input$kd_medians == "Yes"){
#				p = add_df_stats(p, df, "median")
#			}
			
			p = p + labs(x="log10(kdiss [1/t])", y="log10(kass [1/t*M])", fill="Mean (Square) \nMedian (Rhombus)", colour="Anabel Results")


		}
		values$kdiss_kass_plot = p
		p
	})

	##########
	# Download kdiss_kass plot
	##########
	output$download_kdiss_kass_plot = downloadHandler(
	filename = function(){
		paste(paste0("kdiss_kass_plot"), input$save_format, sep=".")
	},
	content = function(file){
		ggsave(file, plot = values$kdiss_kass_plot, device = input$save_format, width=40, units=c("cm"))
	}
	)
	
	##########
	# Generate and Download excel file containing all results
	##########
	output$download_result_file <- downloadHandler(
		filename = function() { 'koffi_db_analysis.xlsx' },
		content = function(file) {
			
			wb <- createWorkbook()
			
			#Create the worksheet containing kdiss kass plot
			addWorksheet(wb, "kdiss_kass_plot")
			plot_name = paste0("kdiss_kass_plot",session$token,".png")
			ggsave(plot_name, plot = values$kdiss_kass_plot, device = "png", width=40, units = c("cm"))
			insertImage(wb, "kdiss_kass_plot", plot_name, startRow = 2, startCol = 2, width = 40, height=17.77, units="cm")
			
			#Create worksheet containing database search results
			addWorksheet(wb, "koffi_db")
			writeData(wb,"koffi_db", values$query_results,startRow=2, startCol=2)
			
			#Create worksheet containing database search results
			addWorksheet(wb, "own_data")
			writeData(wb,"own_data", results_data$df, startRow=2, startCol=2)	
			
			filename = paste(file,"xlsx",sep=".")
			saveWorkbook(wb, file = "koffi_db_analysis.xlsx", overwrite = TRUE)
			file.rename("koffi_db_analysis.xlsx",file)

		}
		)



	output$test = renderPrint({
		print(values$test)
#		print("##########")
#		print(values$advanced_search_filter)
#		print("##########")
#		print(values$advanced_search)
#		print("##########")
#		print(values$call)
#
	})

}


