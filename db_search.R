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
			h3("Analyse your results data from Evaluation Method 1 or 2"),
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
		radioButtons(inputId=ns("db_search_mode"), choices=list("Normal Search","Advanced Search"), label = "Compare your results with our KOFFI Database", inline=TRUE),
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
		),
		
		column(width=12, class = "well",
			plotOutput(outputId=ns("kdiss_kass_plot"))
			),
		column(width = 12, class = "well",
			tableOutput(outputId=ns("test_table")),
			verbatimTextOutput(outputId=ns("test"))
			)
			
		)
	)
)

}

db_search = function(input, output, session, output_kobs_lin, output_sca){

	##########
	# Reactive Values
	##########

	values <- reactiveValues()
	values$query_results = data.frame()
	values$filter = c("article","method","device","chip","software","evaluation","model","partner_A","partner_A_longname","partner_A_information","partner_A_subtype","partner_A_maintype","partner_A_sequence","partner_A_species","partner_B","partner_B_longname","partner_B_information","partner_B_subtype","partner_B_maintype","partner_B_sequence","partner_B_species","position_in_article")
	values$method = c("icontains")
	values$logic = c("AND","OR","NOT")
	values$advanced_search_logic = list()
	values$advanced_search_filter = list()
	values$advanced_search = list()

	results_data = reactiveValues()


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
		url = "http://10.9.235.35:8181/api/interactions/?format=json&filters="
		
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
			# Qet API results
			x = GET(call)
			temp =content(x,"text")
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
		}
	})


	##########
	# Editable Dataframe holding query search results
	##########




	output$query_results_table <- DT::renderDataTable({
		df = values$query_results
		df_names = names(df)
		default_selection = c(names(df)[1], names(df)[2],names(df)[3], names(df)[8], names(df)[15], names(df)[22], names(df)[23], names(df)[24], names(df)[25], names(df)[26], names(df)[27])
		# Define which columns should be displayed
		selection = input$query_results_table_column_selection
		# when the first fit results are saved, selection is NULL and therefore it selects the default columns which are:
		if(is.null(selection)){
			# Defauld column selection
			updateCheckboxGroupInput(session, "query_results_table_column_selection", choices = df_names,
								 	selected = default_selection)
		}
		# Use the selection onto the dataframe
		df = df[,input$query_results_table_column_selection]
		# Output the dataframe and set the number of entries per page to 100
		DT::datatable(df, options = list( lengthMenu = c(100,500,1000)))
	})

	##########
	# Generate Kdis Kass plot
	##########

	output$kdiss_kass_plot = renderPlot({
		df_query = input$query_results_table_rows_selected
		df_fit_results = input$fit_results_table_rows_selected

		if(is.null(df_query)){
			df_query = input$query_results_table
		}
		if(is.null(df_fit_results)){
			df_fit_results = input$fit_results_table
		}

	})


	output$test = renderPrint({
		print(values$advanced_search_logic)
		print("##########")
		print(values$advanced_search_filter)
		print("##########")
		print(values$advanced_search)
		print("##########")
		print(values$test)
	})

}


