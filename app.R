##################################################################################################################################################
##################################################################################################################################################
#
#     ___      .__   __.      ___      .______    _______  __      
#    /   \     |  \ |  |     /   \     |   _  \  |   ____||  |     
#   /  ^  \    |   \|  |    /  ^  \    |  |_)  | |  |__   |  |     
#  /  /_\  \   |  . `  |   /  /_\  \   |   _  <  |   __|  |  |     
# /  _____  \  |  |\   |  /  _____  \  |  |_)  | |  |____ |  `----.
#/__/     \__\ |__| \__| /__/     \__\ |______/  |_______||_______|
#                                                                  
##################################################################################################################################################
##################################################################################################################################################
options(java.parameters = "-Xmx8000m")
library(shiny)
library(markdown)
library(shinydashboard)
library(openxlsx)
library(ggplot2)
library(reshape2)
library(DT)
library(ggExtra)

#loading all app modules

source("kobs_lin.R")
source("single_curve_analysis.R")
source("home.R")
source("download.R")
source("about.R")
source("privacy_policy.R")

# Setting app-parameter
box_colour = "success"
app_theme = "jeti.css"

# Increase upload size to 30MB
options(shiny.maxRequestSize=30*1024^2) 

#############################################################################################################################
# Set shiny app ui:
#############################################################################################################################
ui = 
	# Top navigation bar
	navbarPage("", inverse = T, collapsible = TRUE, theme = app_theme, position = c("fixed-top"),
			   home_UI("home"),
			   kobs_lin_UI("kobs_lin"),
			   single_curve_analysis_UI("sca"),
			   download_UI("Download"),
			   about_UI("About"),
			   privacy_policy_UI("Privacy Policy")
			   )
server = function(input, output,session){
	callModule(kobs_lin,"kobs_lin")
	callModule(single_curve_analysis,"sca")
	callModule(home,"home")
	callModule(download,"download")
	callModule(about,"about")
	callModule(privacy_policy,"privacy_policy")
}
shinyApp(ui,server)

