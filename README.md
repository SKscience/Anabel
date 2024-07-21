<img src="./www/anabel_logo.png" alt="Anabel-Logo" width="300">

# Anabel-App

Anabel is an open source project, designed for fast, reliable analysis of binding curves of biomolecular 1:1 interactions.

This is the App version of Anabel, which supplies an easy user interface for the R Anabel package. All the binding interaction modeling is performed by the [R Anabel package](https://github.com/SKscience/Anabel), that can be used independently of the App version.

## Install the Anabel-App

The Anabel app was built using R and Shiny. To run the app on your machine, you need to have R and RStudio installed. Please follow these steps to set up Anabel on your machine:

1. Download and install R: [CRAN-R](https://cran.r-project.org/index.html)
2. Open R and run the following command:  
   `install.packages("shiny")`

You are now ready to start and run Anabel on your machine. Of course, you can also use RStudio to run the app.

## Start the Anabel-App

To start Anabel, open R and run the following command:  
`shiny::runGitHub("Anabel", "SKscience")`

If you run the app for the first time, you might be asked to install all dependencies. Answer with "Y" for yes. The app should now start locally in your browser. Have fun using Anabel!

## How to use the Anabel-App

### Step 1: Load your data
Our open data format supports the analysis of various different data sets, like BLI and SPR.

### Step 2: Select the mode of analysis
You can choose between 3 different methods:

<img src="./www/methods.png" alt="Image of analysis methods" width="500">

### Step 3: Supply additional information
These include analyte concentration(s) as well as the starting point(s) of the association(s).

### Step 4: Run Anabel and download the results
We supply an Excel table containing all relevant binding parameters as well as a plot of the binding curves including their fits.

## ANABEL in action:

<img src="./www/anabel_app.gif" alt="ANABEL-App GIF" width="700">

## Changelog:
