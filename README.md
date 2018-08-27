# Anabel
Program for the analysis of binding events

## Installation guide

There are two major ways to run Anabel. You can either run it locally on your PC (go to installation method 1) or host it for free online on shinyapps.io (go to installation method 2).
Either way, you first need to install the current version of Cran R on your computer. If you do not have R yet, you can download it here:

https://cran.r-project.org/

Choose your system, download the file and follow the installation instructions or use one of the following links:
[Windows](https://cran.r-project.org/bin/windows/base/) 
[Linux](https://cran.r-project.org/bin/linux/) 
[Mac](https://cran.r-project.org/bin/macosx/)


### Installation method 1: Run Anabel locally on your computer:

1. Download the current version of Anabel to your system. To do this, click on the green "Clone and Download" button and then click on "Download Zip". After download, simply unzip the file. If you want to use an older Anabel version, go to "Releases" and choose the one of interest.
2. Start R on your computer (If you do not have R yet, download and install it for [Windows](https://cran.r-project.org/bin/windows/base/), [Linux](https://cran.r-project.org/bin/linux/) or [Mac](https://cran.r-project.org/bin/macosx/)).It might be possible that you find two R programmes installed on your computer. Simply use the 64bit version (if you have a 64bit computer). However, either version should work. Just be careful to always keep to the same one.
3. Run the following R commands (Copy and paste the commands one after another into R and press "enter"). After excecuting the first command, you will be asked to choose a Cran Mirror. This is the server from which R will download all the nessecary files. Simply choose whichever one is located in your country. Yet, every server will work, even if it is not one in your country. The installation process of one command is finished when the ">" sign reappears and the cursor starts to blink again. Then you can copy and paste the next command. 
	```
	install.packages(c("shiny","markdown", "shinydashboard", "XLConnect", "ggplot2"))
	install.packages(c("reshape2","DT","ggExtra","cowplot","plyr","gridExtra","openxlsx"))
	```
5. Congratulations! You have now installed everything that is needed to run Anabel. Now, in order to start Anabel, just use the following two commands. Be aware to substitute PATH with the actual path of the unzipped Anabel folder on your computer. It might happen that the unzipping process generates an Anabel folder that contains another Anabel folder. Please use the folder containing all the seperate files listed above for the PATH. Moreover, DO NOT REMOVE THE QUOTATION MARKS! Furthermore, Windows users have to substitute the backslashes "\\" with normal slashes "/" in the PATH!
	```
	library(shiny)
	runApp("PATH")
	```
	Example: runApp("~/Desktop/Anabel-master")
Now, a browser window should pop up showing the start screen of Anabel. Have fun evaluating your data!
6. If you like to restart Anabel in order to perform a new analysis, simply reload the page in your browser. Thereafter, everything is set back to default.
7. To stop Anabel, go back to R and click on the "Stop" button. Now, you can close everything. 
8. In order to restart Anabel, repeat step 5.


### Installation Method 2: Host Anabel on shinyapps.io:

1.  Download the current version of Anabel to your system. To do this, click on the green "Clone and Download" button and then click on "Download Zip". After download, simply unzip the file. If you want to use an older Anabel version, go to "Releases" and choose the one of interest.
2. Go to https://www.shinyapps.io/ and sign in.
3. Open up R (If you do not have R yet, Download and install it for [Windows](https://cran.r-project.org/bin/windows/base/), [Linux](https://cran.r-project.org/bin/linux/) or [Mac](https://cran.r-project.org/bin/macosx/)) and initialise your shinyapps-io account by copying and pasting the commands shown on shinyapps.io into your R console. After excecuting the first command, you will be asked to choose a Cran Mirror. This is the server from which R will download all the nessecary files. Simply choose whichever one is located in your country. Yet, every server will work, even if it is not one in your country. The installation process of one command is finished when the ">" sign reappears and the cursor starts to blink again. Then you can copy and paste the next command. 
4. Upload anabel by using the following two commands. Be aware to substitute PATH with the actual path of the unzipped Anabel folder on your computer. DO NOT REMOVE THE QUOTATION MARKS! Furthermore, Windows users have to substitute the backslashes "\\" with normal slashes "/" in the PATH!

	```
	library(rsconnect)
	rsconnect::deployApp("PATH")
	```
	For example: rsconnect::deployApp("~/Desktop/Anabel")
5. After hitting the enter key, R will upload Anabel to your shinyapps.io account. Thereafter, a browser window should pop up running Anabel. Now, you can save the URL of the page and go back to it whenever you like. From now on,you can access this URL from any PC.
6. If you like to restart Anabel in order to perform a new analysis, simply reload the page in your browser. Thereafter, everything is set back to default.


