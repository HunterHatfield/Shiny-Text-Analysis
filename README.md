# Shiny Text Analysis App

This comprehensive linguistic analysis tool is designed with researchers in mind to facilitate analysis of text data with ease and precision. Equipped with advanced features, this app enables users to effortlessly import text files or CSV tables for analysis. It facilitates the removal of stop words, tokenization, and the generation of various plots including token frequency plots, illustrations of Zipf's Law, and TF-IDF analyses. Furthermore, researchers can conduct statistical testing using regression models and other tests to delve deeper into their data.

Designed with researchers in mind, this app provides a seamless experience from data input to insightful findings. Upon completion of analysis, users can conveniently generate reports in PDF, Word, or HTML formats, allowing for easy dissemination of research findings.

Whether exploring linguistic patterns, examining language evolution, or conducting corpus-based studies, this app empowers researchers with the tools they need to unlock the secrets hidden within textual data.
 
An R Shiny application developed by Hunter Hatfield and Emelia Hogg with the Department of Linguistics at the University of Otago. 

--- 

## Running the app

Run this Shiny application by downloading the app files, double-clicking on `ShinyTextApp.Rproj` and executing the function `runTextApp()`. 

Please note that on the first run of the app, any dependencies not already on your local machine may take several minutes to install.

### 1. Download or clone app files to your machine

Download and save the repository files to your machine through `Code > Download .zip`.

Alternatively, this repository can cloned into your R session using the repository URL: <https://github.com/HunterHatfield/Shiny-Text-Analysis>.

### 2. Launch `ShinyTextApp.Rproj`

Open the R project file `ShinyTextApp.Rproj`. This project and its associated `.Rprofile` will automatically call on all package dependencies for the app, R files, and run the `textApp()`. 

![Demo](Launch_Demo.gif)

### 3. Run the app with `runTextApp()`

Execute the command `runTextApp()` in the R console. This will launch the Shiny application on a local server.

To run the app, execute  the 'runTextApp()' function in the  console line just below this message.

Refresh the app by reloading your browser page or by hitting  the 'Reload' icon in the app's header.

Exit the app by clicking the 'Exit' icon in the app's header, or by clicking the small red 'Stop' button at the top right of the R console window.

![Run demo](Run.GIF)

--- 

## (Optional) Editing and manually lauching through R Studio

This app can be launched through in RStudio by calling on the `runTextApp()` function which is sourced in `R/app.R`. If any changes have been made, ensure all app files are sourced before the app is initialized again.

## Troubleshooting

For issues with package downloads, try removing current packages and re-installing required packages with the following: 

`remove.packages(rownames(installed.packages()))`

This removes the currently installed packages. 

`source("R/utils.R")`
`installPackages()`

These lines source the file `utils.R` and calls on the `installPackages()` function, which installs any missing dependencies.


