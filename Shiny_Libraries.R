cat(file = stderr(), "Shiny_Libaries.R", "\n")

testit <- function() {
  message("testing package startup messages")
  packageStartupMessage("initializing ...", appendLF = FALSE)
  Sys.sleep(1)
  packageStartupMessage(" done")
}

suppressPackageStartupMessages(testit())

#shiny
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinyFiles)
library(shinyalert)
#library(shinyBS)

#basic
library(tidyverse)
library(data.table)
library(fs)
library(miscTools)
library(stringr)

#read write
library(readxl)
library(openxlsx)

#tables
library(RSQLite)
library(rhandsontable)
library(DT)

#parallel and background processing
library(callr)

#graphics
library(gplots)
library(rgl)
library(colourpicker)
library(randomcoloR)

