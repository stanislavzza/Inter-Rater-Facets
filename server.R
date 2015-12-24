#This file is part of Inter-Rater Facets

#Inter-Rater Facets is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#Inter-Rater Facets is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with Inter-Rater.  If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(tidyr)

source("include.R") #contains the statistical computations

############ GLOBALS #########################
global <- list(dataUploadDone = FALSE,dataFiles = 0,logit = NULL, fitted <- NULL)

#keep track of the files loaded
dataFileInfo <- data.frame(Filename = character(),Rows=integer(),Columns=integer(),Status=character())

# main data frame to hold the ratings
df <- data.frame(NoData=NA)


############ SERVER ##########################

shinyServer(function(input, output) {
  
  #REACTIVE VALUES-------------------------------------------------------------------
  rvals <-reactiveValues(dataDoneButton=FALSE,dataLoaded=FALSE,refreshData=FALSE,
                         updateDataInfo = 0,interestCB=0,logitModel=0,newVarExists=0,
                         graphUpdate=c(0,0),graphAUC=c(0,0),graphReady=0)

  #INPUT-----------------Button for finished loading data---------------
  output$dataDoneButton <- renderUI({
    rvals$dataDoneButton # create a reactive dependency
    if(rvals$dataLoaded || !global$dataFiles) return("")
    actionButton("dataDoneUploading","Done Uploading")
  })
  
  #INPUT------------- Select data file for upload -----------------------
  output$dataFileUploader <- renderUI({
    if(rvals$dataLoaded) return("")
    
    fileInput('dataFile', 'Choose data File (CSV)',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
  })
  
  #INPUT--------------------- Select the Subject ID column-----------------------
  output$subjectID <- renderUI({ 
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") # all done with this
    namelist <- as.list(sort(names(df)))
    selectInput("subjectID", "Subject ID",  namelist )
  })
  
  #INPUT--------------------- Select Ratings names column-----------------------
  output$ratingNames <- renderUI({ 
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") # all done with this
    if(denull(input$subjectID)== 0) return("")
    namelist <- as.list(sort(names(df)))
    namelist <- namelist[namelist != input$subjectID]
    namelist <- c("In Columns",namelist)
    selectInput("ratingNames", "Rating Names",  namelist )
  })
  
  #INPUT--------------------- Select Ratings Values column-----------------------
  output$ratingVals <- renderUI({ 
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") #all done with this
    if(denull(input$subjectID)== 0 || denull(input$ratingNames,"In Columns")== "In Columns") return("")
    namelist <- as.list(sort(names(df)))
    namelist <- namelist[namelist != input$subjectID]
    namelist <- namelist[namelist != input$ratingNames]
    selectInput("ratingVals", "Rating Values",  namelist )
  })
  
  
  
  #OBSERVER----------- Process data -------------------------------------
  loadDataFile <- observe({ 
    inFile <- input$dataFile
    if (is.null(input$dataFile)){
      h4("")
    } else {
      tdf <- read.csv(inFile$datapath)
      
      # increment the number of files we've uploaded
      global$dataFiles <<- global$dataFiles + 1
      
      df <<- tdf
      status = "Loaded Okay"

      dataFileInfo <<- data.frame(Filename = input$dataFile$name,Rows=nrow(tdf),Columns=ncol(tdf),Status=status)
    
      dataFileInfo$Rows[1] <<- nrow(df)
      dataFileInfo$Columns[1] <<- ncol(df)
      rm(tdf)
      
      # turn on the 'finished' button and update the data tab
      isolate(rvals$dataDoneButton <- TRUE)
      isolate(rvals$updateDataInfo <- rvals$updateDataInfo + 1)
      
    }
  })
  
  
  #INPUT-----------------Button for finished loading data---------------
  output$dataDoneButton <- renderUI({
    rvals$dataDoneButton # create a reactive dependency
    if(rvals$dataLoaded || !global$dataFiles) return("")
    actionButton("dataDoneUploading","Finished")
  })
  
  #OBSERVER--------------spread out data if necessary
  processData <- observe({
    if (denull(input$dataDoneUploading[1]) == 0)
      return()
    
    if (denull(input$ratingNames,"In Columns") != "In Columns" 
        && denull(input$ratingVals,"In Columns") != "In Columns"
        && denull(input$ratingNames) != denull(input$ratingVals)){
      
      df$ID__temp__ <- 1:nrow(df) # needed to make each row unique
      df <<- spread_(df, input$ratingNames,input$ratingVals)
      df$ID__temp__ <<- NULL # get rid of the ID
      # cf http://stackoverflow.com/questions/21390141/specify-dplyr-column-names
    }
  
    isolate(rvals$dataLoaded <<- TRUE)
  })
  
  #--------------------------------------------------------------------------------#
  ############################### END FILE UPLOADS #################################
  
  ############################### DATA PANEL  ######################################
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  
  #OUTPUT---------------------------- Describe File Uploads 
  output$dataPanelHeader <- renderTable({
    rvals$updateDataInfo # reactive dependency
    dataFileInfo
  })
  
  #OUTPUT-------------------  Create and show dataframe of variable characteristics
  output$varStats <- renderDataTable({
    #rvals$updateDataInfo # dependency for intermediate uploads
    if(denull(rvals$dataLoaded) ==0) return() # dependency for final processing
    
    dftype <- c()
    dfN <- c()
    dfSummary <- c()
    dfNA <- c()
    dfUnique <- c()
    dfAction <- c()
    dfmatched <- c()
    subjectID <- isolate(input$subjectID)
    
    dfnames <- as.character(sort(names(df)))
    for (t in dfnames) {
      dftype <- c(dftype,class(df[[t]]))
  
      dfN <- c(dfN, sum(!is.na(df[[t]])) )
      
      u <- length(unique(df[[t]][!is.null(df[[t]])]))
      dfUnique <- c(dfUnique, u )
      
      if (u < 11 && u > 1) { # only allow scores with up to 10 unique response values
        tbl <- rowSums(table(df[[subjectID]],df[[t]]))
        m <- sum(tbl[tbl > 0] - 1)
        dfmatched <- c(dfmatched, m  )
      } else {
        m <- 0
        dfmatched  <-  c(dfmatched, NA )
      }
      
      # drop the column of data if it's useless. We require at least 5 shared ratings.
      if (t != subjectID && (u > 10 || m < 5)){
          df[[t]] <<- NULL
          dfAction <- c(dfAction,"Drop")
      } else {
          dfAction <- c(dfAction,"Keep")
      }

    }
    dfVarStats <<- data.frame(Variable = dfnames, Type = dftype, N = dfN, Unique = dfUnique, Matched = dfmatched, Action = dfAction)
    dfVarStats
  })
  
  ################################## Rater Agreement ###########################################
  
  #INPUT--------------------- Select the Target Var to predict-----------------------
  output$targetVar <- renderUI({ 
    # turning off the newvars update for now--it's quite annoying
    #rvals$newVarExists # update the list of names if a new var is created
    if (!rvals$dataLoaded || denull(input$subjectID) == 0) return("")
    namelist <- as.list(sort(names(df)))
    namelist <- namelist[namelist != input$subjectID]
    
    selectInput("targetVar", "Ratings",  namelist )
  })
  
  #INPUT -------------------- Select the target inclass range--------------------------------
  output$targetRangeIn <- renderUI({ 
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 || denull(input$subjectID) == 0) return("")
    namelist <- as.list(names(table(df[[input$targetVar]])))
    selectInput("targetVarRangeIn", "Optional zoom (pick two)", namelist, multiple = TRUE )
  })
  
  #INPUT -------------------- Choose a sample size for large samples --------------------------------
  output$sampleSize <- renderUI({ 
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 || denull(input$subjectID) == 0) return("")
    n <- nrow(df)
    if(n < 10000) return("")
    sliderInput("sampleSize", "Sample Size", 10000, n, 10000) 
  })
  
  #INPUT -------------------- Button to compute the stats
  output$action <- renderUI({
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 || denull(input$subjectID) == 0) return("")
    if (!is.null(input$targetVarRangeIn) && length(input$targetVarRangeIn) != 2) return ("")
    actionButton("action","Compute")
  })
  
  #OUTPUT-------------------------------graph the agreement
  output$graph <- renderPlot({
    if(denull(input$action[1],0)==0) return() # tied to action button
    
    if (denull(isolate(input$sampleSize)) > 1){
      subsample <- sample(1:nrow(df),isolate(input$sampleSize))
      df <- df[subsample,]
    }
    
    n_matrix <- as.data.frame.matrix(table(df[[input$subjectID]],df[[input$targetVar]]) )
    
    if (!is.null(isolate(input$targetVarRangeIn))) { #zoom
      zoom = which(names(n_matrix) %in% isolate(input$targetVarRangeIn))
      size = 7
    } else {
      zoom = NULL
      size = 3
    }
   
    lambda_graph_facets(n_matrix,text_size=size,zoom = zoom)
  })
  
  
  #OBSERVE---------------------------------- debugging function -----------------------------------------
  observe(label="console",{
    if(denull(input$console) != 0) {
      options(browserNLdisabled=TRUE)
      saved_console<-".RDuetConsole"
      if (file.exists(saved_console)) load(saved_console)
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
    }
  })

})


############## HELPERS

denull <- function(x,y=0){
  if (is.null(x)) return(y)
  return(x)
}
