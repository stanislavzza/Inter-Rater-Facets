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
SID <- "" # place to keep the subject ID

wide <- data.frame(SubjectID=c(111,111,112,112),Objective1=c(3,3,2,1),Objective2=c(1,4,5,2),Objective3=c(2,2,1,3))
rater <- data.frame(Optional_SubjectID=c(111,112,113,114),Rater1=c(3,3,2,1),Rater2=c(1,4,5,2),Rater3=c(2,2,1,3))
narrow <- data.frame(SubjectID=c(111,111,112,112),ObjectiveName=c("Writing","Speaking","Writing","Speaking"),Ratings=c(1,4,5,2))
############ SERVER ##########################

shinyServer(function(input, output,session) {
  
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
    if (rvals$updateDataInfo >0 ) return("") # all done
    fileInput('dataFile', 'Choose data File (CSV)',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
  })
  
  #INPUT--------------------- Select the data format -----------------------
  output$dataType <- renderUI({ 
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") # all done with this
    
    radio_choices <- c("Outcome Ratings in Columns"="wide","Rater Ratings in Columns"="raters","Outcome Names in a Column"="narrow")
    radioButtons("dataType", "Type of Data", radio_choices, selected = "wide", inline = FALSE, width = NULL)
  })
  
  #OUTPUT--------------------- Some instructions -----------------------
  output$dataTypeInstructions <- renderText({
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") # all done with this
    return("Use this option if your data looks like the arrangement below.")
  })
  
  #OUTPUT--------------------- Display sample data type -----------------------
  output$dataTypeExample <- renderTable({
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") # all done with this
    if (denull(input$dataType) == "wide"){
      wide
    } else if(denull(input$dataType) == "narrow") {
      narrow
    } else {
      rater
    }
  },digits = 0)
  
  #INPUT--------------------- Select the Subject ID column-----------------------
  output$subjectID <- renderUI({ 
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") # all done with this
    namelist <- as.list(sort(names(df)))
    if(denull(input$dataType,"")=="raters") namelist <- c("[None]",namelist)
    selectInput("subjectID", "Subject ID",  namelist )
  })
  
  reactive({
    SID <<- input$subjectID
  })
  
  #INPUT--------------------- Select Ratings names column-----------------------
  output$ratingNames <- renderUI({ 
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") # all done with this
    if(denull(input$dataType,"raters")!="narrow") return("") # don't need this unless narrow type
    if(denull(input$subjectID)== 0) return("")
    namelist <- as.list(sort(names(df)))
    namelist <- namelist[namelist != input$subjectID]
    selectInput("ratingNames", "Outcome Names",  namelist )
  })
  
  #INPUT--------------------- Select Ratings Values column-----------------------
  output$ratingVals <- renderUI({ 
    if (rvals$updateDataInfo < 1) return("") # no file yet
    if(rvals$dataLoaded) return("") #all done with this
    if(denull(input$subjectID)== 0 || denull(input$dataType,"raters") != "narrow") return("")
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
    
    type <- isolate(input$dataType)
    SID <<- isolate(input$subjectID)
    
    if (type == "narrow"){
      df$ID__temp__ <- 1:nrow(df) # needed to make each row unique
      df <<- spread_(df, input$ratingNames,input$ratingVals)
      df$ID__temp__ <<- NULL # get rid of the ID
      
      # cf http://stackoverflow.com/questions/21390141/specify-dplyr-column-names
    } else if(type == "raters"){
      if (input$subjectID == "[None]") {
        df$SubjectID__ <<- 1:nrow(df)  # create an ID manually
      } else {
        df$SubjectID__ <<- df[[input$subjectID]] # to create a known ID field
        df[[input$subjectID]] <<- NULL 
      }
      SID <<-  "SubjectID__" 
      df <<- gather(df, Rater, Rating, -SubjectID__)
      df$Rater <<- NULL # don't need this
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
    subjectID <- SID
    
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
    if (!rvals$dataLoaded) return("")
    if(denull(input$tabs) != "Facets" && denull(input$tabs) != "Distribution" ) return("")
    
    namelist <- as.list(sort(names(df)))
    namelist <- namelist[namelist != SID]
    select <- isolate(input$targetVar) # save the old choice if there is one
    selectInput("targetVar", "Ratings",  choices = namelist , selected = select )
  })
  
  #INPUT -------------------- Select the target inclass range--------------------------------
  output$targetRangeIn <- renderUI({ 
    if (!rvals$dataLoaded || denull(input$targetVar) == 0) return("")
    if(denull(input$tabs) != "Facets") return("")
    namelist <- as.list(names(table(df[[input$targetVar]])))
    selectInput("targetVarRangeIn", "Optional zoom (pick two)", namelist, multiple = TRUE )
  })
  
  #INPUT -------------------- Choose a sample size for large samples --------------------------------
  output$sampleSize <- renderUI({ 
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 ) return("")
    if(denull(input$tabs) != "Facets") return("")
    n <- nrow(df)
    if(n < 10000) return("")
    sliderInput("sampleSize", "Sample Size", 10000, n, 10000) 
  })
  
  #INPUT -------------------- Button to compute the stats
  output$action <- renderUI({
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 ) return("")
    if (!is.null(input$targetVarRangeIn) && length(input$targetVarRangeIn) != 2) return ("")
    if(denull(input$tabs) != "Facets") return("")
    actionButton("action","Compute")
  })
  
  #INPUT -------------------- Choose a display for the distribution --------------------------------
  output$distroRadio <- renderUI({ 
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 ) return("")
    if(denull(input$tabs) != "Distribution") return("")
    radio_choices <- c("All ratings"="all","Matched Ratings"="matched","Expected (+) vs Actual Matches"="expected")
    select <- isolate(input$distroRadio) # save the value
    
    radioButtons("distroRadio", "Graph Mode", radio_choices, selected = select, inline = FALSE, width = NULL)
  })
  
  #INPUT -------------------- Choose a simulation size for the estimated kappa --------------------------------
  output$simSize <- renderUI({ 
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 ) return("")
    if(denull(input$tabs) != "Distribution") return("")
    if(denull(input$distroRadio) != "sim") return("")
    sliderInput("simSize", "Simulation Size", 1, 100, 1) 
  })
  
  #INPUT -------------------- Button to recompute simulation
  output$runSim <- renderUI({
    if (!rvals$dataLoaded || denull(input$targetVar) == 0 ) return("")
    if(denull(input$tabs) != "Distribution") return("")
    if(denull(input$distroRadio) != "sim") return("")
    actionButton("runSim","Compute Graph")
  })
  
  
  #OUTPUT-------------------------------graph the agreement
  output$facetGraph <- renderPlot({
    if(denull(input$action[1],0)==0) return() # tied to action button
    
    if (denull(isolate(input$sampleSize)) > 1){
      subsample <- sample(1:nrow(df),isolate(input$sampleSize))
      df <- df[subsample,]
    }
    
    n_matrix <- as.data.frame.matrix(table(df[[SID]],df[[input$targetVar]]) )
    
    if (!is.null(isolate(input$targetVarRangeIn))) { #zoom
      zoom = which(names(n_matrix) %in% isolate(input$targetVarRangeIn))
      size = 5
    } else {
      zoom = NULL
      size = 3
    }
   
    lambda_graph_facets(n_matrix,text_size=size,zoom = zoom)
  })
  
  #OUTPUT--------------------------------------distribution graphs ----------------------------------
  output$distGraph <- renderPlot({
    
     n_sims <- denull(isolate(input$simSize),1)
     target <- denull(input$targetVar)
    if(denull(input$distroRadio) == "all") {
      tbl<- table(df[[target]])
      tdf <- data.frame(Response = names(tbl), Count = tbl[1:length(tbl)])
      avg <- sum(tdf$Count) / nrow(tdf)
  
      ggplot(tdf,aes(x=Response, y = Count)) + geom_bar(stat="identity",fill="#777777") + geom_hline(yintercept = avg)
    } else if (denull(input$distroRadio) == "matched") {
      
      tbl <- table(df[[SID]],df[[target]])
      tbl <- tbl[rowSums(tbl) > 1,]

      tdf <- data.frame(Response = colnames(tbl), Count = colSums(tbl))
      avg <- sum(tdf$Count) / nrow(tdf)
      
      ggplot(tdf,aes(x=Response, y = Count)) + geom_bar(stat="identity",fill="#777777") + geom_hline(yintercept = avg)
    } else if (denull(input$distroRadio) == "sim") { # expected versus actual simulation
      if (denull(input$runSim)==0) return("") #dependency on the button
        tbl <- table(df[[SID]],df[[target]])
        tbl <- tbl[rowSums(tbl) > 1,]
        
        #simulate expected matches
        total <- 0
        total_list <- c()
        total_denom <- 0
        nr <- rowSums(tbl)
        pr <- colSums(tbl)/sum(tbl)
        progress_inc <- 1/(length(nr)*n_sims)
        # Progress Bar ################
        withProgress(message = 'Calculating', value = 0, {
          for(sim in 1:n_sims) {
            sim_total <- 0
            denom <- 0
            for(i in 1:length(nr)){
              d <- rmultinom(1,size = nr[i], prob = pr) #random multinomial based on global distribution and local sample size
              denom <- denom + (d>0) # we won't average in the zeros
              sim_total <- sim_total + (d / nr[i])^2 
              incProgress(progress_inc)
            }
            total_list <- c(total_list,sim_total/denom)
            total <- total + sim_total/denom
          }
        })
        sim_matrix <- matrix(total_list,length(sim_total))
        
        expected <- total / n_sims
        actual <- colSums(( tbl / rowSums(tbl) )^2)/colSums(matrix(tbl > 0, ncol = ncol(tbl))) # omit zeros from calculation
        ps <- rowSums(sim_matrix > actual)/n_sims # estimate a p-value for each column
        
        bonus <- paste0(c("-","+")[(actual > expected) + 1],round((actual - expected)*100),'%') # p=',round(ps,2))
        
        expected_prob_total <- sum(expected * pr) 
        actual_prob_total <- sum(actual * pr)  
        kappa <- (actual_prob_total-expected_prob_total)/(1 - expected_prob_total)
        title <- paste("Estimated Asymptotic Kappa =",round(kappa,2))
        
        tdf <- data.frame(Response = colnames(tbl), Match_Rate = actual, Expected = expected,label = bonus)
    
          ggplot(tdf,aes(x=Response, y = Match_Rate)) + geom_bar(stat="identity",fill="#777777") + 
          geom_point(aes(y = Expected),shape=3,color="black",size = 5 ) + ggtitle(title) +
          geom_text(aes(label = label), vjust = -.5)
    } else if(denull(input$distroRadio) == "expected"){
        # this code is based on that in library(irr) in  kappam.fleiss(ratings, exact = FALSE, detail = TRUE)
        tbl <- table(df[[SID]],df[[target]])
        tbl <- tbl[rowSums(tbl) > 1,]
        N <- sum(tbl)
        nr <- rowSums(tbl)
        coln <- colSums(tbl)
        pr <- coln / N
        
        #nro <- N / nrow(tbl) # original forumla assumes constant number of ratings per subject
        
        pj <- apply(tbl, 2, sum)/N # column proportions (chance a particular response is picked)
        
        # for each row of data, corresponding to one subject, we do this:
        # we choose one rating from the n_k in column k to see if it will be matched by the next one we draw
        # the chances of that are however many are left in the kth column (n_k - 1) divided by the total left
        # in the row, so n_k*(n_k-1)/(n-1), where n is the row total. Then we condition on the kth column by
        # Pr[match  | column = k] = Pr[match and column = k] / Pr[column = k], so we have to now divide by
        # the probability of picking the first one in column k, which is sum of col k divided by the total. 
        # the original formula cannot handle varying row sizes
        #pjko <- (apply(tbl^2, 2, sum) - N * pj)/(N * (nro - 1) * pj) # original formula, with fixed raters
        
        pjk <- colSums(t(t(tbl^2 - tbl)/(nr-1))) / coln  # formula adjusted for varying number of ratings per subject
        
        kappaK <- (pjk - pj)/(1 - pj)
        
        bonus <- paste0(c("","+")[(pjk > pj) + 1],round((pjk - pj)*100),'% K=',round(kappaK,2))
        
        title <- paste("Conditional Fleiss Kappas")
        
        tdf <- data.frame(Response = colnames(tbl), Match_Rate = pjk, Expected = pj,label = bonus,stringsAsFactors = FALSE)
        
        ggplot(tdf,aes(x=Response, y = Match_Rate)) + geom_bar(stat="identity",fill="#777777") + 
          geom_point(aes(y = Expected),shape=3,color="black",size = 5 ) + ggtitle(title) +
          geom_text(aes(label = label), vjust = -.5)
      }
  })
  
  #OUTPUT-----------------------------text to give Fisher's result--------------------------------------
  output$chiSquared <- renderText({
    if(denull(input$distroRadio) == "all") {
      tbl<- table(df[[input$targetVar]])
      f <- chisq.test(tbl)
      paste0("Chi-squared test versus uniform distribution, (X^2,DF,p-value) = (",round(f$statistic,3),",",f$parameter,",",round(f$p.value,3),")")
    }  else if (denull(input$distroRadio) == "matched") {
      tbl <- table(df[[SID]],df[[input$targetVar]])
      tbl <- tbl[rowSums(tbl) > 1,]
      
      f <- chisq.test(colSums(tbl))
      paste0("Chi-squared test versus uniform distribution, (X^2,DF,p-value) = (",round(f$statistic,3),",",f$parameter,",",round(f$p.value,3),")")
      
    } else { return("The marks indicate where agreement levels would be expected if raters randomly assigned values based on the distribution
                      of the data. The bars are the actual (non-asymptotic) match rates, and the numbers at the top give the difference in the 
                      percentages matched. K is the conditional kappa for the response type in that column.")}
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
