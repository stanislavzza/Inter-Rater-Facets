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

options(shiny.maxRequestSize = 200*1024^2) # allow large files

fluidPage(
    titlePanel("Inter-Rater Facets"),
    
    sidebarLayout(  
        sidebarPanel(
            #actionButton("console","server console"), # uncomment this to enable debugging <------------
            
            ########### Loading Data ##################################
            
            helpText(a("[See User Guide]",href="http://github.com/stanislavzza/Inter-Rater/wiki")),
            helpText("Send feedback to deubanks.office@gmail.com"),
            htmlOutput("dataFileUploader"),   # upload data files
            htmlOutput("dataType"),
            htmlOutput("subjectID"),
            htmlOutput("ratingNames"), # are ratings in their own columns?
            htmlOutput("ratingVals"), # values for the ratings
            htmlOutput("dataDoneButton"),     # button to signify finished loading
            
            ########### Analysis of Data ##############################
            
            htmlOutput("targetVar"),          # select the variable to predict
            htmlOutput("targetRangeIn"),    # select the inclass
            htmlOutput("sampleSize"),
            htmlOutput("normalizeCB"),
            htmlOutput("action"),
            htmlOutput("distroRadio")
        ), #end of sidebar panel
        
        mainPanel(
            # turn off the messages. They still appear in the console. 
            #tags$style(type="text/css",
            #           ".shiny-output-error { visibility: hidden; }",
            #           ".shiny-output-error:before { visibility: hidden; }"),
            tabsetPanel(id = "tabs", type = "tabs",
                        
                        tabPanel("Data", 
                                 fluidRow(
                                     column(12, tableOutput("dataPanelHeader")),
                                     column(12, textOutput("dataTypeInstructions")),
                                     column(12, tableOutput("dataTypeExample")),
                                     column(12, dataTableOutput("varStats"))
                                 )
                        ), # end of Data Tabpanel
                        tabPanel("Distribution",
                                 fluidRow(
                                     column(12, plotOutput('distGraph')),
                                     column(12, textOutput('chiSquared'))
                                 )
                        ),
                        tabPanel("Facets", 
                                 fluidRow(
                                     column(12, plotOutput('facetGraph',height = 650))
                                 )
                        )
            ) # end of tabsetPanel
        ), # end of main panel
        position = c("left", "right"),
        fluid = TRUE
    ) # end of sidebarLayout
) # end of fluidPage
