# Inter-Rater
An Open Source R/Shiny app for assessing the inter-rater agreement for nominal or ordinal scales. 

## Installation
Everything that you need is free, and can be downloaded in a few minutes.
1. R from http://www.r-project.org/
2. RStudio from http://www.rstudio.com/products/rstudio/download/
3. The three source files in this repository that end with .R. 

Create a folder somewhere called Survey Prospector, and put the three .R files in it. Then launch RStudio and load the necessary libraries by running at the command prompt (lower left window):

install.packages(c("shiny","tidyr","ggplot2"))

Now use the File menu to find and open ui.R in the Inter-Rater folder. Look for a green arrow with "Run App" near the top of the screen. Click that to run the app.

For instructions and implementation notes, see http://highered.blogspot.com/2015/12/inter-rater-facets.html
