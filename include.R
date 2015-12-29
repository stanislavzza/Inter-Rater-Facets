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

library(ggplot2)

lambda <- function(n_matrix, column, column2) {
  # inputs: 
  # n_matrix= matrix with outcomes in columns and numbers of ratings in each row
  # column, column2 = the column pair to report lambda for
  # Note: this function does NOT remove bad rows. If you don't do that, lambda will report higher numbers than it should
  
  # output: a scalar, the interrater agreement statistic lambda
  
  N <- sum(n_matrix[,c(column,column2)])

  p_matrix <- data.frame(n_matrix[column]/N, n_matrix[column2]/N)
    
  sum(sqrt(rowSums(p_matrix*p_matrix)))
}


simulate_p_value <- function(n_matrix, column,column2,iterations){
  l_sim <- simulate_lambda(n_matrix,column,  column2,iterations)
  l_act <- lambda(n_matrix,column, column2) # warning! the n_matrix has to first be scrubbed of bad rows (count < 2)
  sum(l_sim > l_act)/iterations
}

simulate_lambda <- function(n_matrix, column,column2, iterations){
  #create sampled distributions randomly to get a sense of the variablility
  #http://stats.stackexchange.com/questions/67911/how-to-sample-from-a-discrete-distribution
  
  n_matrix <- n_matrix[,c(column, column2)] # just take what we need for this
  row_n <- rowSums(n_matrix)
  n_matrix <- n_matrix[row_n > 1,] 
  row_n <- row_n[row_n > 1] # remove the ones with no data
  N <- sum(n_matrix)
  p <- sum(n_matrix[1])/N
    
  ls <- c() # placeholder for lambdas
  
  for (it in 1:iterations) {
    sum <- 0
    for (i in 1:length(row_n)) { # generate simulated rows
      sim <- sample(x = c("1","2"), row_n[i], replace = T, prob = c(p,1-p))
      sim <- c(sim,c("1","2")) # hack to include both
      r <- (table(sim) - 1)/N # the decrement fixes it
      sum <- sum + sqrt(r[1]^2+r[2]^2)
    }
    ls <- c(ls,sum)
  }    
  ls
} 

lambda_graph_facets <- function(n_matrix,iterations=0,text_size=10,zoom=NULL, graph_max = 10000){
  results <- data.frame() # place to put graph information
  text_annotation <- data.frame()
  dotted <- data.frame() # reference line
  
  # do we zoom in?
  if (is.null(zoom)){
    column_range <- 1:length(n_matrix)
  } else {
    column_range <- zoom
  }
  withProgress(message = 'Calculating', value = 0, {
    prog_n <- 2 / ( length(column_range)*(length(column_range)+1) )
    
  for( column in column_range) { ######### choose column 1
    outcome1 <- names(n_matrix)[column]
    for( column2 in column_range) {   
        if (column <= column2) next
    
        outcome2 <- names(n_matrix)[column2]
        
        p_matrix <- data.frame( n_matrix[column], n_matrix[column2])
        
        names(p_matrix) <- c("inclass","outclass")
        
        # exclude rows with one or zero observations and normalize matrix
        p_matrix <- p_matrix[rowSums(p_matrix)>1,]
        
        # there might not be anything here....
        if (nrow(p_matrix)==0) next
        
        row_ns <- rowSums(p_matrix) # for use in computing the statisics
        N <- sum(p_matrix)
        p_1 <- sum(p_matrix$inclass) / N
        p_2 <- sum(p_matrix$outclass) / N
        
        # we can now calculate p_values
        ls <- lambda_stats_n(p_1,row_ns,graph=TRUE) #generate the lambda statistics
        l <- sum(sqrt(rowSums(p_matrix*p_matrix)))/N # actual length
        kappa <- (l - ls$lambda_mean)/(1- ls$lambda_mean)
        
        if (iterations > 0){ # generate p-value, which is expensive
          p_value <- simulate_p_value(p_matrix,1,2,iterations)
        } else {
          p_value <- pnorm(l,ls$lambda_mean,ls$lambda_se,lower.tail=FALSE)
        }
        
        p_matrix <- p_matrix/N
        
        # arrange the points for plotting
        p_matrix$slope <- p_matrix$outclass / p_matrix$inclass # R knows about infinity 
        p_matrix <- p_matrix[!is.nan(p_matrix$slope),] #shouldn't have any of these
        p_matrix <- p_matrix[order(p_matrix$slope),]
        p_matrix <- cumsum(p_matrix)
        
        #add stuff to graph
        p_matrix <- rbind(data.frame(inclass= c(0),outclass=c(0),slope=c(0)), p_matrix)
        p_matrix$col1 <- outcome1
        p_matrix$col2 <- outcome2
      
        # move the labels if zoomed in
        if (!is.null(zoom)){
          x_coord <- p_2 /2
          y_coord <- p_1/5
        } else {
          x_coord <- p_2 # save these to calculate best value
          y_coord <- p_1
        }
        
        t <- data.frame(p=round(p_value,3), k = round(kappa,2),N = N,col1 = outcome1,col2=outcome2,x=x_coord,y=y_coord,3) # x=.3,y=.15
        
        # ready to accumulate now
        results <- rbind(results,p_matrix)
        text_annotation <- rbind(text_annotation,t)
        
        dotted <- rbind(dotted, data.frame(x = ls$arc$x, y = ls$arc$y, col1 = outcome1,col2=outcome2))
        incProgress(prog_n)
    }
    
  }
  })
  
  # a huge data set takes forever to plot, so reduce it if that's the case
  if (nrow(results) > graph_max) { # 10000 by default
    subsample <- sample(1:nrow(results), graph_max)
    results <- results[subsample,]
  }
  
  if(is.null(zoom)) { # calculate placement of data
    x_coord <- max(text_annotation$x) / 2
    y_coord <- max(text_annotation$y) / 5
    text_annotation$x <- x_coord
    text_annotation$y <- y_coord
  }
  
  withProgress(message = 'Plotting', value = 0, {
  #require(grid) # in order to make arrows
  g <- ggplot(data = results, aes(x=outclass,y=inclass)) + 
    geom_text(aes(x=x,y=y,label=paste0("p = ",p, "\nK = ",k,"\nN = ",N) ),data=text_annotation,size=text_size,parse=FALSE) +
    geom_point(size = .5) +
    geom_line(data = dotted, mapping = aes(x = x, y= y), size = .5,linetype = 1, color = "gray") +
    theme_classic() + xlab(paste("Outcome")) + ylab(paste("Outcome")) +
    facet_grid(col1 ~ col2)
    
    incProgress(.5)
  
    print(g)
  })
}


lambda_stats_n <- function(p,counts, graph=FALSE){ 
  # returns the expected value for lambda given that p_1 = p (hence p_2 = 1-p) 
  # with a column of possibly different n rater rows
  # we imagine filling in a single subject's ratings with n, using binomial distribution
  
  # create a table to show how many rater-row types we have and how many of each
  row_ns <- as.data.frame(table(counts), stringsAsFactors = FALSE) # find out how many of each we have
  row_ns$counts <- as.numeric(row_ns$counts) 
  N <- sum(row_ns$Freq) 
  M <- sum(counts)
  arc_length <- 0
  
  stats <- data.frame() # to collect the statistics for each type, based on the number of raters in the row
  plot_data <- data.frame()
  for (i in 1:nrow(row_ns)){
      n <- row_ns$counts[i] # number of ratings on this row
      count <- row_ns$Freq[i] # how many rows total have the same n
      data <- data.frame(n=n,
                         count = count,
                         #bc= choose(n,seq(0,n)), # binomial coefficients
                         ylen = seq(n,0,-1),  # how many outcome 1s are in this row
                         xlen = seq(0,n,1)  # how many outcome 2s are in this row 
      )
      data$length <- sqrt(data$xlen^2 + data$ylen^2) /M # divide by M, the total number of ratings in our set
      
      #data$prob <- data$bc * p^data$ylen * (1-p)^data$xlen
      data$prob <- dbinom(seq(0,n), size=n, prob=1-p)
      
      lambda_mean <- sum(data$length*data$prob)
      lambda_var <- sum((data$length - lambda_mean)^2*data$prob)
      
      #accumulate statistics. 
      stats <- rbind(stats, data.frame(n=n,count=count,mean=lambda_mean,var=lambda_var))
      
      if (graph == TRUE){ #save the details for the graph coordinates
         plot_data <- rbind(plot_data, data)
      }
  }
  
  lambda_var <- sum(stats$var*stats$count) # total variance for the length
  lambda_mean <- sum(stats$count*stats$mean)
 
   #optionally, return the points for graphing
  if (graph == TRUE) {
      # sort the vectors by slope
      plot_data$slope <- plot_data$ylen / plot_data$xlen # R knows about infinity for sorting :-)
      plot_data <- plot_data[order(plot_data$slope,decreasing=TRUE),]
      
      # lengths time their probability
      plot_data$xlen <- plot_data$xlen * plot_data$prob / M
      plot_data$ylen <- plot_data$ylen * plot_data$prob / M
      plot_data$length <- plot_data$length * plot_data$prob   # length has already been divided by n

      # expand to account for multiplicity
      plot_data$xlen <- plot_data$xlen * plot_data$count
      plot_data$ylen <- plot_data$ylen * plot_data$count
      plot_data$length <- plot_data$length * plot_data$count
      arc_length <- arc_length + sum(plot_data$length)
      
      # create arc segments
      arc <- data.frame(x = cumsum(c(0,plot_data$xlen)), y = cumsum(c(0,plot_data$ylen)))
  } else {
    arc <- NULL
  }
  
  list(lambda_mean = lambda_mean, lambda_se = sqrt(lambda_var), arc = arc, arc_length = arc_length)
  
}

