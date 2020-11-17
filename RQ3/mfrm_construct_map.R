
# mfrm_construct_map 

mfrm_construct_map <- function(theta_subj=NULL, alfa_rater=NULL, 
        beta_task=NULL, tresholds=NULL, min = -6, max =6, step=1){

  stopifnot(!is.null(theta_subj | alfa_rater | beta_task | step))
  

  library(ggplot2)
  library(tidyverse)
     
        
        
   theta_subj <- as.data.frame(theta_subj)     
   alfa_rater <- as.data.frame(alfa_rater)
   beta_task <- as.data.frame(beta_task)
   
   # subjects theta distributions
   p1 <- ggplot(theta_subj, aes(x=theta_subj)) + 
    geom_histogram(binwidth=.5, colour="black", fill="white") +
    theme_bw()  +
    scale_y_continuous(name = "", breaks = NULL) +
    scale_x_continuous(breaks=seq(min, max, step), limits=c(min, max)) + 
    theme( axis.title.x = element_blank(), axis.title.y= element_blank())+
    labs(title = expression(paste("Subject's ", theta))) + coord_flip()
   
   # item beta distributions 
#   p2 <- ggplot(beta_task, aes(x=beta_task)) + 
#     theme_bw() + geom_dotplot(binwidth = .1) +
#     scale_y_continuous(name = "", breaks = NULL) +
#     scale_x_continuous(breaks=seq(min, max, step), limits=c(min, max)) + 
#     theme(axis.title.x= element_blank(), axis.title.y= element_blank()) +
#     labs(title = expression(paste("Item's ", beta))) + coord_flip()
  
   # rater alpha distributions
   p3 <- ggplot(alfa_rater, aes(x=alfa_rater)) + 
     theme_bw() + geom_dotplot(binwidth = .1) +
     scale_y_continuous(name = "", breaks = NULL) +
     scale_x_continuous(breaks=seq(min, max, step), limits=c(min, max)) +  
     theme( axis.title.x= element_blank(), axis.title.y= element_blank())+
     labs(title = expression(paste("Rater's ", alpha))) + coord_flip()
 
   # Rater structure 
   
   theta <- seq(-6, 6, .1)
   thetas_df <- tibble(
           theta = theta
   )
   
   k <- 0: length(tresholds)
   out <- vector("list", length(k))
   
   for (i in seq_along(k)) {
           
           out[[i]] <- apply(
                   thetas_df[ , 1], MARGIN = 1, 
                   function(x) { 
                           prob_cat_gpcm(a = 1, tresholds = tresholds, 
                                   theta = x, k = k[i] )
                   }
           )
   }
   names(out) <- paste("r", k, sep="")
   
   
   p4 <- bind_cols(thetas_df, out) %>%
           gather(key = "item", value = "P", -theta) %>%
           ggplot(aes(x=theta, y=P)) + theme_bw() + 
           geom_line(aes(linetype=item), size=.5)  + 
           geom_point(aes(shape = item))   +
           scale_y_continuous(name = "", breaks = NULL) +
           scale_x_continuous(breaks=seq(-6, 6, 0.5), limits=c(-6, 6)) + 
           theme(legend.position=c(.4, .01), legend.direction="horizontal",
                   axis.title.x= element_blank(), axis.title.y= element_blank()) +
           labs(title = expression(paste("Scale's ", tau))) + 
           coord_flip() +
           geom_vline(xintercept = tresholds)

        
   multiplot(p1, p3, p4, cols=4)
        
  
        
}

# ____________________ Helper functions ___________________________

# prob_cat_gpcm 
#       
# Based on francis smart  
# Returns P(x=k) calcutate from from generalized partial credit model
# a is the discriminatory power of this item, 
# tresholds are the tau parameters for the item 
# theta is ability 
# k is the response category for P(x=k) is beeing calculated. First category needs to be zero
# D is 1.7, 
# see http://www.edmeasurementsurveys.com/TAM/Tutorials/5PartialCredit.htm     


# cal

prob_cat_gpcm <- function(a=1, tresholds, theta, k, D=1.7){
 
        numerator <- function(a, tresholds, theta, k, D) {
                if (k==0) {
                        return(1)  # Modified to calculate the firs category
                }       
                else {
                        exp(  
                                sum(  D*a*(theta-tresholds[1:k])
                                ) 
                        )
                }    
        }
        
        denominator <- function(a, tresholds, theta, D) {
                sumval <- 1  # Modified from original code
                for (i in 1:length(tresholds)){
                        sumval <- sumval + numerator(a,tresholds,theta,i,D)       
                }
                return(sumval)
        }
        
        numerator(a, tresholds, theta, k, D) / denominator(a, tresholds, theta, D)      
        
}


# Multiple plot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                        ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                layout.pos.col = matchidx$col))
                }
        }
}






