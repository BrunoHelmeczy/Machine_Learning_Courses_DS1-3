# Function to check distinct values & class of all columns - anytime
  # Make sure your dataframe is called df - I didnt want to supply any inputs with purpose
ColsUniques <- function() {
  rbindlist(lapply(1:length(df), function(x) {
    tl <- list()
    tl[['name']] <- colnames(df)[x]
    tl[['distinct']] <- nrow(unique(df[,x]))
    tl[['class']] <- df[,x][[1]] %>% class()
    return(tl)
  }))  
}

# 2 functions to check all histograms / boxplots / LOESS-es in a dataset 
  # calls on previous function for column names
Hists <- function() {
  ColsUni2 <- ColsUniques()
  
  lapply(df %>% dplyr::select(-matches("id")) %>% colnames(),function(x) {
    if( ColsUni2$class[ColsUni2$name == x] %in% c("integer","numeric") ) {
      plot <- df %>% ggplot(aes_string(x = x)) + 
        geom_histogram( color = "red", fill = "blue") + theme_tufte() + 
        labs(title = paste0("NYC Properties ",x," Distribution")) 
      print(plot)
    } else if (ColsUni2$class[ColsUni2$name == x] %in% c("factor","logical") ) {
      plot <- df %>% ggplot(aes_string(x = x ) ) + 
        geom_bar(color = "red", fill = "blue") + coord_flip() + theme_tufte() + 
        labs(title = paste0("NYC Properties ",x," Distribution"))  
      print(plot)
    }
  })
  
}
# Hists()

# calls on ColsUniques for column names & checking data type
  # gives horizontally spaced box plots 
    # I tried ggridges for this too - nice but boxes are more distilled down, 
        # Ridges dont show quantiles or medians / means 
Boxes_n_Scatters <- function() {
  ColsUni1 <- ColsUniques()
  
  # Box Plots
  lapply(df %>% dplyr::select(-matches("id|logTotalValue")) %>%  colnames(), function(x) {
    if (  ColsUni1$class[ColsUni1$name == x] %in% c("factor","logical") ) {
      plot <- df %>% ggplot()  +
        geom_boxplot(aes_string(y = x, x = "logTotalValue", group = x),color = "red", fill = "blue") +
        theme_tufte() +  
        labs(title = paste0("Title ",x))
      print(plot)
      print(paste0("Printing plot: ",x))
    } 
  })
  
  # Scatters 
  xvars <- df %>% dplyr::select(
    matches(paste(ColsUni1$name[ColsUni1$class %in% 
                                  c("numeric","integer")],collapse = "|"))) %>%
    dplyr::select(-matches("id|logTotalValue")) %>% colnames()
  
  for (i in xvars) {
    plot <- df[,c(i,"logTotalValue")] %>% 
      ggplot(aes_string(x = i, y = "logTotalValue")) +
      geom_smooth() + geom_point(size = 0.1)  +
      labs(title = paste0("Title ",i))
    
    print(plot)  
    print(paste0("Printing plot: ",i))
  }
  
}
# Boxes_n_Scatters()
