#'  Bayesian Change Point Plot
#'  
#'  Runs the BCP model, then simplifies the results in a ggplot object.
#'  
#'  @param y_name is the variable you're predicting.
#'  @param x_name (optional) is for establishing order (e.g. a date)
#'  @param data_arg is the data.frame which contains columns that can be pointed to by y_name and x_name.
#'  @param threshold is the probability of change needed to determine if a significant enough change has occured to create a new cluster
#'  @param plot_breaks can be changed when x_name refers to a date variable and you want the tick marks / scale of the plot to change.
#'  @param plot_title = plot title (sensible default provided).
#'  @param ... additional arguments passed to bcp.
#'  @import stringr
#'  @import scales
#'  @import ggplot2
#'  @importFrom bcp bcp
#'  @importFrom plyr ddply
#'  @export
bcp_plot <- function(
    y_name,
    x_name = NULL,
    data_arg, 
    threshold = .9,
    plot_breaks = "10 weeks",
    plot_title = paste0("Average Weekly ",ylabel,"\nWith Bayesian Change Point Trends"),
    ...)
{
    set.seed(55402)
    
    #  order data by x-variable (if exists), build bcp model
    if(is.null(x_name)){
        data_arg$null_x <- 1:nrow(data_arg)
        x_name <- "null_x"
    }
    data_arg <- data_arg[order(data_arg[,x_name]), c(x_name, y_name)]
    bcp_model <- bcp(data_arg[, y_name], ...)
    
    #  clean output for ploting
    bcp_df <- data.frame(x = data_arg[, x_name],
                         y = data_arg[, y_name],
                         prob_change = bcp_model$posterior.prob)
    bcp_df <- bcp_df[is.na(bcp_df$prob_change) == FALSE,]
    bcp_df$change_flag <- ifelse(bcp_df$prob_change > threshold, 1, 0)
    bcp_df$block <- cumsum(bcp_df$change_flag) + 1
    
    
    #  find block averages
    bcp_blocks1 <- ddply(bcp_df[,c("block", "x", "y")],
                         .(block),
                         summarise,
                         mean_y = mean(y), x = min(x))
    bcp_blocks2 <- ddply(bcp_df[,c("block", "x", "y")],
                         .(block),
                         summarise,
                         mean_y = mean(y), x = max(x))
    bcp_blocks <- rbind(bcp_blocks1, bcp_blocks2)
    bcp_blocks$label <- factor(
        paste0(toupper(letters[bcp_blocks$block]),
               ": ",
               round(100*bcp_blocks$mean_y),
               "%"))
    
    #  clean up plot labels
    ylabel <- str_replace_all(y_name, "_", " ")
    if(x_name == "null_x"){
        xlabel <-  NULL
    } else {
        xlabel <- str_replace_all(x_name, "_", " ")
    }
    
    #  return plot
    rval <- ggplot(bcp_df, aes(x, y)) +
        geom_point() +
        geom_line() +
        geom_line(data = bcp_blocks, aes(x, mean_y, colour = label),
                  size = 1) +
        theme_bw() +
        labs(title = plot_title,
             x = xlabel,
             y = ylabel,
             colour = "Group Average") +
        scale_y_continuous(labels = percent)
    #  modify x-axis if they are dates
    if(all(class(bcp_df$x) %in% c("POSIXct", "POSIXt"))){
        rval <- rval +
            scale_x_datetime(breaks = plot_breaks, labels = date_format("%m/%d/%y")) +
            theme(axis.text.x  = element_text(angle=45, hjust = 1, size = 8))
    }
    return(rval)
}