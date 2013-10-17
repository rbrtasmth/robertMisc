#'  Numeric to quantiles (bin a variable)
#'  
#'  Use scale to change units (e.g. scale = 365 can convert bin labels from days to years)
#'  Change pretty to FALSE if you want bin labels to just be 1, 2, ..., n
#'  
#'  @param var_name name of variable to bin (passed as a string)
#'  @param quants number of quantiles to use
#'  @param data_arg data.frame containing var_name
#'  @export
ntq <- function(var_name, quants = 10, data_arg, scale = 1, digits = 2, pretty = TRUE){
    temp_df <- round(data_arg[,match(var_name, names(data_arg))]/scale, digits)
    cutoff <- unique(quantile(temp_df, probs = seq(0, 1, length = (quants+1)), na.rm = TRUE))
    
    if(length(cutoff) == 1){
        rval <- factor(rep(paste("[", cutoff, ", ", cutoff, "]", sep = ""), length(temp_df)))
    } else {
        if(pretty){
            n_breaks <- length(cutoff)
            var_levels <- rep(NA, (n_breaks))
            var_levels[n_breaks] <- "Unknown"
            for(i in 1:(n_breaks-1)){
                if(i < (n_breaks-1)){
                    var_levels[i] <- paste("[", round(cutoff[i], digits), ", ", round(cutoff[i+1], digits), ")", sep = "")
                }
                else {
                    var_levels[i] <- paste("[", round(cutoff[i], digits), ", ", round(cutoff[i+1], digits), "]", sep = "")
                }
            }
            
            rough_rval <- cut(temp_df, breaks = cutoff, labels = var_levels[1:(n_breaks-1)], right = FALSE, include.lowest = TRUE)
            rval <- droplevels(factor(ifelse(is.na(rough_rval) == TRUE,
                                             "Unknown",
                                             as.character(rough_rval)),
                                      levels = c(var_levels)))
        } else {
            rough_rval <- cut(temp_df, breaks = cutoff, right = FALSE, labels = FALSE, include.lowest = TRUE)
            rval <- droplevels(factor(ifelse(is.na(rough_rval) == TRUE,
                                             "Unknown",
                                             as.character(rough_rval)),
                                      levels = c(1:quants, "Unknown")))
        }
    }
    return(rval)
}



#'  fit_in_bin is used when predicting on a new data set when the trained model contains binned version of variables.
#'
#'  I.E. use when you want to predict on a new data set and the variables need to be binned the same way they were to create the model.
#'
#'  @param var the name (as a string) of the continuous variable you want to bin
#'  @param bin_labels a character vector of the binned labels (e.g. bin_labels = levels(my_training_data$my_binned_predictor)
#'  @param input_df the data set used to create the original binned version of the variable (var must be in this data)
#'  @param output_df the data set containing var you want to find the bin for.
#'  @param scale if there was a scale argument used in creating the bin.
#'  @param quants sometimes when binning a variable you specify a quants argument but because of the data you may end up with less that quants bins, use this agument for when this happens.
#'  @export
fit_in_bin <- function(var, bin_labels, input_df, output_df, scale = 1, quants = NULL)
{
    bin_labels <- unique(bin_labels[bin_labels != "Unknown"])
    if(is.null(quants)) quants <- length(bin_labels)
    
    bin_vals <- unique(quantile(input_df[,var]/scale, probs = seq(0, 1, length = (quants+1)), na.rm = TRUE))
    #  create a mini data frame with bins and values
    lkup <- data.frame(bin = bin_labels,
                       lower = bin_vals[1:length(bin_labels)],
                       upper = bin_vals[2:(length(bin_labels)+1)])
    #  Lookup the appropriate bin label
    rval <- factor(unlist(sapply(output_df[,var], function(val){
        if(is.na(val)){
            rturn <- "Unknown"
        } else if(val < max(lkup$upper) & val > min(lkup$lower)) {
            rturn <- as.character(lkup$bin[which(lkup$lower <= val & lkup$upper > val)])
        } else if(val >= max(lkup$upper)) {
            rturn <- as.character(lkup$bin[nrow(lkup)])
        } else if(val <= min(lkup$lower)) {
            rturn <- as.character(lkup$bin[1])
        }
        return(rturn)})),
                   levels = c(bin_labels, "Unknown"))
    
    return(rval)
}