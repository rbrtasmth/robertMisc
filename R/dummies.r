#'  Create dummy variables for each level of a categorical variable
#'  @export
createDummies <- function(var, df_arg, keepNAs = TRUE)
{
    #  clean up variables names (if the categorical variable was created with ntq())
    level_names <- gsub("[", "", levels(df_arg[, var]), fixed = TRUE)
    level_names <- gsub("-", "neg", level_names, fixed = TRUE)
    level_names <- gsub(")", "", level_names, fixed = TRUE)
    level_names <- gsub(", ", "_", level_names, fixed = TRUE)
    level_names <- gsub("]", "", level_names, fixed = TRUE)
    
    for (i in seq(1, length(level_names))) {
        if(keepNAs) {
            df_arg[, paste(var,"_", level_names[i], sep = "")] <- ifelse(df_arg[, var] != levels(df_arg[, var])[i], 0, 1)
        } else {
            df_arg[, paste(var,"_", level_names[i], sep = "")] <- ifelse(df_arg[, var] != levels(df_arg[, var])[i] | is.na(df_arg[, var]) , 0, 1)     
        }
    }
    return(df_arg)
}
