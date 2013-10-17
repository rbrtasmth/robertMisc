#' Summarize a single variable by the levels of another variable. Default output is LaTeX table. Sample size also given.
#' 
#'  Example situation: how does average GPA (value) vary by Degree (group_by)?
#'  @param group_by name as a string of variable that is used like SQL group by.
#'  @param value name, as a string, of variable you want to summarize.
#'  @param data_arg a data frame containing value and group_by.
#'  @param func (optional, defaults to \code{\link{mean}}) function to use as summarize by.
#'  @param digits (optional) number of digits to print out in LaTeX table.
#'  @param caption (optional) caption for the LaTeX table.
#'  @param display (option) change to anything but "latex" to return a standard R table.
#'  @export
#'  @import xtable
#'  @importFrom plyr ddply
#'  @examples
#'  DF <- data.frame(x = rnorm(100), grp = c(rep("A", 50), rep("B", 50)))
#'  tbl_1var("grp", "x", DF, display = FALSE)
tbl_1var <- function(
    group_by,
    value,
    data_arg,
    func = mean,
    digits = 2,
    caption = NULL,
    display = "latex")
{
    group_ind <- match(group_by, names(data_arg))
    value_ind <- match(value, names(data_arg))
    
    temp_df <- data.frame(data_arg[,group_ind], data_arg[,value_ind])
    names(temp_df) <- c(group_by, value)
    tbl1 <- ddply(temp_df, c(group_by), colwise(func), na.rm = TRUE)
    
    temp_df <- data.frame(data_arg[,group_ind], 1)
    names(temp_df) <- c(group_by, "Sample Size")
    tbl2 <- ddply(temp_df, c(group_by), colwise(length))
    
    tbl <- merge(tbl1, tbl2, by = group_by)
    tbl <- tbl[order(tbl[,1]),]
    
    if(display == "latex"){  
        san_value <- gsub("_", " ", value, fixed=TRUE)
        san_group_by <- gsub("_", " ", group_by, fixed=TRUE)
        if(is.null(caption)) caption <- paste(as.character(body(func)[2]), "(", san_value, ") by ", san_group_by, ".", sep = "")
        xtbl <- xtable(tbl, type = "latex", digits = digits,
                       caption = caption)
        print(xtbl, include.rownames = FALSE, table.placement=getOption("xtable.table.placement", "H"),
              sanitize.text.function=function(x){
                  temp <- gsub("[", "{[}", x, fixed=TRUE)
                  temp2 <- gsub("_", " ", temp, fixed=TRUE)
                  return(temp2)
              }
        )
    } else tbl
}

#' Similar to \code{\link{tbl_1var}}), summarize two variables into a table. Default output is LaTeX table.
#' 
#' Create pretty LaTeX tables, yay!
#' 
#' @param var1 name, as a string, of groups to summarize rows of table by.
#' @param var2 name, as a string, of groups to summarize columns of table by.
#' @param value name, as a string, of variable to summarize.
#' @param data_arg a data.frame containing the variables var1, var2, and value.
#' @param func (optional) function to summarize by. Defaults to mean.
#' @param label (optional) label to add to LaTeX table.
#' @param digits (optional) number of digits to display in LaTeX table. Defaults to 2.
#' @param scale (optinoal) number to scale the size of the LaTeX table.
#' @param caption (optional) caption to LaTeX table.
#' @param display (optional) defaults to "latex", change to anything else to return standard R table.
#' @export
#' @import xtable
#' @examples
#' DF <- data.frame(x = rnorm(100),
#' grp1 = c(rep("A", 50),rep("B", 50)),
#' grp2 = rep(c(rep("C", 25), rep("D", 25)), 2))
#' tbl_2var("grp1", "grp2", "x", DF, display = FALSE)
tbl_2var <- function(var1, var2, value, data_arg, func = mean, label = NA, digits = 2, scale = NULL, caption = NULL, display = "latex")
{    
    if(is.na(label) == TRUE){
        label <- as.character(body(func)[2])
    }
    
    tbl <- tapply(data_arg[,c(value)], data_arg[c(var1, var2)], func)
    
    if(display == "latex"){
        san_var1 <- gsub("_", " ", var1, fixed=TRUE)
        san_var2 <- gsub("_", " ", var2, fixed=TRUE)
        san_value <- gsub("_", " ", value, fixed=TRUE)
        if(is.null(caption)) caption <- paste(label, " of ", san_value, ", rows = ", san_var1, " and columns = ", san_var2, ".", sep = "")
        xtbl <- xtable(tbl, digits = digits,
                       caption = caption)
        print(xtbl, scalebox = scale, table.placement=getOption("xtable.table.placement", "H"),
              sanitize.text.function=function(x){
                  temp <- gsub("[", "{[}", x, fixed=TRUE)
                  temp2 <- gsub("_", " ", temp, fixed=TRUE)
                  return(temp2)
              })
    } else tbl
}


#'  Dispay output of a table with percentage signs.
#'  
#'  Use this as the func argument in \code{\link{tbl_1var}}) or \code{\link{tbl_2var}}).
#'  @export
as_pct <- function(an_array){
    an_df <- as.data.frame(an_array)
    an_df <- an_df * 100
    rval <- sapply(an_df, function(something){
        sapply(something, function(val){
            if(is.na(val)) ""
            else paste(val, "%", sep = "")
        })
    })
    return(rval)
}

#'  Wrapper function to take just a simple data frame and put it in latex format with my preferred defaults.
#'  
#'  This wrapper function also includes a few more sanitization functions.
#'  
#'  @param xtbl a table to put in a LaTeX format 
#'  @param digits Number of digits, passed to  \code{\link{xtable}}'s \code{digits}
#'  @param caption A caption, passed to  \code{\link{xtable}}'s \code{caption}
#'  @param include_rownames Include row names? Defaults to \code{FALSE}.
#'  @param align Column alignment, passed to  \code{\link{xtable}}'s \code{align}
#'  @param ... Additional arguments passed to \code{\link{print.xtable}}
#'  @export
#'  @import xtable
#'  @examples
#'  require(xtable)
#'  xtable_wrap(data.frame(x = 1))
#'  xtable_wrap(data.frame(x = 1.5), digits = 0)
#'  xtable_wrap(data.frame(x = 1), caption = "Hello x")
#'  xtable_wrap(data.frame(x = "a", y = 0), align = c("NULL", "l", "c"))
xtable_wrap <- function(xtbl, digits = 2, caption = NULL, include_rownames = FALSE, align = NULL, ...){
    print(xtable(xtbl, type = "latex", digits = digits, caption = caption, align = align),
          include.rownames = include_rownames,
          table.placement=getOption("xtable.table.placement", "H"),
          sanitize.text.function=function(x){
              x2 <- gsub("[", "{[}", x, fixed = TRUE)
              x3 <- gsub("_", " ", x2, fixed = TRUE)
              x4 <- gsub("%", "\\%", x3, fixed = TRUE)
              return(x4)},
          ...
    )
}