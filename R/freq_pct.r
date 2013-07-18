#' @title Cross tablulation of frequencies and percentages
#'
#' @description
#' \code{freq_pct} returns a data frame containting cross tabulated frequencies
#' and column percentages for a given list of variables.
#' 
#' @details
#' ...
#' 
#' @param varlist a \code{\link{list}} of variables/vectors to tabulate.
#' @param crossvar a variable/vector to tablulate \code{varlist} across.
#' @param varlabs a character vector of length \code{length(varlist)} that 
#' contains variable labels.
#' @param useNA passed to \code{\link{table}}
#' @export 
freq_pct <- function(varlist, crossvar, varlabs, useNA = "no") {
  tablist <- lapply(X=varlist, FUN=table, crossvar, useNA = useNA)
  tab <- NULL
  for (i in 1:length(tablist)) {
    temp_frame <- as.data.frame.matrix(tablist[[i]])
                                
    # place the variable name in a vector so that we can add a column
    # to our "table" data frame
    varname <- c(varlabs[i], rep("", length.out = nrow(temp_frame) - 1))
    
    # value labels are the row names of the output from -table()
    vallabs <- rownames(temp_frame)
    rownames(temp_frame) <- NULL
    
    # calculate column percentages and create a formatted sub-table
    temp_p_frame <- as.data.frame.matrix(round(100*prop.table(tablist[[i]], 2), 
                                               0
                                         )
    )
    temp_combined_frame <- matrix("", 
                                  nrow = nrow(temp_frame), 
                                  ncol = ncol(temp_frame)
    )
    for (ii in 1:nrow(temp_combined_frame)) {
      for (jj in 1:ncol(temp_combined_frame)) {
        temp_combined_frame[ii,jj] <- paste(temp_frame[ii,jj],
                                            " (",
                                            temp_p_frame[ii,jj],
                                            ")",
                                            sep = ""
        )
      }
    }
    
    # combine formatted values with labels, append to table
    temp_table <- cbind(data.frame(var = varname, val = vallabs),
                        temp_combined_frame
    )
    tab <- rbind(tab, temp_table)
  }
  tab
}

