#' @title Cross tablulation of frequencies and percentages
#'
#' @description
#' \code{freq_pct} returns a data frame containting cross tabulated frequencies
#' and column percentages for a given list of variables.
#' 
#' @details
#' ...
freq_pct <- function(varlist, crossvar, varlabs, useNA = "no") {
  tablist <- lapply(X=varlist, FUN=table, crossvar, useNA = useNA)
  tab <- NULL
  for (i in 1:length(tablist)) {
    tab <- rbind(tab, tablist[[i]])
  }
  tab
}

