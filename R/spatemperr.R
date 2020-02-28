#' Spatio-temporal regression model
#' @title 
#' spatemperr
#' @description The \code{spatemperr} function provides maximum likelihood estimation of a spatio-temporal simultaneous autoregressive lag error model. This package is built on the \code{lagsarlm()} function from the \code{spatialreg} package.
#' @param formula Model formula specified by user (without lags). Any transformed variables, such as logged-variables, must be created outside of this function in the dataframe.
#' @param id Group identifier (example: state).
#' @param data Name of dataframe.
#' @param listw Weights list object.
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @example 
#' \donttest{
#' formula <- as.formula( gsp_log  ~ pcap_log + pc_log + emp_log + unemp)
#' spatemperr(formula2, time=2)}
spatemperr <- function(formula, id,data, listw,time=2){
    if(time==1){
        warning("You have set time = 1, indicating a spatial error model. No temporal component will be assessed.")
        #TODO: perform regular lagsarlm model
    }
    else {
        formin <- formula
        print(formin)
        #add in temporally lagged response and temporally lagged explanatory variable into formula
        ##add in time lagged response
        allvars <- all.vars(formin)
        y <- allvars[[1]]
        xs <- allvars[-1]
        xs_lags <- rep(list(0), (time-1))# rep(NA, (length(xs)*(time-1)))
        y_lags <- rep(list(0), (time-1))
        for (i in 1:(time-1)){
            xs_lags[[i]] <- paste0(xs, "_lag",i)
            y_lags[[i]] <- paste0(y, "_lag",i)
        }
        xs_lags <- as.vector(unlist(xs_lags))
        y_lags <- as.vector(unlist(y_lags))
        
        rhs <- paste(c(xs,xs_lags, y_lags), collapse=" + ")
        formout <- as.formula(paste0(y," ~ ", rhs))
        print(formout)
    }
    
}