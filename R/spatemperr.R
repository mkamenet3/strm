spatemperr <- function(formula2, data, listw, type, time=2){
    if(time==1){
        warning("You have set time = 1, indicating a spatial error model. No temporal component will be assessed.")
        #TODO: perform regular lagsarlm model
    }
    else {
        formin <- formula2
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