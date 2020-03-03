#' Spatio-temporal regression model
#' @title 
#' strm
#' @description The \code{strm} function provides maximum likelihood estimation of a spatio-temporal simultaneous autoregressive lag error model. This package is built on the \code{errorsarlm()} function from the \code{spatialreg} package.
#' @param formula Model formula specified by user (without lags). Any transformed variables, such as logged-variables, must be created outside of this function in the dataframe.
#' @param id Group identifier (example: state).
#' @param data Name of dataframe.
#' @param listw Weights list object.
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @param trans Transformation to be applied. Current implementation allows for \code{"log"}, \code{"sqrt"}, \code{"log10"}, and transformations.
#' @details Any transformed variables should be included in the formula statement as \code{variablename_transf}, where the transformation is indicated by an underscore \code{_} followed by a short name for the transformation. For example, to request \code{gdp} log-transformed, you would build the model formula as \code{gdp_log}.
#' @example 
#' \donttest{
#' data("Produc", package = "Ecdat")
#' data("usaww")
#' usalw <- mat2listw(usaww)
#' formula <- as.formula( gsp_log  ~ pcap_log + pc_log + emp_log + unemp)
#' out <- strm(formula, id="state", data=Produc, listw= usalw, time=2,trans="log",year==1970 | year==1971)}
strm <- function(formula, id,data, listw,time=2,trans,...){
    formin <- formula
    if (is.null(trans)){
        trans <- 0
    }
    allvars <- all.vars(formin)
    y <- allvars[[1]]
    xs <- allvars[-1]
    if(time==1){
        warning("You have set time = 1, indicating a spatial error model. No temporal component will be assessed.")
        outdf <- createlagvars(data = data, vars=c(y,xs), id=id, time=1, trans=trans, ...)
        #add in spatially lagged explanatory variables into formula
        #vars to NO spatially lag
        varsnoslag <- names(outdf)[grepl("Tlag",names(outdf), perl=TRUE)]
        
        xs_Slags <- as.data.frame(spatialreg::create_WX(as.matrix(outdf[,-which(names(outdf)%in% c(varsnoslag,id))]),
                                                        
                                                        listw=usalw, prefix="Slag"))
        #extract the names
        Slagvarskeep <- names(xs_Slags)[(sub('.*\\.','', names(xs_Slags)) %in% xs)]
        #add spatially lagged explanatory vars outdf
        outdf<- cbind.data.frame(outdf, xs_Slags)
        
        #Put formula together
        rhs <- paste(c(xs, Slagvarskeep), collapse=" + ")
        formout <- as.formula(paste0(y," ~ ", rhs))
        modframe <- model.frame(formout, data=outdf)
        #run spatial error model with spatially lagged explanatory vars
        message("The spatial regression model fitted: ")
        print(formout)
        res<- spatialreg::errorsarlm(modframe, data=outdf,
                       listw=listw)
    }
    else {
        outdf <- createlagvars(data = data, vars=c(y,xs), id=id, time=time, trans=trans, ...)
        #add in temporally lagged response and temporally lagged explanatory variable into formula
        xs_Tlags <- rep(list(0), (time-1))
        y_lags <- rep(list(0), (time-1))
        for (i in 1:(time-1)){
            xs_Tlags[[i]] <- paste0("Tlag",i, ".",xs)
            y_lags[[i]] <- paste0("Tlag",i, ".",y)
        }
        xs_Tlags <- as.vector(unlist(xs_Tlags))
        y_lags <- as.vector(unlist(y_lags))
        #add in spatially lagged explanatory variables into formula
        #vars to NO spatially lag
        varsnoslag <- names(outdf)[grepl("Tlag",names(outdf), perl=TRUE)]
        
        xs_Slags <- as.data.frame(spatialreg::create_WX(as.matrix(outdf[,-which(names(outdf)%in% c(varsnoslag,id))]),
                                                        
                                                        listw=usalw, prefix="Slag"))
        #extract the names
        Slagvarskeep <- names(xs_Slags)[(sub('.*\\.','', names(xs_Slags)) %in% xs)]
        #add spatially lagged explanatory vars outdf
        outdf<- cbind.data.frame(outdf, xs_Slags)
  
        #Put formula together
        rhs <- paste(c(xs,xs_Tlags, Slagvarskeep,y_lags), collapse=" + ")
        formout <- as.formula(paste0(y," ~ ", rhs))
        message("The spatio-temporal regression model fitted: ")
        print(formout)
        modframe <- model.frame(formout, data=outdf)
        #run ST regression model
        res<- spatialreg::errorsarlm(modframe, data=outdf,
                                   listw=listw)
        
    }
    return(res)
}



