#' Spatio-temporal regression model
#' @title 
#' strm
#' @description The \code{strm} function provides maximum likelihood estimation of a spatio-temporal simultaneous autoregressive lag error model. This package is built on the \code{errorsarlm()} function from the \code{spatialreg} package.
#' @param formula Model formula specified by user (without lags). Any transformed variables, such as logged-variables, should be specified in the model formula.
#' @param id Group identifier (example: state).
#' @param data Name of dataframe.
#' @param listw Spatial weights list object.
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @param wide Boolean indicator. Takes \code{TRUE} if data is in wide format and \code{FALSE} if data is in long format. If data is in wide format, it is assumed that the user is including the temporal lags for the explanatory variables and response variable manually. Default is \code{FALSE}.
#' @param filter_options Additional arguments to be passed to \code{dplyr::filter()}. Default is \code{NULL}.
#' @param returndf Logical. If modified dataframe should be returned. Default is \code{FALSE}.
#' @param ... Additional arguments to be passed to \code{spatialreg::errorsarlm()}.
#' @details Any transformed variables should be included in the formula statement. For example, to request \code{gdp} natural log-transformed, you would build the model formula as \code{log(gdp)}.
#' @export
#' @examples 
#' library(spdep)
#' library(dplyr)
#' data("Produc", package = "Ecdat")
#' data("usaww")
#' usalw <- mat2listw(usaww)
#' formula <- as.formula( log(gsp)  ~ log(pcap) + log(pc) + log(emp) + unemp)
#' out1 <- strm(formula, id="state", data=Produc, 
#' listw = usalw, time=2,wide=FALSE,
#' filter_options="year==1970 | year==1971")
#' out2 <- strm(formula, id="state", data=Produc, 
#' listw= usalw, time=2, wide = FALSE, 
#' filter_options="year==1970 | year==1971",
#'  method="Chebyshev")
#' @importFrom spatialreg errorsarlm
#' @importFrom stats as.formula 
#' @importFrom stats model.frame 
#' @importFrom stats terms
strm <- function(formula, id,data, listw,time=2,wide=FALSE,filter_options=NULL, returndf=FALSE,...){
    formin <- formula
    if(missing(wide) | wide == FALSE){
        wide <- FALSE
    } else {
        warning("Data is in wide format. strm assumes you include the temporally-lagged explanatory variables manually.")
        wide <- wide
    }
    if (returndf==TRUE){
        returndf <- TRUE
    } else {
        returndf <- FALSE
    }
    y <- deparse(formula(formin)[[2]])
    xs <- attributes(terms(formin))$term.labels
    #combine transformed data frame with original dataframe (so any other filtering can be passed to createlagvars)
    modframe0 <- cbind.data.frame(model.frame(formin, data=data),  data[,which(names(data)%in% c(y,xs)==FALSE)])
    if(time==1){
        warning("You have set time = 1, indicating a spatial error model. No temporal component will be assessed.")
        outdf <- createlagvars(data = modframe0, vars=c(y,xs), id=id, time=1,wide, filter_options)
        #Put formula together
        rhs <- paste(c(xs), collapse=" + ")
        #clean out any transformed variable names
        outdfnames <- names(outdf)
        outdfnamesclean <- gsub("*\\(*)*","",outdfnames)
        names(outdf) <- outdfnamesclean
        y <- gsub("*\\(*)*","",y)
        rhs <- gsub("*\\(*)*","",rhs)
        
        formout <- as.formula(paste0(y," ~ ", rhs))
        #run spatial error model with spatially lagged explanatory vars
        message("The spatio-temporal regression model fitted:")
        message(deparse(formout))
        modframe <- model.frame(formout, data=outdf)
        res<- spatialreg::errorsarlm(modframe,listw=listw,...)
    }
    else {
        if (wide==FALSE){
            outdf <- createlagvars(data = modframe0, vars=c(y,xs), id=id, time=time,wide, filter_options)
            #add in temporally lagged response and temporally lagged explanatory variable into formula
            xs_Tlags <- rep(list(0), (time-1))
            y_lags <- rep(list(0), (time-1))
            for (i in 1:(time-1)){
                xs_Tlags[[i]] <- paste0(xs,".","Tlag",i)
                y_lags[[i]] <- paste0(y,".","Tlag",i)
            }
            xs_Tlags <- as.vector(unlist(xs_Tlags))
            y_lags <- as.vector(unlist(y_lags))
            #Put formula together
            rhs <- paste(c(xs,xs_Tlags, y_lags), collapse=" + ")
        } else {
            outdf <- createlagvars(data = modframe0, vars=c(y,xs), id=id, time=time,wide, filter_options)
            #Put formula together
            rhs <- paste(c(xs), collapse=" + ")
        }
        #clean out any transformed variable names
        outdfnames <- names(outdf)
        outdfnamesclean <- gsub("*\\(*)*","",outdfnames)
        names(outdf) <- outdfnamesclean
        y <- gsub("*\\(*)*","",y)
        rhs <- gsub("*\\(*)*","",rhs)
    
        formout <- as.formula(paste0(y," ~ ", rhs))
        #message("The spatio-temporal regression model fitted:")
        message("The spatio-temporal regression model fitted:")
        message(deparse(formout))
        modframe <- model.frame(formout, data=outdf)
        res<- spatialreg::errorsarlm(modframe, listw=listw, ...)
    }
    if(returndf == TRUE){
        return(list(res=res,
                    modframe = modframe))
    } else {
        return(res)
    }
}