#' Create lagged and transformed variables 
#' @title 
#' createlagvars
#' @description
#' @param data Name of dataframe.
#' @param vars Response and explanatory variables to be lagged.
#' @param id Group identifier (example: state).
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @param trans Transformation to be applied. Current implementation allows for \code{"log"}, \code{"sqrt"}, \code{"log10"}, and transformations.
createlagvars <- function(data, vars, id, time=2, trans=NULL){
    keepvars <- c(vars, id)
    id <- enquo(id)
    time <- time-1
    if(trans=="log"){
        outdf <- data %>%
            dplyr::select(!!keepvars) %>% 
            dplyr::mutate_at(vars(-!!id), .funs = list(log = ~log(.)))
    }
    else if (trans=="sqrt"){
        outdf <- data %>%
            dplyr::select(!!keepvars) %>% 
            dplyr::mutate_at(vars(-!!id), .funs = list(sqrt = ~sqrt(.)))
    }
    else if (trans=="log10"){
        outdf <- data %>%
            dplyr::select(!!keepvars) %>% 
            dplyr::mutate_at(vars(-!!id), .funs = list(log10 = ~log(.,10)))
    } 
    else if (!is.null(trans)){
        stop(paste0("Your selected transformation, ",trans, " is not yet implemented. Please create an issue."))
    } else {
        message("No transformations")
    }
    outdf <- suppressWarnings(outdf%>%
        tidyr::nest(-!!id) %>%
        dplyr::mutate(lags = purrr::map(data, function(dat){
             purrr::imap_dfc(dat, ~set_names(map(time, lag, x= .x),
                                             paste0(.y, '_lag',time)))
         })) %>%
        tidyr::unnest(cols=c(data,lags)) %>%
        dplyr::filter(complete.cases(.)))
    return(outdf)
}

#subdat <-  subset(Produc, year==1970 | year==1971)
res <- createlagvars(data = subdat, 
                     vars=c(y,xs), id="state", time=2, trans="log")
                     #,
                     #time=2, trans=NULL, filter=NULL)






suppressWarnings(Produc %>%
                     dplyr::filter(year==1970|year==1971) %>%
                     dplyr::select(gsp, pcap, pc, emp, unemp,state) %>%
                     tidyr::nest(-state) %>%
                     dplyr::mutate(lags = purrr::map(data, function(dat){
                         purrr::imap_dfc(dat, ~set_names(map(1, lag, x= .x),
                                                         paste0(.y, '_lag',1)))
                     })) %>%
                     tidyr::unnest(cols=c(data,lags)) %>%
                     dplyr::filter(complete.cases(.))) %>%
    #if(isTRUE(log))
    dplyr::mutate_at(vars(-state), .funs = list(log = ~log(.,2)))