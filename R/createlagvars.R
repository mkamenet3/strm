#' Create lagged and transformed variables 
#' @title 
#' createlagvars
#' @description Creates lagged explanatory and response variables for data in long format.
#' @param data Name of dataframe that has been transformed in strm (object \code{modframe0}).
#' @param vars Response and explanatory variables to be lagged.
#' @param id Group identifier (example: state).
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @param wide Boolean indicator. Takes \code{TRUE} if data is in wide format and \code{FALSE} if data is in long format. Default is \code{FALSE}.
#' @param filter_options Additional arguments to be passed to \code{dplyr::filter()}.
#' @import rlang 
#' @import dplyr 
#' @import tidyr 
#' @import purrr 
createlagvars <- function(data, vars, id, time=time, wide, filter_options){
    keepvars <- c(vars, rlang::as_name(id))
    id <- rlang::enquo(id)
    timeseq <- seq(time-1)
    if(is.null(filter_options)){
        outdf <- data %>%
            dplyr::select(!!keepvars)
    } else {
        outdf <- data %>%
            dplyr::filter(eval(parse(text = filter_options))) %>%
            dplyr::select(!!keepvars)
    }
    if (wide == FALSE){
        if(time>1){
            lags <- NULL
            outdf <- suppressWarnings(outdf%>%
                                          tidyr::nest(-!!id) %>%
                                          dplyr::mutate(lags = purrr::map(data, function(dat){
                                              purrr::imap_dfc(dat, ~purrr::set_names(purrr::map(timeseq, lag, x= .x),
                                                                                     paste0(.y,".",'Tlag',timeseq)))
                                          })) %>%
                                          tidyr::unnest(cols=c(data,lags)) %>%
                                          dplyr::filter(complete.cases(.)))
            return(outdf)
        } else {
            return(outdf)
        }
    } else {
        return(outdf)
    }
}