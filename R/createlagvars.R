#' Create lagged and transformed variables 
#' @title 
#' createlagvars
#' @description
#' @param data Name of dataframe.
#' @param vars Response and explanatory variables to be lagged.
#' @param id Group identifier (example: state).
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @param trans Transformation to be applied. Current implementation allows for \code{"log"}, \code{"sqrt"}, \code{"log10"}, and transformations.
#' @example 
#' \donttest{
#'  res <- createlagvars(data = Produc, vars=c(y,xs), id="state", time=2, trans="log",year==1970 | year==1971)}
createlagvars <- function(data, vars, id, time=2, trans=NULL, ...){
    filter_args <- rlang::enquos(...)
    keepvars <- c(vars, id)
    id <- enquo(id)
    time <- time-1
    if(trans=="log"){
        outdf <- data %>%
            dplyr::filter(!!!filter_args) %>%
            dplyr::select(!!keepvars) %>% 
            dplyr::mutate_at(vars(-!!id), .funs = list(log = ~log(.)))
    }
    else if (trans=="sqrt"){
        outdf <- data %>%
            dplyr::filter(!!!filter_args) %>%
            dplyr::select(!!keepvars) %>% 
            dplyr::mutate_at(vars(-!!id), .funs = list(sqrt = ~sqrt(.)))
    }
    else if (trans=="log10"){
        outdf <- data %>%
            dplyr::filter(!!!filter_args) %>%
            dplyr::select(!!keepvars) %>% 
            dplyr::mutate_at(vars(-!!id), .funs = list(log10 = ~log(.,10)))
    } 
    else if(trans==0){
        message("No transformations")
        outdf <- data %>%
            dplyr::filter(!!!filter_args) %>%
            dplyr::select(!!keepvars)
    } else {
        stop(paste0("Your selected transformation, ",trans, " is not yet implemented. Please create an issue."))
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

# res <- createlagvars(data = Produc,
#                      vars=c(y,xs), id="state", time=1, trans="log",
#                      year==1970 | year==1971)

