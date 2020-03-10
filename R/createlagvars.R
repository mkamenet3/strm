#' Create lagged and transformed variables 
#' @title 
#' createlagvars
#' @description Creates lagged explanatory and response variables for data in long format.
#' @param data Name of dataframe.
#' @param vars Response and explanatory variables to be lagged.
#' @param id Group identifier (example: state).
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @param wide Boolean indicator. Takes \code{TRUE} if data is in wide format and \code{FALSE} if data is in long format. Default is \code{FALSE}.
#' @param ... Arguments to be passed to \code{dplyr::filter()}
#' @export
#' @examples 
#' \donttest{
#'  outdf<- createlagvars(data = Produc, vars=c(y,xs), id="state", time=2,year==1970 | year==1971)}
createlagvars <- function(data, vars, id, time=time, wide, ...){
    #print(paste0("time: ",time))
    filter_args <- rlang::enquos(...)
    keepvars <- c(vars, rlang::as_name(id))
    id <- rlang::enquo(id)
    timeseq <- seq(time-1)
    outdf <- data %>%
                dplyr::filter(!!!filter_args) %>%
                dplyr::select(!!keepvars)
    if (wide == FALSE){
        lags <- NULL
        outdf <- suppressWarnings(outdf%>%
            tidyr::nest(-!!id) %>%
            dplyr::mutate(lags = purrr::map(data, function(dat){
                purrr::imap_dfc(dat, ~purrr::set_names(purrr::map(timeseq, lag, x= .x),
                                                       paste0(.y,".",'Tlag',timeseq)))
                # purrr::imap_dfc(dat, ~purrr::set_names(purrr::map(time, lag, x= .x),
                #                                        paste0(.y,".",'Tlag',lag)))
             })) %>%
            tidyr::unnest(cols=c(data,lags)) %>%
            dplyr::filter(complete.cases(.)))
        return(outdf)
    } else {
        return(outdf)
    }
}