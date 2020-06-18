#' Create lagged and transformed variables 
#' @title 
#' createlagvars
#' @description Creates lagged explanatory and response variables for data in long format.
#' @param data Name of dataframe that has been transformed in strm (object \code{modframe0}).
#' @param vars Response and explanatory variables to be lagged.
#' @param id Group identifier (example: state).
#' @param time Number of time periods in the dataset. Lags will be taken for each time period. Default is 2 time periods. For a spatial-only regression model, set \code{time=1}. 
#' @param wide Boolean indicator. Takes \code{TRUE} if data is in wide format and \code{FALSE} if data is in long format. Default is \code{FALSE}.
#' @param ... Arguments to be passed to \code{dplyr::filter()}
#' @export
#' @examples 
#' \donttest{
#'  outdf<- createlagvars(data = modframe0, vars=c(y,xs), id="state", time=2,wide = FALSE, filter_options="year==1970 | year==1971")}
createlagvars <- function(data, vars, id, time=time, wide, filter_options){
    print(filter_options)
#createlagvars <- function(data, vars, id, time=time, wide, ...){
    #filter_args <- rlang::enquos(...)
    keepvars <- c(vars, rlang::as_name(id))
    print(keepvars)
    id <- rlang::enquo(id)
    timeseq <- seq(time-1)
    
    
    if(is.null(filter_options)){
       print("a")
        outdf <- data %>%
            dplyr::select(!!keepvars)
        print("ok0")
    } else {
        print("b")
        outdf <- data %>%
            dplyr::filter(eval(parse(text = filter_options))) %>%
            dplyr::select(!!keepvars)
        print("oka")
    }
    #     #filter_args <- parse(text = filter_options)
    #     #print(filter_args)    
    #     outdf <- data %>%
    #         #dplyr::filter(!!!filter_args) %>%
    #         dplyr::filter(eval(parse(filter_options))) %>%
    #         dplyr::select(!!keepvars)
    # } else {
    #     outdf <- data %>%
    #         #dplyr::filter(!!!filter_args) %>%
    #         #dplyr::filter(eval(filter_args)) %>%
    #         dplyr::select(!!keepvars)
    # }
    # #filter_args <- rlang::enquos(filter_options)
    # 
   
    print("ok2")
    if (wide == FALSE){
        if(time>1){
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
    } else {
        return(outdf)
    }
}