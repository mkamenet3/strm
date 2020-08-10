#' sptdmg3 SpatialPolygonsDataFrame: Wisconsin Minor Civil Divisions
#' 
#' We use the example from *Spatial Regression Models for the Social Sciences* Chi & Zhu (2019). The example uses population growth data from 2000 to 2010. Data are at the minor civil division (MCD) level in Wisconsin. There are two years of data: 2000 and 2010. The variables we will use are:
#'  \itemize{
#'      \item `LNP1000`: population growth from 2000 to 2010.
#'      \item `LNP0090`: population growth from 1990 to 2000.
#'      \item `POLD00`: percentage of the old population (age sixty-five and older) in 2000.
#'      \item `POLD90`: percentage of the old population (age sixty-five and older) in 1990.
#' }
#' @docType data
#' @usage data(sptdmg3)
#' @keywords datasets
#' @references Chi, Guangqing, and Jun Zhu. *Spatial Regression Models for the Social Sciences.* SAGE, 2019.
#' @examples 
#' data(sptdmg3)
#' class(sptdmg3)
#' names(sptdmg3)
"sptdmg3"