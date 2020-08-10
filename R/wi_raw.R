#' wi_raw sf dataframe: Wisconsin 5-Year County-Level Raw ACS Data
#' 
#' We use two 5-year ACS county level data in Wisconsin downloaded using the tidycensus R package. The example uses raw 5-year estimates from 2013-2017 and 2014-2018 ACS data at the county-level in Wisconsin. The variables downloaded are:
#'  \itemize{
#'        \item `B17020_002` - Estimate: Total - Income in the past 12 months below poverty level
#'        \item `B17020_001` - Estimate: Total - Poverty Status in the past 12 months.
#'        \item `B23022_026` - Estimate: Total Female by Work Status by weeks worked in the past 12 months for the population 16-64 years old.
#'        \item `B23022_001` - Estimate: Total: status in the past 12 months by usual hours worked per week in the past 12 months by weeks worked in the past 12 months for the population 16-64 years old (Male and Female)
#' }
#' @docType data
#' @usage data(wi_raw)
#' @keywords datasets
#' @references Kyle Walker (2020). tidycensus: Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames. R package version 0.9.9.5.https://CRAN.R-project.org/package=tidycensus
#' @examples 
#' data(wi_raw)
#' class(wi_raw)
#' names(wi_raw)
"wi_raw"