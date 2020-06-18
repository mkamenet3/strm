library(testthat)
library(tidyr)
library(spdep)
context("Executing the createlagvars() function in strm.")
#set up
set.seed(2)
#create test dataset
y <- rnorm(n=100, mean = 50, sd=5)
x1 <- rnorm(n=100, mean = 200, sd=50)
x2 <- rnorm(n=100, mean = 10, sd=2)
id <- rep(1:20, each=5)
year <- rep(c(2000,2001,2002,2003,2004), times=20)
datf <- cbind.data.frame(y, x1, x2,id,year)
nb0 <- cell2nb(nrow=10,ncol=5, type="queen")
listw0 <- nb2listw(nb0, style="W")
y <- "y"
xs <- c("x1","x2")
#wide format
datf_wide <- datf %>% tidyr::pivot_wider(names_from = c(year),
                                         values_from = c(y,x1,x2))
nb1 <- cell2nb(nrow=5,ncol=4, type="queen")
listw1 <- nb2listw(nb1, style="W")
y_wide <- "y_2004"
xs_wide<- c("x1_2004", "x1_2003", "x1_2002", "x1_2001", "x1_2000", 
        "x2_2004", "x2_2003", "x2_2002", "x2_2001","x2_2000",  
        "y_2003", "y_2002", "y_2002", "y_2001",  "y_2000")



test_that("Filtering works correctly 1", {
    #expect no error
    expect_error(createlagvars(data = datf, vars=c(y,xs), id="id", time=2, wide=FALSE,
                               filter_options="year==2000 | year==2001"),NA)
 
})

test_that("Filtering works correctly 2", {
    #no filtering
    datfsub<- subset(datf, year==2000 | year==2001)
    expect_error(createlagvars(data = datfsub, vars=c(y,xs), id="id", time=2, wide=FALSE, filter_options=NULL),NA)
})

test_that("Filtering works correctly 3", {
    #no filtering
    datfsub<- subset(datf, year==2000 | year==2001)
    #this should give no error
    expect_error(createlagvars(data = datf_wide, vars=c(y_wide,xs_wide), id="id", time=5, wide=TRUE, filter_options=NULL), NA)
    
})
