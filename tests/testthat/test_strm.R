library(testthat)
context("Executing the spatio-temporal regression model, strm.")
#set up
set.seed(2)
#create test dataset
y <- rnorm(n=100, mean = 50, sd=5)
x1 <- rnorm(n=100, mean = 200, sd=50)
x2 <- rnorm(n=100, mean = 10, sd=2)
id <- rep(1:50, each=2)
year <- rep(c(2000,2005), times=50)
datf <- cbind.data.frame(y, x1, x2,id,year)
nb0 <- cell2nb(nrow=10,ncol=5, type="queen")
listw0 <- nb2listw(nb0, style="W")



test_that("Other transformations that are in () are correctly handled", {

    #expect no error
    form0 <- as.formula(log(y) ~ x1 +x2)
    expect_error(strm(form0, id="id", data=datf, listw = listw0,
                 time=2, wide=FALSE),NA)
    #expect error
    form0 <- as.formula(y ~ x1 + I(x2^2))
    expect_error(strm(form0, id="id", data=datf, listw = listw0,
                      time=2, wide=FALSE))
    #expect no error
    form0 <- as.formula(sqrt(y) ~ x1 + x2)
    expect_error(strm(form0, id="id", data=datf, listw = listw0,
                      time=2, wide=FALSE), NA)
})

test_that("Check model structure", {
    form0 <- as.formula(sqrt(y) ~ x1 +x2)
    expect_message(strm(form0, id="id", data=datf, listw = listw0,
                      time=2, wide=FALSE), regexp = "The spatio-temporal regression model fitted:","sqrty ~ x1 + x2 + x1.Tlag1 + x2.Tlag1 + sqrty.Tlag1")
})

test_that("time=1 warning", {
    form0 <- as.formula(sqrt(y) ~ x1 + x2)
    datfsub<-subset(datf, year==2000)
    expect_warning(strm(form0, id="id", data=datfsub, listw = listw0,
                      time=1, wide=FALSE))
})

