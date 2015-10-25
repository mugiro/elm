library(elm)
context("Test an object and projection")

test_that("Creation of an SLFN object",{

#   load_all()
#   object= new("SLFN")
#   inputs(object)=3; d
#   neurons(object)=4; L
#   outputs(object)=1; c
#
#   print(object)
#   IN=inputs(object)
#   X= matrix(c(rep(1,IN),
#               rep(2,IN),
#               rep(3,IN),
#               rep(4,IN),
#               rep(5,IN)),ncol=IN,byrow=T)
#   X
#   act(object)
#   H=project(object,X)
#
#   beta(object) = matrix(rnorm(neurons(object)*outputs(object), mean=0, sd=1),
#                          nrow=neurons(object), ncol=outputs(object))
#   Y=predict(object,X)

  expect_equal(length("a"),1)

})


