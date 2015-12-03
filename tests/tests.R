library(devtools)
devtools::document()
# roxygen2::roxygenise()
devtools::build()
devtools::install()
devtools::load_all()
# library(elm)
data = read.csv("/Users/mugiro/01work/elm/tests/testthat/hydrocarbons.csv")
X = as.matrix(data[,c(2:5)])
Y = as.matrix(data[,6])
object = new("ELM")

object = ELM()

# inputs(object) = ncol(X)
# neurons(object)=4
# outputs(object)= ncol(Y)
object = new("SLFN", inputs = ncol(as.matrix(X)), outputs = ncol(as.matrix(Y)))
inputs(object); w_out(object); h_neurons(object)

elm::act_fun()

# object = addNeurons(object, type = "sigmoid", number = 10)

nType = vector(); nType[1] = "linear"
nNumber = vector(); nNumber[1] = 5
for (i in 1:length(nType)){ # call addNeurons
  object = addNeurons(object, nType[i], nNumber[i])
}
neurons(object)
X=X[-1,]

if (checkingXY(object, X = X, Y = Y)) print("dhskfdsha")

checkingXY(object, X = Xv, Y = Yv)

is(X)

train(object, X = X, Y = Y)

inputs(object)

# X = seq(0, 7, 0.01)
# Y = sin(X)
plot(X,Y)



# a = trainELM(X = X, Y = Y, nType = "sigmoid", nNumber = 20)
a = trainELM(X = X, Y = Y, nType = c("sigmoid", "tanH"), nNumber = c(20, 10))


print(object)
IN=inputs(object)
X= matrix(c(rep(1,IN),
            rep(2,IN),
            rep(3,IN),
            rep(4,IN),
            rep(5,IN)),ncol=IN,byrow=T)
X
act(object)
H=project(object,X)

Wout(object) = matrix(rnorm(neurons(object)*outputs(object), mean=0, sd=1),
                      nrow=neurons(object), ncol=outputs(object))
Y=predict(object,X)

X = diag(x = rnorm(n = 4,mean = 1,sd = 0),nrow = 2,ncol = 2)
ref = as.array(c(2,1))
distMatVect(X, ref, type = "euclidean")
x = X[1,]
V =var(x,ref)

H0 = diag(x = rnorm(n = 9,mean = 1,sd = 0),nrow = 3,ncol = 3)
B = as.array(c(2,1,1))
H0 = diag(x = rnorm(n = 3,mean = 1,sd = 0),nrow = 3,ncol = 1)
B = as.array(c(2))
H0 + matrix(rep(B, nrow(H0)), nrow = nrow(H0), byrow = TRUE)
B0 = matrix(B,ncol = length(B),byrow=TRUE)
apply(H0, 1, function(x) {x + B0} )


B
A=matrix(B,ncol = length(B))
dim(A)
n=10

for (i in 1:n) print(i)
for (i in n) print(i)




######################################################################
# Create the first quadrant class
#
# This is used to represent a coordinate in the first quadrant.
FirstQuadrant <- setClass(
  # Set the name for the class
  "FirstQuadrant",

  # Define the slots
  slots = c(
    x = "numeric",
    y = "numeric"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    x = 0.0,
    y = 0.0
  ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)


new("FirstQuadrant")

object = FirstQuadrant

new("hiddenlayer")

hiddenlayer()

setClass("hiddenlayer",  # Definition of Single-hidden Layer Feed-forward Network SLFN
         slots = c(act_fun = "factor",
                   w_in = "matrix",
                   b = "numeric"),
         prototype = list()
)

