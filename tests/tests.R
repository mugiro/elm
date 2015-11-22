library(devtools)
devtools::build()
devtools::install()
devtools::load_all()
library(elm)
data = read.csv("/Users/andres/00work/elm/tests/testthat/hydrocarbons.csv")
X = as.matrix(data[,c(2:5)])
Y = as.matrix(data[,6])
object = new("SLFN")
# inputs(object) = ncol(X)
# neurons(object)=4
# outputs(object)= ncol(Y)
object = new("SLFN", inputs = ncol(as.matrix(X)), outputs = ncol(as.matrix(Y)))

inputs(object); Wout(object)
addNeurons(object, type = "sigmoid", number = 20)

neurons(object)

nType = vector(); nType[1] = "sigmoid"
nNumber = vector(); nNumber[1] = 20
for (i in 1:length(nType)){ # call addNeurons
  object = addNeurons(object, nType[i], nNumber[i])
}

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
