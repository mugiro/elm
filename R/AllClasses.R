# AllClasses.R

# Definition of the main class
elm = setClass("elm",
                   slots = c(inputs = "numeric",
                             activation = "character",
                             type = "character",
                             bigdata = "logical"))

# Testing the devtools
elm1 = elm(inputs = 4,
           activation = "sigmoid",
           type = "I-ELM",
           bigdata = FALSE)
