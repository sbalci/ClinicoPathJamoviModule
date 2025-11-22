
library(jmvcore)
library(R6)

print("Checking jmvcore::Options")
opts <- tryCatch(jmvcore::Options$new(package="test", name="test"), error=function(e) e)
print(opts)

if (inherits(opts, "R6")) {
    print("Methods:")
    print(names(opts))
    print("Has addOption:")
    print(exists("addOption", envir=opts))
}

# Define a minimal class
TestOptions <- R6::R6Class(
    "TestOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function() {
            super$initialize(package="test", name="test")
            print("Initialized super")
            self$addOption(jmvcore::OptionBool$new("test", TRUE))
            print("Added option")
        }
    )
)

print("Instantiating TestOptions")
t <- TestOptions$new()
print("Done")
