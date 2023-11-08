# This is the enclosing environment for all of the functions involved in
# instantiating objects. It is also the binding environment for all these
# functions, except for R6Class(). This is because a generator object can be
# saved (in a built package, for example) and then restored in a different R
# session which has a different version of the R6 package. With the capsule
# environment, the generator object doesn't need to use any functions or objects
# from the potentially different R6 namespace, and because the saved/restored
# object also saves and restores the capsule environment (but not the R6
# namespace).
capsule <- new.env(hash = FALSE)
attr(capsule, "name") <- "R6_capsule"

# This function takes an expression and evaluates it in the capsule environment.
encapsulate <- function(expr) {
  expr <- substitute(expr)
  eval(expr, capsule)
}


# This list contains functions that are copied to the generator environment and
# are assigned as the generator env as their enclosing environment.
# This is simpler than encapsulate, because these functions don't need to be
# enclosed in a special environment now; when a class is created, they will be
# copied into the generator environment and assigned it as their enclosing env.
generator_funs <- list()
