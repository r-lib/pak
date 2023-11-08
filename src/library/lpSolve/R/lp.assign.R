lp.assign <- function (cost.mat, direction="min", presolve = 0, compute.sens = 0)
{
#
# lp.assign: use lpsolve.dll to solve an assignment problem. This
# is a linear program with an ixj matrix of decision variables,
# and i+j constraints: that the rows and columns all add up to one.
#
# Arguments:
#  cost.mat: matrix or data.frame of costs
#  direction: "min" (default) or "max"
#  presolve: numeric. Presolve? Default 0. Currently ignored.
#  compute.sens: numeric. Compute sensitivities? Default 0 (no).
#                Any non-zero number means "yes" and, in that
#                case, presolving is attempted.
#
# Return value: list from lpsolve, including objective and
# assignments.
#
# Check for the lpslink function, dyn.open if needed. (It should
# from data.frame if needed.
#
    if (!is.matrix(cost.mat))
        stop("Matrix of costs required.")
    if (is.data.frame(cost.mat))
        cost.mat <- as.matrix(cost.mat)
#
# Set up the stuff.
#
    nr <- nrow(cost.mat)
    nc <- ncol(cost.mat)
    rnum.signs <- rep (3, nr)
    row.rhs <- rep (1, nr)
    cnum.signs <- rep (3, nc)
    col.rhs <- rep (1, nc)
    if (direction == "min")
        direction <- as.integer(0)
    else
	if (direction == "max")
            direction <- as.integer (1)
        else
            stop ("Direction must be 'min' or 'max'")
    varcount <- as.integer(nr * nc)
    objective <- as.double(c(0, c(t(cost.mat))))
#
# Set up the row and column constraints. Each is of the
# "=1" type, represented by 3 (for "equals") 1.
#
    const.count <- as.integer(nr + nc)
    intcount <- as.integer(varcount) # number of integers
    intvec <- as.integer(1:varcount) # indicators of integers
#
# Prepare objective value, integer indicators, solution, and status
#
    objval <- as.double(0)
    int.count <- nc * nr
    integers <- as.integer (numeric (int.count))
    solution <- as.double(numeric(nc * nr))
    status <- as.integer(0)
#
# Set up sensitivity stuff
#
    sens.coef.from <- sens.coef.to <- 0
    duals <- duals.from <- duals.to <- 0
    if (compute.sens) {
        sens.coef.from <- sens.coef.to <- numeric(varcount)
        duals <- duals.from <- duals.to <- numeric(varcount +
            const.count)
    }
    ## costs <- as.double (c(0, c(cost.mat)))
    lps.out <- .C("lp_transbig",
        direction = direction,
        rcount = as.integer (nr),
        ccount = as.integer (nc),
        costs = objective,
        rsigns = as.integer (rnum.signs),
        rrhs = as.double (row.rhs),
	csigns = as.integer (cnum.signs),
        crhs = as.double (col.rhs),
	objval = objval,
        int.count = int.count, 
        integers = integers,
        solution = solution,
        presolve = as.integer(presolve),
        compute.sens = as.integer(compute.sens),
        sens.coef.from = as.double(sens.coef.from),
        sens.coef.to = as.double(sens.coef.to),
        duals = as.double(duals),
        duals.from = as.double(duals.from),
        duals.to = as.double(duals.to),
        status = status, PACKAGE="lpSolve")
#
# Reset solution back into matrix form.
#
    lps.out$solution = matrix(lps.out$solution, nr, nc, byrow = TRUE)
    if (length(duals) > 0) {
        lps.out$sens.coef.from <- matrix(lps.out$sens.coef.from,
            nr, nc, byrow = TRUE)
        lps.out$sens.coef.to <- matrix(lps.out$sens.coef.to,
            nr, nc, byrow = TRUE)
        lps.out$duals <- matrix(lps.out$duals, nr, nc, byrow = TRUE)
        lps.out$duals.from <- matrix(lps.out$duals.from, nr,
            nc, byrow = TRUE)
        lps.out$duals.to <- matrix(lps.out$duals.to, nr, nc,
            byrow = TRUE)
    }
#
# Reset the costs, to which we had to add a 0
#
    lps.out$costs <- cost.mat
    if(any(names(version) == "language"))
        class(lps.out) <- "lp"
    else oldClass(lps.out) <- "lp"
    lps.out
}
