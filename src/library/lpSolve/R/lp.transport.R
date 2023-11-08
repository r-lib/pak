lp.transport <- function (cost.mat, direction = "min", row.signs, row.rhs, col.signs,
    col.rhs, presolve = 0, compute.sens = 0, integers = 1:(nc * nr))
{
#
# lp.transport: use lpsolve.dll to solve a transportation problem.
# This is a linear program with an ixj matrix of decision variables,
# and constraints on the row and column sums (and no others)
#
# Arguments: cost.mat: matrix or data.frame of costs
#                 dir: direction ("min" or "max")
#           row.signs: signs for row constraints
#             row.rhs: values for row constraints
#           col.signs: signs for column constraints
#             col.rhs: values for column constraints
#            presolve: Numeric: should we presolve? Default 0 (no); non-0
#                      values means "yes." Currently mostly ignored.
#        compute.sens: Numeric: compute sensitivities? Default 0 (no);
#                      non-zero value means "yes."
#            integers: Indicator of integer variables: default, all.
#
# Return value: list from lpsolve, including objective and optimal values.
#
# Check that the cost matrix is in fact a matrix; convert
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
	#
	# Ensure that row stuff is of the correct size.
	#
    if (is.matrix(row.signs))
        row.signs <- as.vector(row.signs)
    if (length(row.signs) != nr)
        stop(paste("Error: We have", length(row.signs), "signs, but",
            nr, "rows"))
    if (is.matrix(row.rhs))
        row.rhs <- as.vector(row.rhs)
    if (length(row.rhs) != nr)
        stop(paste("Error: We have", length(row.rhs), "rhs's, but",
            nr, "rows"))
	#
	# Ensure that col stuff is of the correct size.
	#
    if (is.matrix(col.signs))
        col.signs <- as.vector(col.signs)
    if (length(col.signs) != nc)
        stop(paste("Error: We have", length(col.signs), "signs, but",
            nc, "columns"))
    if (is.matrix(col.rhs))
        col.rhs <- as.vector(col.rhs)
    if (length(col.rhs) != nc)
        stop(paste("Error: We have", length(col.rhs), "rhs's, but",
            nc, "rows"))
    if (direction == "min")
        direction <- as.integer(0)
    else
        if (direction == "max")
            direction <- as.integer(1)
        else
            stop ("Direction should be 'min' or 'max'")
    varcount <- as.integer(nr * nc)              # no of vars
    objective <- as.double(c(0, c(t(cost.mat))))
    if (is.null (integers)) {
        int.count <- 0
        integers <- 0
    }
    else
        int.count <- length(integers)
    const.count <- as.integer(nr + nc)       # no of constraints
    rnum.signs <- rep(-1, nr)               # sign holder
#
# Set the signs: <, >, = turn into 1,2,3 respectively. We also
# allow those followed by another "=". Anything else is an error.
#
    rnum.signs[row.signs == "<" | row.signs == "<="] <- 1
    rnum.signs[row.signs == "=" | row.signs == "=="] <- 3
    rnum.signs[row.signs == ">" | row.signs == ">="] <- 2
    if (any(rnum.signs == -1))
        stop(paste("Unknown row sign in position ", which(rnum.signs ==
            -1)[1]))
#
# Column signs.
#
    cnum.signs <- rep(-1, nc)
    cnum.signs[col.signs == "<" | col.signs == "<="] <- 1
    cnum.signs[col.signs == "=" | col.signs == "=="] <- 3
    cnum.signs[col.signs == ">" | col.signs == ">="] <- 2
    if (any(cnum.signs == -1))
        stop(paste("Unknown column sign in position ", which(cnum.signs ==
            -1)[1]))
#
# A few more things, plus dual action.
#
    objval <- as.double(0)
    solution <- as.double(numeric(nc * nr))
    status <- as.integer(0)
    sens.coef.from <- sens.coef.to <- 0
    duals <- duals.from <- duals.to <- 0
    if (compute.sens) {
        sens.coef.from <- sens.coef.to <- numeric(varcount)
        duals <- duals.from <- duals.to <- numeric(varcount +
            const.count)
    }
#
# Stick a zero on the front of costs, and off we go.
#
    costs <- as.double (c(0, c(cost.mat)))
    lps.out <- .C("lp_transbig",
	direction = direction,
	rcount = as.integer (nr),
        ccount = as.integer (nc),
	costs = costs,
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
# Set solution back into a matrix.
#
    lps.out$solution = matrix(lps.out$solution, nr, nc, byrow = FALSE)
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
    if(any(names(version) == "language"))
        class(lps.out) <- "lp"
    else oldClass(lps.out) <- "lp"
    lps.out
}
