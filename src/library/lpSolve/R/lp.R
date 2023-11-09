lp <- function(direction = "min", objective.in, const.mat, const.dir, const.rhs,
	transpose.constraints = TRUE, int.vec, presolve = 0, compute.sens = 0,
	binary.vec, all.int=FALSE, all.bin=FALSE, scale=196, dense.const, 
        num.bin.solns=1, use.rw=FALSE, timeout = 0L)
{
	#
	# lp: solve a general linear program
	#
	# Arguments:
	#     direction: Character: direction of optimization: "min" (default) 
	#	 or "max."
	#     objective.in: Numeric vector (or one-column data frame) of 
	#	 coefficients of objective function
	#     const.mat: Matrix of numeric constraint coefficients, one row
	#        per constraint, one column per variable (unless
	#        transpose.constraints =  FALSE; see below).
	#     const.dir: Vector of character strings giving the direction of the
	#        constraints: each value should be one of "<," "<=," "=," "==,"
	#        ">," or ">=."
	#     const.rhs: Vector of numeric values for the right-hand sides 
	#        of the constraints.
	#     transpose.constraints: By default each constraint occupies a 
	#        row of const.mat, and that matrix needs to be transposed before
	#        being passed  to the optimizing code.  For very large
	#        constraint matrices it may be wiser  to construct the
	#        constraints in a matrix column-by-column. In that case set
	#        transpose.constraints to FALSE.
	#     int.vec: Numeric vector giving the indices of variables that are
	#        required to be integer. The length of this vector will
	#        therefore be the  number of integer variables.
	#     presolve: Numeric: Should presolve be done (in lp_solve)? 
	#	 Default: 0 (no).
	#        A non-zero value means "yes." Currently mostly ignored.
	#     compute.sens: Numeric: compute sensitivities? Default 0 (no). 
	#	Any non-zero value means "yes."
	#     binary.vec: Numeric vector giving indices of binary variables
	#     all.int: logical: True if all variables should be integers. This
	#	overrides anything in int.vec.
	#     all.bin: logical: True if all variables should be binary. This
	#	overrides anything in binary.vec.
	#     dense.const: alternative specification of constraints in the 
	#        form of a three-column matrix or data frame. If a row contains
	#        (i, j, k), it means "constraint i, variable j = value k." This
	#        is ignored if const.mat is supplied.
        #     scale: integer giving scaling. Possible values can be found in
        #      in the lpSolve documentatation. 0 = no scaling. Default: 196.
	#      num.bin.solns: If all.bin is True, we will attempt to find up to
	#        num.bin.solns solutions and return them in a matrix.
	#      use.rw: Work around a bug when num.bin.solns=TRUE by writing each
	#        solution out to a file and reading it back in.
        #      timeout: set as timeout variable in lpslink.
        #
	# Set up the direction.
	#
	if(direction == "min")
		direction <- 0
	else if (direction == "max")
                direction <- 1
             else stop ("Direction must be 'max' or 'min'")
	#
	# Convert one-column data frame objective to vector. Add leading 0 to 
	#	obejctive.
	#
	if(is.data.frame(objective.in)) {
		if(ncol(objective.in) > 1)
			stop("Objective vector has more than one column")
		objective.in <- unlist(objective.in)
		names(objective.in) <- NULL
	}
	#
	# Set up solution, status, x.count (= number of variables)
	#
	objective <- c(0, objective.in)
	solution <- numeric(length(objective.in))
	status <- objval <- 0
	x.count <- length(objective.in)
        constraints <- 0
        const.count <- 0
	#
	# Constraint matrix stuff. If const.mat is passed in, convert it
	# to a matrix if necessary (and set NA's to 0). Also transpose
	# if necessary.
	#
      use.dense <- FALSE
	if (!missing (const.mat)) {
            dense.const <- 0; dense.const.nrow = 0; dense.ctr = 0
	    if(is.data.frame(const.mat)) {
		    cm <- as.numeric(unlist(const.mat))
		    names(cm) <- NULL
		    const.mat <- matrix(cm, nrow = nrow(const.mat))
	    }
	    const.mat[is.na(const.mat)] <- 0
	    if(transpose.constraints)
		    const.mat <- t(const.mat)
	    const.count <- ncol(const.mat)
	}
	else
        {
	#
	# If there was no const.mat, our constraints are in dense.const (or
	# there are no constraints). If there's a dense.const, it needs to have
	# three columns, each row having <const #>, <var #> and <value>.
	# The number of constraints is the largest value in the const column.
	# Check to make sure every value in the range is present, and that
	# there aren't references to variables that don't exist. It will
	# be convenient to sort this by constraint number.
	#
            if (missing (dense.const)) {
            dense.const <- 0; dense.const.nrow = 0; dense.ctr = 0
		} else {
            if (!is.matrix (dense.const))
		stop ("Dense constraints need to be matrix or data frame.")
	    if (is.data.frame (dense.const)) dense.const <- as.matrix (dense.const)
            if (ncol (dense.const) != 3)
                stop ("Dense constraints must be in three columns.")
            dimnames(dense.const) <- list (NULL, c("Const", "Var", "Value"))
            dense.const <- dense.const[order(dense.const[,"Const"]),,drop=FALSE]
            const.indices <- unique (dense.const[,"Const"])
            if (length(const.indices) != max (const.indices))
		stop ("Error in constraint numbering")
            const.count <- max (const.indices)
        #
        # It's convenient to send in one other piece of information. The
	# "dense.ctr" vector's ith entry says how many non-zero elements
	# are found in constraint i. Once we have that we don't need the
	# "Const" column of dense.const, either.
	#
            dense.ctr <- table (dense.const[,"Const"])
            names(dense.ctr) <- NULL
            dense.const <- dense.const[,c("Var", "Value"), drop=FALSE]
            dense.const.nrow <- nrow (dense.const)
            use.dense <- TRUE
	}}
	#
	# Set up constraint signs...
	#
	const.dir.num <- rep(-1, length(const.dir))
	const.dir.num[const.dir == "<" | const.dir == "<="] <- 1
	const.dir.num[const.dir == "=" | const.dir == "=="] <- 3
	const.dir.num[const.dir == ">" | const.dir == ">="] <- 2
	if(any(const.dir.num == -1))
		stop("Unknown constraint direction found\n")
	#
	# ...constraint count, and right-hand sides.
	#
	if(is.data.frame(const.rhs))
		const.rhs <- as.matrix(const.rhs)
	const.rhs <- c(const.rhs)
	names(const.rhs) <- NULL
	#
	# For regular (non-dense) constraints, set up big matrix of 
        # constraint info; add a 0 on the front.
	#
	if (!missing (const.mat)) {
	    big.const.mat <- rbind(const.mat, const.dir.num, const.rhs)
	    constraints <- c(0, c(big.const.mat))
        }
        #
        # For dense constraints, just add rows of directions and rhs's
	# to dense.ctr, which will then be a 3-row matrix w/ const.count cols.
        #
        else if (!missing (dense.const)) {
            dense.ctr <- rbind (dense.ctr, const.dir.num, const.rhs)
		big.const.mat <- 0
		constraints <- 0
		}
	#
	# Set up int.vec. If all.int is present (and TRUE), use that.
	#
	if (!missing (all.int) && all.int) {
		int.vec <- 1:length(solution)
		int.count <- length(int.vec)
	}
	else
	{
		if(missing(int.vec)) {
			int.count <- 0
			int.vec <- 0
		}
		else
			int.count <- length(int.vec)
	}
	#
	# Do the same for binary.vec.
	#
	if (!missing (all.bin) && all.bin) {
		binary.vec <- 1:length(solution)
		bin.count <- length(binary.vec)
	}
	else
	{
		if(missing(binary.vec)) {
			bin.count <- 0
			binary.vec <- 0
		}
		else
			bin.count <- length(binary.vec)
	}
	# If all variables are binary, set all.bin to TRUE.
      if (length(binary.vec) == length(solution)) all.bin <- TRUE
	#
	# If more than one solution is called for, prepare for that. "Solution" will need
      # one extra element.
	#
		if (num.bin.solns > 1)
                if (all.bin)	
                    solution <- c(0, rep (solution, num.bin.solns))
		    else {
                    warning ("Num.bin.solns can only be > 1 if all variables are binary")
                    num.bin.solns <- 1
             }
	#
	# Set up sensitivity stuff.
	#
	sens.coef.from <- sens.coef.to <- 0
	duals <- duals.from <- duals.to <- 0
	if(compute.sens != 0) {
		sens.coef.from <- sens.coef.to <- numeric(x.count)
	    duals <- duals.from <- duals.to <- numeric(x.count + const.count)
	}
	#

	if (num.bin.solns > 1 && use.rw == TRUE)
          tmp <- tempfile ()
      else
          tmp <- "Nobody will ever look at this"
        if (missing (dense.const) || !is.matrix (dense.const))
        {
            dense.col <- dense.val <- 0
        }
        else
        {
	    dense.col = dense.const[,"Var"]
	    dense.val = dense.const[,"Value"]
        }
	lp.out <- .C("lpslink",
		direction = as.integer(direction),
		x.count = as.integer(x.count),
		objective = as.double(objective),
		const.count = as.integer(const.count),
		constraints = as.double(constraints),
		int.count = as.integer(int.count),
		int.vec = as.integer(int.vec),
		bin.count = as.integer(bin.count),
		binary.vec = as.integer(binary.vec),
                num.bin.solns = as.integer (num.bin.solns),
		objval = as.double(objval),
		solution = as.double(solution),
		presolve = as.integer(presolve),
		compute.sens = as.integer(compute.sens),
		sens.coef.from = as.double(sens.coef.from),
		sens.coef.to = as.double(sens.coef.to),
		duals = as.double(duals),
		duals.from = as.double(duals.from),
		duals.to = as.double(duals.to),
                scale = as.integer (scale),
                use.dense = as.integer (use.dense),
		dense.col = as.integer (dense.col),
		dense.val = as.double (dense.val),
                dense.const.nrow = as.integer (dense.const.nrow),
                dense.ctr = as.double (dense.ctr),
                use.rw = as.integer (use.rw),
                tmp = as.character(tmp),
		status = as.integer(status),
                timeout = as.integer(timeout),
                PACKAGE="lpSolve")
        lp.out$objective <- objective.in
        lp.out$constraints <- big.const.mat
	if(any(names(version) == "language"))
		class(lp.out) <- "lp"
	else oldClass(lp.out) <- "lp"
	return(lp.out)
}
