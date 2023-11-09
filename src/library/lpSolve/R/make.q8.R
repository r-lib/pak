make.q8 <- function ()  
{
#
# Q8: Set up sparse constraints (in the "lp" package sense) for the 8 queens problem.
#
# "Chess" holds the chess board.
#
chess <- matrix (1:64, 8, 8, byrow=T)
#
# Row and column constraints. Recall that each constraint has three entries.
#
row.const <- cbind (rep (1:8, each=8), c(t(chess)), 1)
col.const <- cbind (rep (9:16, each=8), c(chess), 1)
#
# Diagonals, bottom left to top right.
#
const.ctr <- 17
chess <- matrix (1:64, 8, 8, byrow=T)
row.chess <- row(chess); col.chess <- col(chess)
d1.const <- NULL
rplusc <- row.chess + col.chess
for (i in 3:15) {
d1.const <- rbind (d1.const, cbind (const.ctr, chess[rplusc == i], 1))
const.ctr <- const.ctr + 1
}
#
# Now the other way.
#
start <- seq (49,1, by=-8)
for (i in start) {
d1.const <- rbind (d1.const, cbind (const.ctr, seq (i, 64, by=9), 1))
const.ctr <- const.ctr + 1
}
#
# Also a few more!
#
for (i in 2:7)
{
d1.const <- rbind (d1.const, cbind (const.ctr, seq (i, chess[9-i,8], by=9), 1))
const.ctr <- const.ctr + 1
}
#
# Return all constraints.
#
rbind (row.const, col.const, d1.const)
}

