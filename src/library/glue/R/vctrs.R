
# Registered in .onLoad()

vec_ptype2.glue.glue <- function(x, y, ...) {
  x
}
vec_ptype2.glue.character <- function(x, y, ...) {
  x
}
vec_ptype2.character.glue <- function(x, y, ...) {
  y
}

# Note order of class is the opposite as for ptype2
vec_cast.glue.glue <- function(x, to, ...) {
  x
}
vec_cast.glue.character <- function(x, to, ...) {
  as_glue(x)
}
vec_cast.character.glue <- function(x, to, ...) {
  unclass(x)
}
