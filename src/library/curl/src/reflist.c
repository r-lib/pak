#include <Rinternals.h>

SEXP reflist_init(void){
  return R_NilValue;
}

//note: you MUST use the return value for this object
SEXP reflist_add(SEXP x, SEXP target){
  if(!Rf_isPairList(x))
    Rf_error("Not a LISTSXP");
  return(Rf_cons(target, x));
}

SEXP reflist_has(SEXP x, SEXP target){
  if(!Rf_isPairList(x))
    Rf_error("Not a LISTSXP");
  while(x != R_NilValue){
    if(CAR(x) == target)
      return(Rf_ScalarLogical(1));
    x = CDR(x);
  }
  return(Rf_ScalarLogical(0));
}

SEXP reflist_remove(SEXP x, SEXP target){
  if(!Rf_isPairList(x))
    Rf_error("Not a LISTSXP");

  //drop head
  if(x != R_NilValue && CAR(x) == target)
    return(CDR(x));
  SEXP prev = x;
  SEXP current = CDR(x);

  //check inner nodes
  while(current != R_NilValue){
    if(CAR(current) == target){
      SETCDR(prev, CDR(current));
      return(x);
    }
    prev = current;
    current = CDR(current);
  }
  Rf_error("Object not found in reflist!");
}

SEXP reflist_length(SEXP x) {
  if(!Rf_isPairList(x))
    Rf_error("Not a LISTSXP");
  int i = 0;
  while(x != R_NilValue){
    i++;
    x = CDR(x);
  }
  return Rf_ScalarInteger(i);
}
