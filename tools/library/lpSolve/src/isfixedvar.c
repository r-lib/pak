#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

#include "lp_types.h"
#include "lp_utils.h"
#include "lp_lib.h"

MYBOOL is_fixedvar(lprec *lp, int variable)
{
  if((lp->bb_bounds != NULL && lp->bb_bounds->UBzerobased) || (variable <= lp->rows))
    return( (MYBOOL) (lp->upbo[variable] < lp->epsprimal) );
  else
    return( (MYBOOL) (lp->upbo[variable]-lp->lowbo[variable] < lp->epsprimal) );
} /* is_fixedvar */


