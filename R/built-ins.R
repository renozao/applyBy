# Convenient pre-defined applyBy functions
# 
# Author: Renaud Gaujoux
###############################################################################


#' Built-in Group Apply Utility Functions
#' 
#' These functions provide convenience shortcut for commonly
#' performed group-apply operations, such as column/row group means, medians, 
#' maxs, mins, etc...
#'
#' @inheritParams applyBy
#' @rdname built-ins
#' @name builtins
NULL

# generator functions
.rowApplyByFunction <- function(FUN){
  function(x, BY, ...){
    applyBy(x, BY=BY, MARGIN=1L, FUN=FUN, ...)
  }
}
.colApplyByFunction <- function(FUN){
  function(x, BY, ...){
    applyBy(x, BY=BY, MARGIN=2L, FUN=FUN, ...)
  }
}

#' \code{col<STAT>By} computes for each column a given statistic within separate groups of rows, which are defined by a factor.
#' @export
#' @rdname built-ins
colSumsBy <- .colApplyByFunction(colSums)

#' \code{row<STAT>By} computes for each row a given statistic within separate groups of columns, which are defined by a factor.
#' @export
#' @rdname built-ins
rowSumsBy <- .rowApplyByFunction(rowSums)

#' @export
#' @rdname built-ins
rowMeansBy <- .rowApplyByFunction(rowMeans)
#' @export
#' @rdname built-ins
colMeansBy <- .colApplyByFunction(colMeans)

#' @export
#' @rdname built-ins
rowMediansBy <- .rowApplyByFunction(matrixStats::rowMedians)
#' @export
#' @rdname built-ins
colMediansBy <- .colApplyByFunction(matrixStats::colMedians)

#' @export
#' @rdname built-ins
rowMaxsBy <- .rowApplyByFunction(matrixStats::rowMaxs)
#' @export
#' @rdname built-ins
colMaxsBy <- .colApplyByFunction(matrixStats::colMaxs)

#' @export
#' @rdname built-ins
rowMinsBy <- .rowApplyByFunction(matrixStats::rowMins)
#' @export
#' @rdname built-ins
colMinsBy <- .colApplyByFunction(matrixStats::colMins)
