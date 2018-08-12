# Project: applyBy
#
# Matrix stats utility functions
# 
# Author: Renaud Gaujoux
# Creation: 23 Jan 2012
###############################################################################

# static variable
.applyBy_BY <- local({
    .value <- NULL
    function(val){
        if( missing(val) ) .value
        else{
            old <- .value
            .value <<- val
            old
        }
    }
})

#' Group Apply
#' 
#' \code{appplyBy} is an S3 generic function that applies a given function 
#' to sub-matrices of a matrix-like object, which are generated according to 
#' a factor that defines groups rows or columns.
#' 
#' @export
applyBy <- function(x, ...){
    UseMethod('applyBy')
}

#' The method \code{applyBy.matrix} is the work horse function 
#' that is called by other more user-friendly functions.
#' 
#' In essence, \code{applyBy.matrix} is a wrapper around \code{\link[matrixStats]{colAvgsPerRowSet}} 
#' and \code{\link[matrixStats]{rowAvgsPerColSet}} from the \code{\link{matrixStats}} package, 
#' which makes the computation really fast, but requires somehow cumbersome matrix arguments
#' to specify the groups of columns or rows.
#' The wrapper function builds the required arguments for cases where the groups 
#' are defined by a factor or a list of indexes.
#' 
#' @param x matrix-like object on which \code{\link{apply}} can be called.
#' @param BY factor or object coerced to a factor, that defines the groups within 
#' which the function \code{FUN} is applied.
#' 
#' If \code{x} is an ExpressionSet object, then \code{BY} can be the names of a
#' sample (resp. feature) annotation variable if \code{MARGIN=1} (resp. \code{MARGIN=2L}) 
#' (see examples).
#' @param MARGIN margin along which the function \code{FUN} is applied: 
#' 1L for rows, 2L for columns. 
#' @param FUN function to apply to each sub-matrix that contains the rows/columns 
#' defined by each level of argument \code{BY}.
#' It must be a function that takes a matrix as its first argument and returns a vector 
#' of length the dimension of margin \code{MARGIN} of \code{x}.
#' @inheritParams matrixStats::colAvgsPerRowSet
#' @param ... extra parameters passed to \code{FUN}.
#' @param DROP logical that indicates if absent levels should be removed 
#' from the result matrix, or appear as 0-filled rows/columns.
#' 
#' If \code{BY} is a list of character indexes, then \code{DROP=FALSE} will 
#' complete the list with any missing index, so that the corresponding rows/columns 
#' form singletons.
#' This means that these data will be passed as row/column vectors to the 
#' aggregation function \code{FUN}.
#' 
#' @param DUPS logical, used when \code{BY} is a list, that indicates 
#' if overlapping sets are allowed.
#' Default is to throw a warning if such case is detected.
#' Using \code{DUPS = TRUE} silence the warning, while \code{DUPS = FALSE}
#' turns into an error.  
#' 
#' @return The result is a matrix or an \code{ExpressionSet} object 
#' whose margin's dimension \code{MARGIN} is equal the same margin's 
#' dimension in \code{x}, and the other to the number of levels 
#' in \code{BY}.
#'
#' @seealso built-ins
#' @export 
#' @importFrom pkgmaker isNumber isString str_out
#' @importFrom matrixStats colAvgsPerRowSet rowAvgsPerColSet
#' @importFrom stats setNames
#' @rdname applyBy 
#' @examples
#' 
#' # random data matrix
#' x <- matrix(runif(12 * 6), 12, 6)
#' 
#' # by groups of columns
#' fc <- gl(2, 3)
#' b <- applyBy(x, fc, 1L, rowSums)
#' b
#' # or
#' balt <- rowApplyBy(x, fc, rowSums)
#' stopifnot(identical(b, balt))
#' 
#' # by groups of rows
#' fr <- gl(3, 4)
#' b <- applyBy(x, fr, 2L, colSums)
#' # or
#' balt <- colApplyBy(x, fr, colSums)
#' stopifnot(identical(b, balt))
#'  
applyBy.matrix <- function(x, BY, MARGIN, FUN, W=NULL, ..., DROP=FALSE, DUPS = NULL){
    
    if( !isNumber(MARGIN) ){
        stop("Invalid value for argument 'MARGIN' of class '", class(MARGIN), "': must be a single number (e.g. 1 or 2)")
    }
    BYMARGIN <- 3L-MARGIN
    # check arguments
    mdim <- length(dim(x))
    if( MARGIN > mdim )
        stop("Invalid value for argument `MARGIN`: greater than the number of dimensions of `x` [", mdim, "]")	
    if( BYMARGIN > mdim )
        stop("Invalid value for argument `BYMARGIN`: greater than the number of dimensions of `x` [", mdim, ']')
    if( MARGIN > 2 || BYMARGIN > 2 )
        stop("Invalid margins: must be either 1L or 2L [", MARGIN, ', ', BYMARGIN, ']')
    
    bynames <- dimnames(x)[[BYMARGIN]]
    # convert list of indexes into a vector/factor
    if( is.list(BY) ){ 
        idx <- unlist(BY)
        if( !is.integer(idx) && !is.character(idx) )
            stop("Invalid `BY` argument of type list [", str_out(idx, use.names = TRUE), "]:"
                    , " should contain integer or character indexes")
        if( is.character(idx) && any(!idx %in% bynames) )
            stop("Invalid `BY` argument of type list [", str_out(idx, use.names = TRUE), "]:"
                    , " some of the indexes do not match any data BY margin names"
                    , " [", str_out(bynames), "].")
        
        if( hasDUPS <- anyDuplicated(idx) ){
            if( is.null(DUPS) ) warning("List `BY` contains duplicated indexes: overlapping group values main be correlated.")
            else if( !isTRUE(DUPS) ) stop("Invalid `BY` argument of type list: it should not contain any duplicated values.")
        }
        
        # complete list with missing levels if necessary
        if( !DROP && is.character(idx) && any(missing_levels <- !bynames %in% idx) ){
            missing_levels <- setNames(bynames[missing_levels], bynames[missing_levels])
            BY <- c(BY, as.list(missing_levels))
            idx <- c(idx, missing_levels)
        }
        
        # use names as levels
        if( is.null(names(BY)) ) names(BY) <- seq_along(BY)		
        
        if( !hasDUPS ){# convert into a factor
            # subset to the given indexes
            x <- if( BYMARGIN == 1L ) x[idx, , drop=FALSE] else x[, idx, drop=FALSE]        
            v <- rep(names(BY), sapply(BY, length))
            BY <- factor(v, levels=names(BY))
        }else if( is.character(idx) ){ # convert to integer indexes
			BY <- sapply(BY, match, table = bynames, nomatch = 0L, simplify = FALSE)
		}
    }
    
    # convert non-list specification to a factor
    if( !is.list(BY) ){
        bydim <- dim(x)[BYMARGIN]
        if( length(BY) != bydim )
            stop("Invalid value for argument `BY`: length [", length(BY), "] is not equal to the dimension [", bydim, "] of the BY margin [", BYMARGIN, ']')
        
        # coerce to factor if necessary
        if( !is.factor(BY) ) BY <- factor(BY, levels=unique(BY))
        s <- split(1:bydim, BY)
        
	}else s <- BY
    # build subset matrix
    nm <- max(sapply(s, length))
    S <- matrix(0, nm, length(s))
    colnames(S) <- names(s)
    sapply(seq_along(s), function(i){
                idx <- s[[i]]
                if( length(idx) > 0L ) S[1:length(idx),i] <<- idx
            })
    # store idx in static variable if requested
    if( isTRUE(.applyBy_BY()) ){
        .applyBy_BY(BY)
    }
    
    # call relevant function from matrixStats
	.FUN <- FUN
    if( MARGIN == 1L ){
        # prevent bug in matrixStats for row matrix: add dummy column
        xmat <- if( nrow(x) == 1L ){
            .FUN <- function(x, ...) c(FUN(x[-nrow(x), , drop = FALSE], ...), NA)
            rbind(x, 0)
            }else if( !nrow(x) ){ # early exit if empty input matrix
                  x <- x[, 1:ncol(S), drop = FALSE]
                  colnames(x) <- colnames(S)
                  return(x)
              } 
              else x
        # call
        res <- rowAvgsPerColSet(X=xmat, S=S, FUN=.FUN, W=W, ...)
        # remove dummy row
        if( nrow(x) == 1L ) res <- res[-nrow(res), , drop = FALSE]
    }else{
        # prevent bug in matrixStats for column matrix: add dummy column
        xmat <- if( ncol(x) == 1L ){
            .FUN <- function(x, ...) c(FUN(x[, -ncol(x), drop = FALSE], ...), NA)
            cbind(x, 0)
            }else if( !ncol(x) ){ # early exit if empty input matrix
                    x <- x[1:ncol(S), , drop = FALSE]
                    rownames(x) <- colnames(S)
                    return(x)
                } else x 
        # call
        res <- colAvgsPerRowSet(X=xmat, S=S, FUN=.FUN, W=W, ...)
        # remove dummy column
        if( ncol(x) == 1L ) res <- res[, -ncol(res), drop = FALSE] 
    }
    
    # drop absent levels if requested
    if( DROP ){
        lv <- levels(BY)
        if( length(w <- which(!lv %in% unique(as.character(BY)))) ){
            # update stored factor if necessary
            if( !is.null(.applyBy_BY()) ){
                .applyBy_BY(droplevels(BY))
            }
            if( MARGIN == 1L ){
                res <- res[,-w,drop=FALSE]
#			tmp <- cbind(res, NA_Matrix(nrow(res), length(w)))
#			colnames(tmp)[(ncol(res)+1):ncol(tmp)] <- lv[w] 
            }else{
                res <- res[-w,,drop=FALSE]
#			tmp <- rbind(res, NA_Matrix(length(w), ncol(res)))
#			rownames(tmp)[(nrow(res)+1):nrow(tmp)] <- lv[w]
            }
        }
    }
    
    #
    
    # return result
    res
}

#' A method is provided for \code{\link[Biobase]{ExpressionSet}} objects, 
#' which preserve sample and feature annotations.
#' Moreover it allows directly passing names of feature/sample annotation -- factor -- variables 
#' in argument \code{BY} (see examples).
#' 
#' @param ANNOTATIONS logical that indicates if samples/feature annotations should 
#' be kept, when the input data is an \code{\link[Biobase]{ExpressionSet}} object.
#' Currently, if \code{TRUE}:
#' \itemize{
#' \item if code{MARGIN=1L}, then feature annotations are kept unchanged, and 
#' phenotypic sample annotations are discarded. 
#' \item if code{MARGIN=2L}, then phenotypic sample annotations are kept unchanged, and 
#' feature annotations are discarded.
#' } 
#' 
#' In any case, the value of slot \code{annotation} (i.e. the annotation package), 
#' is passed on to the result object.
#'  
#' @export 
#' @rdname applyBy
#' @examples
#' 
#' ## Method for apply directly to ExpressionSet objects
#' 
#' library(Biobase)
#' x <- ExpressionSet(x, annotation='abcd.db')
#' y <- rowMinsBy(x, fc)
#' y <- colMinsBy(x, fr)
#' 
#' ## annotations are conserved/collapsed
#' pData(x) <- data.frame(Group=fc, Sample=letters[1:ncol(x)])
#' pData(x)
#' fData(x) <- data.frame(ENTREZID=fr, Gene=letters[nrow(x):1])
#' fData(x)
#' 
#' # keep feature annotations, collapse sample annotations
#' y <- rowMinsBy(x, 'Group')
#' pData(y)
#' fData(y)
#' 
#' # keep sample annotations, collapse feature annotations 
#' y <- colMinsBy(x, 'ENTREZID')
#' pData(y)
#' fData(y)
applyBy.ExpressionSet <- function(x, BY, MARGIN, ..., ANNOTATIONS=TRUE){
    
    # convert single character string into annotation variable
    if( !requireNamespace('Biobase') ){
      stop("Could not perform group-apply operation on ExpressionSet object: required package Biobase could not be loaded.")
    }
    # load Biobase functions
    phenoData <- Biobase::phenoData
    pData <- Biobase::pData
    featureData <- Biobase::featureData
    fData <- Biobase::fData
    varLabels <- Biobase::varLabels

    if( isString(BY) ){
        if( MARGIN == 1L ){ # phenotypic variable
            if( !BY %in% varLabels(x) ){
                stop("Invalid string argument BY: there is no phenotypic/sample annotation variable called '", BY, "'.\n"
                        , "  Defined variable are: ", str_out(varLabels(x), Inf))
            }
            BY <- pData(x)[[BY]]
        }else{ # feature annotation variable
            if( !BY %in% names(fData(x)) ){
                stop("Invalid string argument BY: there is no feature annotation variable called '", BY, "'.\n"
                        , "  Defined variable are: ", str_out(names(fData(x)), Inf))
            }
            BY <- fData(x)[[BY]]
        }
    }
    
    # apply to expression matrix
    .applyBy_BY(TRUE)
    on.exit( .applyBy_BY(NULL) )
    res <- applyBy(Biobase::exprs(x), BY=BY, MARGIN=MARGIN, ...)
    
    # re-wrap into an ExpressionSet objects
    # pass on annotations whenever possible
    fd <- pd <- NULL
    if( ANNOTATIONS ){
        if( MARGIN == 1L ){
            if( nrow(ad <- featureData(x)) > 0L ) fd <- ad # keep feature annotations
            # collapse sample annotation for non-list BY arguments
            if( !is.list(BY) && nrow(ad <- phenoData(x)) > 0L ){
                # get used BY factor
                fBY <- .applyBy_BY()
                new_margin_names <- colnames(res) 
                # use annotation of first representative of each level
                irep <- match(new_margin_names, as.character(fBY)) # order must match the item names
                # force missing levels to get all NA annotations
                irep[is.na(irep)] <- nrow(pData(x)) + 1L
                df <- pData(x)[irep, , drop = FALSE]
                # warp into an AnnotatedDataFrame object
                rownames(df) <- new_margin_names
                pd <- Biobase::AnnotatedDataFrame(df)
            }
        }else if( MARGIN == 2L ){
            if( nrow(ad <- phenoData(x)) > 0L ) pd <- ad # keep sample annotations
            # collapse feature annotation for non-list BY arguments
            if( !is.list(BY) && nrow(ad <- featureData(x)) > 0L ){
                # get used BY factor
                fBY <- .applyBy_BY()
                new_margin_names <- rownames(res)
                # use annotation of first representative of each level
                irep <- match(new_margin_names, as.character(fBY)) # order must match the item names
                # force missing levels to get all NA annotations
                irep[is.na(irep)] <- nrow(fData(x)) + 1L
                df <- fData(x)[irep, , drop = FALSE]
                # warp into an AnnotatedDataFrame object
                rownames(df) <- new_margin_names
                fd <- Biobase::AnnotatedDataFrame(df)
            }
        }
    }
    
    # do wrap
    ca <- call('ExpressionSet', res, annotation = Biobase::annotation(x), experimentData = Biobase::experimentData(x))
    if( !is.null(pd) ) ca$phenoData <- pd
    if( !is.null(fd) ) ca$featureData <- fd
    res <- eval(ca)
    
    res
}

#' @export
#' @rdname applyBy
#' @examples
#' 
#' x <- runif(12)
#' # 3 groups of 4 elements
#' g <- gl(3, 4)
#' g
#' 
#' # row means ~> row matrix with 3 columns
#' a <- rowMeansBy(x, g)
#' a
#' # col means ~> column matrix with 3 rows
#' b <- colMeansBy(x, g)
#' b
#' 
#' # values are identical
#' stopifnot( identical(as.numeric(a), as.numeric(b)) )
#' 
applyBy.numeric <- function(x, BY, MARGIN, ...){
    
    if( MARGIN == 1L ){ 
        applyBy(matrix(x, nrow = 1L), BY = BY, MARGIN = MARGIN, ...)
    }else{
        applyBy(matrix(x, ncol = 1L), BY = BY, MARGIN = MARGIN, ...)
    }
}


#' \code{rowApplyBy} applies a function to rows of sub-matrices whose columns 
#' are defined by a factor.
#' @export
#' @rdname applyBy
rowApplyBy <- function(x, BY, FUN, ...){ 
    applyBy(x, BY=BY, MARGIN=1L, FUN=FUN, ...)
}

#' \code{rowApplyBy} applies a function to columns of sub-matrices whose rows 
#' are defined by a factor.
#' @export
#' @rdname applyBy
colApplyBy <- function(x, BY, FUN, ...){ 
    applyBy(x, BY=BY, MARGIN=2L, FUN=FUN, ...)
}
