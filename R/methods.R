# methods.R - DESC
# FLasher/R/methods.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# show {{{
setMethod("show", signature("fwdControl"),
  function(object) {
    
    cat("An object of class \"fwdControl\"\n", sep="")

    # SHOW always year, min, value, max, quantity
    nms <- names(object@target)

    # FIND relevant cols (!unique)
    idx <- apply(object@target[,-c(1:5)], 2, function(x) length(unique(x))==1)
    nms <- c(nms[1:5], nms[-c(1:5)][!idx])
    
    # SELECT cols
    df <- object@target[, nms]

    # SWAP median and mad from iters
    df[,c('min', 'value', 'max')] <- 
      apply(object@iters, 1:2, function(x)
        if(all(is.na(x)))
          sprintf("%s", "NA")
        else if(length(unique(x)) == 1)
          sprintf("%4.3f", x[1])
        else
          paste0(
            sprintf("%4.3f", median(x, na.rm=TRUE)), '(',
            sprintf("%4.3f", mad(x, na.rm=TRUE)), ')')
      )
    print(cbind(`(step)`=targetNo(object), df), row.names=FALSE)
      
    if(dim(object@iters)[3] > 1) {
      cat("   iters: ", dim(object@iters)[3],"\n\n")
    }
  }
) # }}}

# [ {{{
setMethod("[", signature(x="fwdControl"),
  function(x, i, j) {

    # 'i' applies to rows in both target ands iters
    if(!missing(i)) {
      x@target <- x@target[i,,drop=FALSE]
      x@iters <- x@iters[i,,,drop=FALSE]
    }
    # 'j' applies only to 3rd dimension in iters
    if(!missing(j)) {
      x@iters <- x@iters[,,j,drop=FALSE]
    }

    return(x)
  }
) # }}}

# [<- {{{

# vector
setMethod("[<-", signature(x="fwdControl", value="vector"),
  function(x, i, j, k, ..., value) {
    
    arge <- lapply(as.list(dim(x@target)), seq)
    argi <- lapply(as.list(dim(x@iters)), seq)

    # i 
    if(!missing(i)) {
      arge[[1]] <- i
      argi[[1]] <- i
    }
    # j
    if(!missing(j)) {
      # ONLY one column
      if(length(j) > 1)
        stop("vector can only be assigned to a single column, none selected")
      arge[[2]] <- j
      if(any(j %in% c('min', 'value', 'max') | j %in% c(3:5))) {
        argi[[2]] <- j[j %in% c('min', 'value', 'max') | j %in% c(3:5)]
      }
    }
    # k
    if(!missing(k)) {
      argi[[3]] <- k
    }

    # if min, value or max changed
    if(any(j %in% c('min', 'value', 'max') | j %in% c(3:5))) {
      x@iters <- do.call('[<-', c(list(x@iters), argi, list(value=value)))
      # UPDATE target
      x@target <- do.call('[<-', c(list(x@target), arge,
        list(value=mean(do.call('[', c(list(x@iters), argi)), na.rm=TRUE))))
    # other columns
    } else {
      x@target <- do.call('[<-', c(list(x@target), arge, list(value=value)))
    }

    return(x)
  }
)

# }}}

# propagate {{{
setMethod("propagate", signature(object="fwdControl"),
  function(object, iter, fill.iter=TRUE) {

    nit <- dim(object@iters)[3]

    if(iter == nit)
      return(object)
    
    # Only extend if iter == 1
    if(nit > 1)
      stop("Can only propagate an object with a single 'iter'")

    its <- object@iters[, , rep(1, iter), drop=FALSE]
    dimnames(its)$iter <- seq(1, iter)

    if(!fill.iter)
      its[,,seq(2, iter)] <- NA

    object@iters <- its

    return(object)
  }
)# }}}
