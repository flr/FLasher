# constructors.R - Constructor methods for fwdControl
# FLasher/R/constructors.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# fwdControl(target='data.frame', iters='array') {{{
#' @rdname fwdControl
#' @examples
#'
#' # Construct from data.frame and array
#' fcn <- fwdControl(data.frame(year=2000:2005, quantity='f', value=0.5))

setMethod('fwdControl', signature(target='data.frame', iters='array'),
  function(target, iters) {

    # TODO TEST dimensions
    dtg <- dim(target)
    dit <- dim(iters)
    dni <- dimnames(iters)

    # COMPLETE df
    trg <- new('fwdControl')@target[rep(1, nrow(target)),]
    
    # HACK: drop rownames
    rownames(trg) <- NULL
    
    # CONVERT year to integer
    if('year' %in% names(target))
      target$year <- as.integer(target$year)
    # ASSIGN to trg, DROP 'min', 'value', 'max'
    trg[, names(target)[names(target) %in% names(trg)]] <- target
    
    # HACK: reassign quantity to keep factors
    trg[,'quantity']  <- factor(target$quantity, levels=FLasher:::qlevels)

    # MASTER iters
    ite <- array(NA, dim=c(dtg[1], 3, dit[length(dit)]),
      dimnames=list(row=seq(dtg[1]), val=c('min', 'value', 'max'), iter=seq(dit[length(dit)])))

    # DIMNAMES in array?
    if(!is.null(dni)) {
      ite[, dni[['val']], ] <- iters

    # or NOT
    } else {
      # 2D or dim[2] == 1, assign to 'value'
      if(length(dit) == 2 | dit[2] == 1) {
        ite[,'value',] <- iters
      # 3D
      } else {
        ite[,,] <- iters
      }
    }
    return(new('fwdControl', target=trg, iters=ite))
  }
) 
# }}}

# fwdControl(target='data.frame', iters='matrix') {{{
setMethod('fwdControl', signature(target='data.frame', iters='matrix'),
  function(target, iters) {

    dni <- dimnames(iters)

    # NO dimnames in iters, assume dims are 'row' & 'iter' for 'value'
    if(is.null(dni)) {
      dimnames(iters) <- list(row=1:dim(iters)[1], iter=1:dim(iters)[2])
      dni <- dimnames(iters)
    }

    dms <- list(row=1, val=c('min', 'value', 'max'), iter=1)
    dms[names(dni)] <- dni

    ite <- array(NA, dimnames=dms, dim=unlist(lapply(dms, length)))

    # MISSING 'val' dimension, assume val='value'
    if(!"val" %in% names(dni)) {
      ite[,'value',] <- iters
    }
    # MISSING row
    else if (!"row" %in% names(dni)) {
      ite[1,,] <- iters
    }
    # MISSING iter
    if(!"iter" %in% names(dni))
      stop("No 'iter' dimname in iters, cannot create object")

    return(fwdControl(target=target, iters=ite))
  }
) # }}}

# fwdControl(target='data.frame', iters='numeric') {{{
setMethod('fwdControl', signature(target='data.frame', iters='numeric'),
  function(target, iters) {
  
    # CREATE iters
    dti <- dim(target)
    ite <- array(NA, dim=c(dti[1], 3, iters), dimnames=list(row=1:dti[1], 
      val=c('min', 'value', 'max'), iter=seq(iters)))

    # FIND val names in target
    vns <- c('min', 'value', 'max')
    nms <- vns %in% colnames(target)

        # ASSIGN values if 'value', 'min' or 'max' in df colnames
        if(any(nms))
        ite[, vns[nms],] <- target[,vns[nms]]

    return(fwdControl(target=target, iters=ite))
  }
)
# }}}

# fwdControl(target='data.frame', iters='missing') {{{
setMethod('fwdControl', signature(target='data.frame', iters='missing'),
  function(target) {
    
    # CREATE iters
    dti <- dim(target)
    ite <- array(NA, dim=c(dti[1], 3, 1), dimnames=list(row=1:dti[1], 
      val=c('min', 'value', 'max'), iter=1))

    # FIND val names in target
    vns <- c('min', 'value', 'max')
    nms <- vns %in% colnames(target)
    ite[, vns[nms], 1] <- unlist(c(target[,vns[nms]]))

    return(fwdControl(target=target, iters=ite))
  }
)
# }}}

# fwdControl(target='list', iters='missing') {{{
setMethod('fwdControl', signature(target='list', iters='missing'),
  function(target) {

  foo <- function(...) {

    args <- list(...)
  
    # SEPARATE df and
    df <- as.data.frame(args[!names(args) %in% 'value'])
    
    # array components
    val <- args[['value']]
    
    # TURN val into matrix
    if(is(val, 'list')) {
      mat <- do.call(rbind, val)
    } else {
      mat <- t(matrix(val))
    }
    # CHECK no. rows in iters
    if(nrow(df) > nrow(mat)) {
      mat <- t(matrix(mat, nrow=length(mat), ncol=nrow(df)))
    }

    ele <- new('fwdControl')@target[rep(1, 1),]
    ele[names(df)] <- df

    return(list(target=ele, iters=mat))
  }
  
  if(is(target[[1]], 'list')) {
    
    inp <- lapply(target, function(x) do.call('foo', x))

    ele <- do.call('rbind', lapply(inp, function(x) x$target))
    ite <- do.call('rbind', lapply(inp, function(x) x$iters))

    return(fwdControl(target=ele, iters=ite))

  } else {
    
    inp <- do.call('foo', target)
    return(do.call('fwdControl', inp))
  }
  
    
  }
)

# }}}

# fwdControl(target='missing', iters='missing') {{{
setMethod('fwdControl', signature(target='missing', iters='missing'),
  function(...) {

    args <- list(...)
    
    # SEPARATE df and
    df <- as.data.frame(args[!names(args) %in% 'value'])
    
    # array components
    val <- args[['value']]
    
    # TURN val into matrix
    if(is(val, 'list')) {
      mat <- do.call(rbind, val)
    } else {
      mat <- t(matrix(val))
    }
    # CHECK no. rows in iters
    if(nrow(df) > nrow(mat)) {
      mat <- t(matrix(mat, nrow=length(mat), ncol=nrow(df)))
    }
    return(fwdControl(target=df, iters=mat))
  }
)

# }}}
