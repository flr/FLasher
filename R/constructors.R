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
#' fcn <- fwdControl(data.frame(year=2000:2005, quant='f', value=0.5))

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

    # HACK: reassign quant to keep factors
    trg[,'quant']  <- factor(target$quant, levels=FLasher:::qlevels)

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
    res <- new('fwdControl', target=trg, iters=ite)
    return(
           res[targetOrder(res),]
           )
  }
) 
# }}}

# fwdControl(target='list', iters='missing') {{{
setMethod('fwdControl', signature(target='list', iters='missing'),
  function(target) {

  if(is(target[[1]], 'list')) {

    inp <- lapply(target, function(x) do.call('parsefwdList', x))

    # target
    trg <- do.call('rbind', lapply(inp, '[[', 'target'))

    # iters
    ites <- lapply(inp, '[[', 'iters')
    # dim as 'val', 'iters', 'row'
    dms <- Reduce('rbind', lapply(ites, dim))

    # CHECK iters match (1/N)
    its <- max(dms[,2])

    if(any(dms[,2][dms[,2] > 1] != its))
      stop(paste("Number of iterations in 'iters' must be 1 or", its))

    # FINAL array
    # dim, sum over rows
    dms <- c(3, its, sum(dms[,3]))
    ite <- array(NA, dim=dms, dimnames=list(val=c('min', 'value', 'max'),
      iters=seq(its), row=seq(dms[3])))

    ite[] <- Reduce(c, lapply(ites, c))

    # APERM to 'row', 'val', 'iter'
    ite <- aperm(ite, c(3, 1, 2))

    return(fwdControl(target=trg, iters=ite))

  } else {
    
    inp <- do.call('parsefwdList', target)

    return(do.call('fwdControl', inp))
  }
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

    # DROP value, min, max
    target <- target[!colnames(target) %in% vns[nms]]
    
    return(fwdControl(target=target, iters=ite))
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

# parsefwdList {{{
# RETURNS iters as aperm(c('val', 'iter' ,'row')) for processing
parsefwdList <- function(...) {

    args <- list(...)
  
    # SEPARATE df and ...
    df <- as.data.frame(args[!names(args) %in% c('value', 'min', 'max')])

    #  ... array components
    val <- args[names(args) %in% c('value', 'min', 'max')]

    # TURN val into matrix
    if(is(val, 'list')) {
      mat <- do.call(rbind, val)
    } else {
      mat <- t(matrix(val))
    }

    # NEW target
    trg <- new('fwdControl')@target[rep(1, nrow(df)),]
    trg[names(df)] <- df

    # NEW iters
    ite <- array(NA, dim=c(nrow(trg), 3, ncol(mat)),
      dimnames=list(row=seq(nrow(trg)), val=c('min', 'value', 'max'), iter=seq(ncol(mat))))

    ite <- aperm(ite, c(2, 3, 1))
    ite[match(rownames(mat), dimnames(ite)$val), ,] <- c(mat)

    # RETURNS permutated array!
    return(list(target=trg, iters=ite))
  } # }}}

# targetOrder(object) {{{
targetOrder <- function(object) {

  trg <- object@target
  ite <- object@iters

  # ORDER by timestep (year + season) ...
  # HACK: can only deal with 100 seasons
  tim <- (trg$year * 100) + ifelse(is.character(trg$season), 0, as.numeric(trg$season))
  # ... then 'value' before 'min'/'max'
  pre <- !is.na(ite[,'value',1])

  return(order(tim, pre))
}
# }}}

# targetNo(object) {{{
targetNo <- function(object) {

  trg <- object@target
  ite <- object@iters

  # CALCULATE step

  tim <- (trg$year * 100) + as.numeric(ifelse(trg$season == 'all', 0, trg$season))

  # INDEX for 'value' before 'min'/'max'
  pre <- !is.na(ite[,'value',1])

  idx <- 100 * tim + pre

  return(match(idx, unique(idx)))
}
# }}}
