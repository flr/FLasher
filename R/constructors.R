# constructors.R - Constructor methods for fwdControl
# FLasher/R/constructors.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# fwdControl(target='data.frame', iters='array') {{{
#' fwdControl constructor for data.frame and array
#' @rdname fwdControl
setMethod('fwdControl', signature(target='data.frame', iters='array'),
  function(target, iters, ...) {
    
    # dimensions
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
    trg[,'quant']  <- factor(target$quant, levels=.qlevels)

    # MASTER iters
    ite <- array(NA, dim=c(dtg[1], 3, dit[length(dit)]),
      dimnames=list(row=seq(dtg[1]), val=c('min', 'value', 'max'),
      iter=seq(dit[length(dit)])))

    # MATCH arrays
    if(identical(dim(iters), dim(ite))) {
       ite[,,] <- iters
    # DIMNAMES in array?
    } else if(!is.null(dni)) {
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

    # TODO CHECK quant ~ dims

    # TODO Default fcb

    # REORDER by year, season, value/min-max
    idx <- targetOrder(trg, ite)
    trg <- trg[idx,]
    row.names(trg) <- seq(len=nrow(trg))

    ite <- ite[idx,,,drop=FALSE]
    rownames(ite) <- seq(len=nrow(trg))
    
    return(new('fwdControl', target=trg, iters=ite, ...))
  }
) 
# }}}

# fwdControl(target='data.frame', iters='numeric') {{{
#' fwdControl constructor for data.frame and numeric
#' @rdname fwdControl
setMethod('fwdControl', signature(target='data.frame', iters='numeric'),
  function(target, iters, ...) {

  if(length(iters) > 1)
    stop("'iters' must be of length 1 or of class 'array'")

  # CREATE w/ empty iters
  res <- fwdControl(target=target, ...)
  # then EXTEND
  resits <- res@iters[,,rep(1, iters), drop=FALSE]
  # HACK: fix iters dimnames$iter
  dimnames(resits)$iter <- seq(1, iters)
  res@iters <- resits

  return(res)

  }
) # }}}

# fwdControl(target='data.frame', iters='missing') {{{
#' fwdControl constructor for data.frame and missing
#' @rdname fwdControl
setMethod('fwdControl', signature(target='data.frame', iters='missing'),
  function(target, ...) {
    
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
    
    return(fwdControl(target=target, iters=ite, ...))
  }
)
# }}}

# fwdControl(target='list', iters='missing') {{{

#' fwdControl constructor for list and missing
#' @rdname fwdControl
#' @examples
#' # Single target value
#' fwdControl(list(year=2010:2014, quant='catch', value=2900))  
#' # One value per target (year)
#' fwdControl(list(year=2010:2014, quant='catch', value=seq(2900, 3500, length=5)))  
#' # 40 iters in each target
#' fwdControl(list(year=2010:2014, quant='catch',
#'   value=rnorm(200, seq(2900, 3500, length=5))))  

setMethod('fwdControl', signature(target='list', iters='missing'),
  function(target, ...) {
    
    # target is LIST of LISTS
    if(is(target[[1]], 'list')) {
      
      inp <- lapply(target, function(x) do.call('parsefwdList', x))
      
      # target
      trg <- do.call('rbind', c(lapply(inp, '[[', 'target'), stringsAsFactors=FALSE))

      # iters
      ites <- lapply(inp, '[[', 'iters')
      
      # dims as 'row', 'val', 'iters'
      dms <- Reduce('rbind', lapply(ites, dim))

      # CHECK iters match (1/N)
      its <- max(dms[,3])
      if(any(dms[,3][dms[,3] > 1] != its))
        stop(paste("Number of iterations in 'iters' must be 1 or", its))

      # EXPAND to max iters
      ites[dms[,3] != its]  <- lapply(ites[dms[,3] != its],
        function(x) array(x, dim=c(dim(x)[-3], its)))

      # FINAL array
      # dim, sum over rows
      dms <- c(3, its, sum(dms[,1]))
      ite <- array(NA, dim=dms, dimnames=list(val=c('min', 'value', 'max'),
        iters=seq(its), row=seq(dms[3])))

      ite[] <- Reduce(c, lapply(ites, function(x) c(aperm(x, c(2,3,1)))))

      # APERM to 'row', 'val', 'iter'
      ite <- aperm(ite, c(3, 1, 2))

      return(fwdControl(target=trg, iters=ite, ...))

    } else {
      
      inp <- do.call('parsefwdList', target)

    return(do.call('fwdControl', c(inp, list(...))))
    }
  }
) # }}}

# fwdControl(target='list', iters='list') {{{
#' fwdControl constructor for a series of list
#' @rdname fwdControl
setMethod('fwdControl', signature(target='list', iters='list'),
  function(target, iters, ...) {
    
    args <- list(...)

    if(any(names(args) == 'FCB')) {
      nfcb <- match("FCB", names(args))
      FCB <- args[nfcb]
      args <- args[-nfcb]
    }
    else
      FCB=NULL

    # MERGE all but FCB
    target <- c(list(target, iters), args)
    
    return(do.call('fwdControl',
      c(list(target=target), FCB)))
  }
) # }}}

# fwdControl(target='missing', iters='missing') {{{
#' fwdControl constructor for mising and missing
#' @rdname fwdControl
setMethod('fwdControl', signature(target='missing', iters='missing'),
  function(...) {

    args <- list(...)

    # EMPTY
    if(length(args) == 0)
      return(new("fwdControl"))

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
#' Parse the list argument to fwd to make a fwdControl object
#'
#' Internal function
#' @param ... Things
parsefwdList <- function(...) {
  
  args <- list(...)
 
  if(is(args$biol, 'list') | is(args$biol, 'AsIs')) {
    df <- as.data.frame(args[!names(args) %in% c('value', 'min', 'max', 'biol')],
      stringsAsFactors = FALSE)
    df$biol <- I(args$biol)
  } else {
    df <- as.data.frame(args[!names(args) %in% c('value', 'min', 'max')],
      stringsAsFactors = FALSE)
  }

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
  
  # COMPUTE No. iters
  dite <- max(1, ncol(mat) / nrow(trg))
  # CHECK match with length(values)
  if(dite != floor(dite))
    stop("Number of target values is not a multiple of number of targets")

  # NEW iters
  ite <- array(NA, dim=c(nrow(trg), 3, dite),
    dimnames=list(row=seq(nrow(trg)), val=c('min', 'value', 'max'),
    iter=seq(dite)))

  ite <- aperm(ite, c(2,1,3))
  ite[match(rownames(mat), dimnames(ite)$val),,] <- c(mat)
  ite <- aperm(ite, c(2,1,3))

  # RETURNS permutated array!
  return(list(target=trg, iters=ite))
} # }}}

# targetOrder(object) {{{
#' Get the order of targets in a fwdControl
#'
#' Targets must be processed by FLasher in the correct order.
#' Internal function. Ignore.
#' @param target The target.
#' @param iters The iters.
targetOrder <- function(target, iters) {

  # ORDER by timestep and value/minmax
  tim <- suppressWarnings(as.integer(target$season))
  
  if(all(is.na(tim)))
    tim[] <- 1
  else if(sum(!is.na(tim)) != length(tim))
    stop("Season names cannot be ordered")

  idx <- order(target$year, tim)

  return(idx)
}
# }}}

# FCB {{{
setMethod("FCB", signature(object="ANY"),
  function(object, ...) {

    args <- c(list(object), list(...))

    # OUTLIST list
    if(length(args) == 1 & is(args[[1]], "list"))
      args <- args[[1]]

    # USE matrix
    if(length(args) == 1 & is(args[[1]], "matrix"))
      x <- args[[1]]

  
    # CREATE matrix
    else
      x <- do.call(rbind, args)

    # CHECK dims
    if(dim(x)[2] != 3)
      stop("FCB matrix can only have 3 columns")

    dimnames(x) <- list(seq(1, dim(x)[1]), c("F", "C", "B"))

  return(x)
  }
) # }}}

# guessfcb {{{

#' Generate an FCB matrix from FLBiols and FLFisheries
#'
#' Tries to generate FCB matrix based on names.
#' Internal function. Ignore.
#' @param biols The FLBiols.
#' @param fisheries The FLFisheries.
guessfcb <- function(biols, fisheries) {

  # GET names
  nmf <- names(fisheries)
  nmc <- lapply(fisheries, names)
  nmb <- names(biols)

  fc <- do.call(rbind, lapply(names(nmc), function(x) unlist(cbind(x, nmc[[x]]))))
  b <- nmb[match(fc[,2], nmb)]

  fcb <- cbind(fc[!is.na(b),, drop=FALSE], b[!is.na(b)])
  colnames(fcb) <- c("f", "c", "b")
  rownames(fcb) <- seq(nrow(fcb))

  return(fcb)
}

# fcb2int(fcb, biols, fisheries)
#' fcb2int function
#'
#' Internal function not for public consumption
#' @param fcb The FCB matrix
#' @param biols The biols
#' @param fisheries The fisheries
fcb2int <- function(fcb, biols, fisheries) {
  
  # GET names
  nmf <- names(fisheries)
  nmc <- lapply(fisheries, names)
  nmb <- names(biols)

  fcbint <- array(NA, dim=dim(fcb), dimnames=dimnames(fcb))

  fcbint[,"f"] <- as.integer(match(fcb[,"f"], nmf))
  fcbint[,"b"] <- as.integer(match(fcb[,"b"], nmb))

  for(i in names(nmc))
    fcbint[fcb[,"f"] == i, "c"] <- match(fcb[fcb[,"f"] == i, "c"], nmc[[i]])

  return(fcbint)
} # }}}
