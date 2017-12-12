# methods.R - DESC
# FLasher/R/methods.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# show {{{
#' Show method for fwdControl
#'
#' More Gills Less Fishcakes
#' @param object A fwdControl
setMethod("show", signature("fwdControl"),
  function(object) {
    
    cat("An object of class \"fwdControl\"\n", sep="")

    # SHOW always year, min, value, max, quantity
    nms <- names(object@target)

    # FIND relevant cols (!unique)
    idx <- apply(object@target[,-c(1,2,15,16,17)], 2,
      function(x) length(unique(x))==1)
    idx2 <- apply(object@target[,c(15,16,17)], 2, function(x) all(is.na(x)))
    nms <- c(nms[c(1,2)], nms[-c(1,2)][!c(idx, idx2)])
    
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
    print(cbind(`(step)`=rownames(object@target), df), row.names=FALSE)
      
    if(dim(object@iters)[3] > 1) {
      cat("   iters: ", dim(object@iters)[3],"\n\n")
    }
  }
) # }}}

# [ {{{
#' Set and replacement accessors for fwdControl
#'
#' We're Pastie to be Grill You
#' @param x A fwdControl object
#' @param i Row of both target and iters
#' @param j Third dimenions of iters
#' @rdname fwdControl-accessors
setMethod("[", signature(x="fwdControl"),
  function(x, i, j) {

    # 'i' applies to rows in both target and iters
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
#' Set and replacement accessors for fwdControl
#' @param k The replacement.
#' @param ... Some things.
#' @rdname fwdControl-accessors
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
        stop("vector can only be assigned to a single column")
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
    if(any(j %in% c('min', 'value', 'max'))) {
      x@iters <- do.call('[<-', c(list(x@iters), argi, list(value=value)))
      x@target <- do.call('[<-', c(list(x@target), arge,
        list(value=mean(do.call('[', c(list(x@iters), argi)), na.rm=TRUE))))
    # other columns
    } else {
      x@target <- do.call('[<-', c(list(x@target), arge, list(value=value)))
    }

    return(x)
  }
)

#' @rdname fwdControl-accessors
setMethod("[<-", signature(x="fwdControl", value="ANY"),
  function(x, i, j, k, ..., value) {
    
    if(j == "biol") {
      if(missing(i)) {
        x@target$biol <- value
      } else {
        biol <- x@target$biol
        biol[i] <- value
        x@target$biol <- biol
      }
    }
    return(x)
  }
)

# }}}

# $ {{{
#' Set and replacement accessors for fwdControl
#' @param name Column name of target or value column of iters.
#' @rdname fwdControl-accessors
setMethod("$", signature(x="fwdControl"),
  function(x, name) {

    if(name == "value")
      return(x@iters[,"value"])
    else
      return(x@target[,name])
  }
) # }}}

# $<- {{{
#' Set and replacement accessors for fwdControl
#' @param value Replacement value
#' @rdname fwdControl-accessors
setMethod("$<-", signature(x="fwdControl", value="vector"),
  function(x, name, value) {
    if(name %in% c("min", "value", "max"))
      x@iters[,name,] <- value
    else
      x@target[,name] <- value
    return(x)
  }
)

#' @rdname fwdControl-accessors
setMethod("$<-", signature(x="fwdControl", value="AsIs"),
  function(x, name, value) {
    x@target <- do.call("$<-", list(x=x@target, name=name, value=value))
    return(x)
  }
) # }}}

# propagate {{{
#' Propagate the fwdControl
#'
#' Change the nuber of iterations in the iter slot of the fwdControl.
#' @param object A fwdControl object.
#' @param iter The number of iterations.
#' @param fill.iter Fill the new iters with original values (TRUE) or NA (FALSE)
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

# summary {{{

#' summary method for fwdControl
#'
#' @name summary
#' @aliases summary,fwdControl-method
#' @docType methods
#' @section Generic function: summary(object)
#' @param object fwdControl object to show summary of
#' @examples
#' control <- fwdControl(data.frame(year=rep(2010:2015, each=2),
#'   quant=c("f", "catch"), min=c(rbind(NA, 20000)), max=c(rbind(NA, 30000)),
#'   value=c(rbind(seq(1, 1.3, length=6), NA))))
#'
#' summary(control)
setMethod("summary", signature(object="fwdControl"),
  function(object) {
  
    relc <- c("relYear", "relSeason", "relFishery", "relCatch", "relBiol")
    reli <- colSums(!is.na(object@target[,relc])) > 0

    # EXTRACT target columns
    tab <- object@target[, c("year", "fishery", "catch", "biol", "quant",
      relc[reli])]
    
    # WILL fishery, catch and biol be output?
    cnas <- apply(tab[,c("fishery", "catch", "biol")], 2,
      function(x) sum(is.na(x)))
    fcbd <- cnas == dim(tab)[1]
    
    # CONVERT NA to empty string
    tab[is.na(tab)] <- character(1)

    # CONVERT list columns to character
    tab <- data.frame(year=tab[,"year"], apply(tab[,-1], 2,
      function(x) unlist(lapply(x, paste, collapse=','))),
      stringsAsFactors=FALSE)

    # CONVERT factor to character
    tab$quant <- as.character(tab$quant)
    
    # FIND rows with ranges 
    idx <- !is.na(object@iters[, "min", 1])
    
    # DUPLICATE tab rows wth min/max
    ind <- rep(seq(1, nrow(tab)), times=as.integer(idx) + 1)
    tab <- tab[ind,]
  
    # EXTRACT iters, COMPACT if needed
    if(dim(object@iters)[3] > 1)
      tis <- apply(object@iters, 1:2, function(x)
        paste0(format(median(x), digits=3), "(", format(mad(x), digits=2), ")"))
    else
      tis <- apply(object@iters, 1:2, function(x)
        ifelse(is.na(x), "", as.character(format(x, digits=3))))
  
    # DUPLICATE tis rows with min/max
    tis <- tis[ind,]
  
    # FIND first row of duplicates
    min <- match(seq(1, length(idx))[idx], ind)
    # MOVE min and max to value
    tis[min, "value"] <- tis[min, "min"]
    tis[min + 1, "value"] <- tis[min + 1, "max"]

    # CREATE long table
    ltab <- cbind(tab, value=tis[,"value"], stringsAsFactors = FALSE)
  
    # add < / > to "quant"
    ltab[min, "quant"] <- paste(ltab[min, "quant"], ">")
    ltab[min + 1, "quant"] <- paste(ltab[min + 1, "quant"], "<")
 
    # PARSE rel*
    if(sum(reli) > 0) {

      # FIND rows with rel
      idx <- ltab[, relc[reli], drop=FALSE] != ""
    
      # HOW MANY repetitions?
      reps <- apply(idx, 1, sum)
    
      # DUPLICATE rel rows
      ind <- rep(seq(1, nrow(tab)), times=reps + 1)
      ltab <- ltab[ind,]
 
      # DUPLICATED rows     
      dup <- match(seq(1, length(reps))[reps>0], ind)

      for(i in seq(length(relc[reli]))) {
      
        # COPY rel* to value
        ltab[dup + i, "value"] <- ltab[dup + i, relc[reli][i]] 
        ltab[dup + i, "quant"] <- paste0("\U2514", "\U2500",
          tolower(sub("rel", "", relc[reli][i])))

        # DROP rel* columns
        ltab[, relc[reli][i]] <- list(NULL)
        }
    }

    # CREATE wide tab 
    wtab <- reshape(ltab, idvar=c("fishery", "catch", "biol", "quant"),
      timevar="year", direction="wide",
      varying=list(as.character(unique(tab$year))))
  
    rownames(wtab) <- NULL
  
    # CHANGE F, C, B names
    colnames(wtab)[1:3] <- c("F", "C", "B")

    # DROP F, C or B if nor used
    wtab <- wtab[, c(c(1:3)[!fcbd], seq(4, dim(wtab)[2]))]

    # DROP NA characters
    wtab[is.na(wtab)] <- character(1)
  
    # PRINT to screen
    cat("An object of class 'fwdControl' with:\n\n")
    print(wtab, row.names = F)
  
    invisible(wtab)
  }
) # }}}
