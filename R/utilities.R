# utilities.R - DESC
# /utilities.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# calc_F {{{
# F = effort * selectivity * alpha * biomass ^ -beta
# operating_model.cpp: * F = effort * selectivity * alpha * biomass ^ -beta
#' Calculate fishing mortality
#'
#' Calculate F in the same way as the internal C++ does
#' @param catch The FLCatch
#' @param biol The FLBiol
#' @param effort The fishing effort
calc_F <- function(catch, biol, effort){
    biomass <- biol@n * biol@wt
    F <- effort %*% catch@catch.sel %*% catch@catch.q['alpha',] %*%
      (biomass %^% -catch@catch.q['beta',])
    quant(F) <- "age"
    units(F) <- "f"
    return(F)
} # }}}

# fillchar {{{
#' fillchar for FLFisheries
#' @rdname fillchar
setMethod("fillchar", signature("FLFisheries"),
  function(object) {

    if(length(object@desc) == 0)
      object@desc <- character(1)

    lapply(object, function(x) {
      if(length(name(x)) == 0)
        name(x) <- character(1)
      if(length(desc(x)) == 0)
        desc(x) <- character(1)
      lapply(x, function(y) {
        if(length(name(y)) == 0)
          name(y) <- character(1)
        if(length(desc(y)) == 0)
          desc(y) <- character(1)
        return(y)
      })
      return(x)
    })
  }
)

#' fillchar for FLBiols
#' @rdname fillchar
setMethod("fillchar", signature("FLBiols"),
  function(object) {

    if(length(object@desc) == 0)
      object@desc <- character(1)

    lapply(object, function(x) {
      if(length(name(x)) == 0)
        name(x) <- character(1)
      if(length(desc(x)) == 0)
        desc(x) <- character(1)
      return(x)
    })
  }
) # }}}

# G {{{

#' @rdname fwdControl
#' @examples
#' # Construct a fwdControl with some targets having multiple Biols, specified using the G() function
#' fwdControl(list(year=2000:2001, value=200, quant="catch", biol=G(1,2)),
#'   list(year=2002:2003, value=100, quant="catch", biol=c(1,2)))
G <- function(...)
  return(I(list(unlist(list(...)))))
# }}}

# target_order {{{

#' Add the order column to the control target
#'
#' Add the order column to the control target data.frame so that targets are
#' processed in the correct order.
#'
#' It is important that the targets in the control object are processed in the
#' correct order. Targets can happen simultaneously. For example, if there are
#' multiple FLFishery objects in operating model each will need to have a target
#' to solve for at the same time as the others. The targets are processed in a
#' time ordered sequence (year / season).
#' However, within the same year and season it is necessary for the min and max
#' targets to be processed separatley and after the other targets.
#'
#' @param control A fwdControl object
#' @return A fwdControl object with an order column.


add_target_order_fls <- function(control) {

  target <- control@target

  # KEEP original order
  target$orig_order <- seq(1, nrow(control@target))

  # min and max values?
  target$minmax <- is.na(control@iters[ ,"value", 1])

  # ORDER by year, season, minmax, orig_order 
  target <- target[order(target$year, target$season, target$minmax,
    target$orig_order),]

  #
  target$order <- seq(1, nrow(target))

  control@iters <- control@iters[target$orig_order, , , drop=FALSE]

  target <- target[,colnames(target) != "minmax"]
  target <- target[,colnames(target) != "orig_order"]

  control@target <- target

  return(control)
}


add_target_order <- function(control){
    
  # Add temporary original order column - order gets messed about with merge
  control@target$orig_order <- 1:nrow(control@target)
  
  # Add temporary minmax column
  control@target$minmax <- is.na(control@iters[,"value",1])
  sim_targets <- unique(control@target[,c("year","season","minmax")])
  
  # Order by year / season / minmax
  sim_targets <- sim_targets[order(sim_targets$year, sim_targets$season,
    sim_targets$minmax),]
  
  sim_targets$order <- 1:nrow(sim_targets)
  
  # Problem - merge reorders by order column
  control@target <- merge(control@target, sim_targets) # order should be the same
  
  # Reorder by original order so that target and iters slots are consistent
  control@target <- control@target[order(control@target$orig_order),]
  
  # Reorder target and iters slots by new order
  # Just get first element of each list element in fishery, catch and biol
  new_order <- order(control@target$order, unlist(lapply(control@target$fishery,
    "[[", 1)), unlist(lapply(control@target$catch, "[[", 1)), 
    unlist(lapply(control@target$biol, "[[", 1)))
  
  control@target <- control@target[new_order,]
  control@iters <- control@iters[new_order,,,drop=FALSE]
  
  # Remove minmax and orig_order columns
  control@target <- control@target[,colnames(control@target) != "minmax"]
  control@target <- control@target[,colnames(control@target) != "orig_order"]
  
  return(control)
} # }}}

# match_posns_names {{{
#' Change names of biols, catches and fisheries in the control object into integer positions
#'
#' Change names of biols, catches and fisheries in the control object into integer positions
#'
#' Before calling the C++ code it is necessary for the catch, fishery and biol columns (and their Rel) equivalents
#' to be integers. The user can specify by name. This function changes the name to integer position and throws an
#' error if the name does not match.
#'
#' @param trg The target slot of a fwdControl object
#' @param biol_names A vector of names in the FLBiols objects
#' @param fishery_catch_names A named list - elements of list are vector of the catch names of each fishery
#' @return The updated target slot

match_posns_names <- function(trg, biol_names, fishery_catch_names){



    # Biols
    for(biol_col in c("biol", "relBiol")){
        # biol column is a list
        if (is.character(unlist(trg[,biol_col]))){
            # Check that all biol names in target exist
            if (!(all(unlist(trg[,biol_col]) %in% c(NA,biol_names)))){
                stop(paste("Names in the ", biol_col, " column of the control object do not match names of the FLBiols object\n", sep=""))
            }
            # Match positions
            # This drops the list
            trg <- do.call("$<-", list(trg,biol_col, lapply(trg[,biol_col], match, biol_names))) # Need to use $<- as [,biol_col] unpacks list
        }
    }
    # Fisheries
    for (fishery_col in c("fishery", "relFishery")){
        # fishery column is a list
        if (is.character(unlist(trg[,fishery_col]))){
            fishery_names <- names(fishery_catch_names)
            # Check that all biol names in target exist
            if (!(all(unlist(trg[,fishery_col]) %in% c(NA,fishery_names)))){
                stop(paste("Names in the ", fishery_col, " column of the control object do not match names of the FLFisheries object\n", sep=""))
            }
            # Match positions
            trg <- do.call("$<-", list(trg,fishery_col, lapply(trg[,fishery_col], match, fishery_names)))
        }
    }

    # Catches - trickier as must relate to the fishery
    for (cfcols in list(c("relCatch", "relFishery"), c("catch","fishery"))){
        # fishery column is a list
        catch_col <- cfcols[[1]]
        fishery_col <- cfcols[[2]]
        if(is.character(unlist(trg[,catch_col]))){
            # Just go row by row to keep it simple and correct
            for (fc in 1:nrow(trg)){
                new_catch <- NULL
                # Loop over each element in the list of that row
                for (cn in 1:length(trg[fc, catch_col][[1]])){
                    # get the catch name and fishery no from the target frame and check the combination exists
                    catch_name <- trg[fc,catch_col][[1]][cn]
                    fishery_no <- trg[fc,fishery_col][[1]][cn]
                    if (!(catch_name %in% c(NA,fishery_catch_names[[fishery_no]]))){
                        stop(paste("Names in the ", catch_col, " column of the control object do not match FLCatch names of the corresponding FLFishery object\n", sep=""))
                    }
                    # match and add to new list
                    new_catch <- c(new_catch, match(trg[fc,catch_col][[1]][cn], fishery_catch_names[[fishery_no]]))
                }
                trg[fc,] <- do.call("$<-", list(trg[fc,], catch_col, list(new_catch)))
            }
        }
    }
    return(trg)
}
# }}}

# parsefwdList {{{

#' Parse the list argument to fwd to make a fwdControl object
#'
#' Internal function
#' @param ... Things

parsefwdList <- function(...) {
  
  args <- list(...)

  # Identify which of biol, relBiol, fishery, catch, relFishery, relCatch are lists
  fcbcols <- c("biol", "fishery", "catch", "relBiol", "relFishery", "relCatch")
  fcbcols_list <- unlist(lapply(args[fcbcols],
    function(x) return(is(x, "list") | is(x, "AsIs"))))
  listcols <- fcbcols[fcbcols_list]
  
  # Make a data.frame dropping those columns 
  dropcols <- c('value', 'min', 'max', listcols)
  df <- as.data.frame(args[!names(args) %in% dropcols], stringsAsFactors = FALSE)
  
  # if any (list | asis) are true, make a df without those cols, then add list
  for (i in listcols){
    df <- do.call("$<-", list(df, i, I(args[[i]])))
  }

  #  ... array components
  val <- lapply(args[names(args) %in% c('value', 'min', 'max')], c)

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

  # BUT consider FLPar
  if(is(args$value, "FLPar"))
    dite <- max(unlist(lapply(val, length)))

  # CHECK match with length(values)
  if(dite != floor(dite))
    stop("Number of target values is not a multiple of number of targets")

  # NEW iters
  ite <- array(NA, dim=c(nrow(trg), 3, dite),
    dimnames=list(row=seq(nrow(trg)), val=c('min', 'value', 'max'),
    iter=seq(dite)))
  
  if(is(args$value, "FLPar")) {
    ite <- aperm(ite, c(2,3,1))
    ite[match(rownames(mat), dimnames(ite)$val),,] <- c(mat)
    ite <- aperm(ite, c(3,1,2))
  } else {
    ite <- aperm(ite, c(2,1,3))
    ite[match(rownames(mat), dimnames(ite)$val),,] <- c(mat)
    ite <- aperm(ite, c(2,1,3))
  }
  
  # RETURNS permutated array!
  return(list(target=trg, iters=ite))
} # }}}

# targetOrder {{{

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

# findNAs {{{
  findNAs <- function(x, slots) {

    # GET values
    vals <- lapply(setNames(nm=slots), function(s) do.call(s, list(x)))

    # IS there any NA?
    nas <- unlist(lapply(vals, anyNA))

    # RETURN slot name(s) if any NA
    if(any(nas))
      return(names(nas)[nas])

    return(NULL)
  }
# }}}
