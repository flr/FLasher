# Classes and methods for plotting FCB diagrams
setClass("basicBlock",
    representation(
        height = "numeric",
        width = "numeric",
        x_centre = "numeric",
        y_centre = "numeric",
        name = "character",
        name_cex = "numeric"
    ),
    prototype(
        height = NA_real_,
        width =  NA_real_,
        x_centre =  NA_real_,
        y_centre =  NA_real_,
        name = NA_character_,
        name_cex =  NA_real_
    )
)

setClass("biolBlock",
    representation(
        neck_length = "numeric",
        circle_cex = "numeric"
    ),
    prototype(
        neck_length =  NA_real_,
        circle_cex =  NA_real_
    ),
    contains = "basicBlock"
)

setClass("fisheryBlock",
    representation(
        no_tails = "integer",
        neck_length = "numeric",
        tail_gap = "numeric",
        tail_length = "numeric"
    ),
    prototype(
        no_tails = NA_integer_,
        branch_length =  NA_real_,
        neck_length =  NA_real_,
        tail_gap = NA_real_,
        tail_length = NA_real_
    ),
    contains = "basicBlock"
)

setClass("catchBlock",
    representation(
        tail_length = "numeric"
    ),
    prototype(
        tail_length = NA_real_
    ),
    contains = "basicBlock"
)

setClass("FCBDrawing",
    representation(
        biolBlocks = "list",
        fisheryBlocks = "list",
        catchBlocks = "list",
        CBConnectors = "list",
        FCB = "matrix"),
    prototype(
        biolBlocks = list(),
        fisheryBlocks = list(),
        catchBlocks = list(),
        CBConnectors = list(),
        FCB = matrix()
    )
)

setGeneric("draw", function(object){
    standardGeneric("draw")
})

setMethod("draw", signature(object="basicBlock"),
    function(object){
        # Draw the box round centre with height and width
        rect(object@x_centre - (object@width/2),
            object@y_centre - (object@height/2),
            object@x_centre + (object@width/2),
            object@y_centre + (object@height/2))
        # Add the text
        text(x=object@x_centre, y=object@y_centre, labels=object@name, cex=object@name_cex)
})

setMethod("draw", signature(object="biolBlock"),
    function(object){
        # Call the basic drawing method
        callNextMethod()
        # Add the neck
        lines(x=c(object@x_centre, object@x_centre), y=c(object@y_centre + (object@height/2), object@y_centre + (object@height/2) + object@neck_length)) 
        # Add the circle 
        points(x=object@x_centre, y=object@y_centre + (object@height/2) + object@neck_length, pch=21, cex=object@circle_cex, col="black", bg="white")
})

setMethod("draw", signature(object="fisheryBlock"),
    function(object){
        # Call the basic drawing method
        callNextMethod()
        # Add the neck
        lines(x=c(object@x_centre, object@x_centre), y=c(object@y_centre - (object@height/2), object@y_centre - (object@height/2) - object@neck_length)) 
        # Add the branch
        branch_y <- object@y_centre - (object@height/2) - object@neck_length
        branch_length <- (object@no_tails -1) * object@tail_gap
        branch_x1 <- object@x_centre - branch_length/2
        branch_x2 <- object@x_centre + branch_length/2
        lines(x=c(branch_x1, branch_x2), y=c(branch_y, branch_y))
        # Or set tail gap as member instead of branch length?
        # Add the tails
        for (tail_count in 1:object@no_tails){
            tail_x <- branch_x1 + ((tail_count - 1) * object@tail_gap)
            lines(x=c(tail_x, tail_x), y=c(branch_y, branch_y - object@tail_length))
        }
})

setMethod("draw", signature(object="catchBlock"),
    function(object){
        # Call the basic drawing method
        callNextMethod()
        # Add the tail
        lines(x=c(object@x_centre, object@x_centre), y=c(object@y_centre - (object@height/2), object@y_centre - (object@height/2) - object@tail_length)) 
})

# Constructor for FCBDrawing
setGeneric("FCBDrawing", function(FCB){
    standardGeneric("FCBDrawing")
})

setMethod("FCBDrawing", signature(FCB="matrix"),
    function(FCB){
        out <- new("FCBDrawing", FCB=FCB)

        # Sort out the FCB
        FCB <- data.frame(FCB)
        colnames(FCB) <- c("F","C","B")
        # FCB must be sorted by fishery and catch
        FCB <- FCB[order(FCB[,"F"],FCB[,"C"]),,drop=FALSE]
        # Need to add catch position in list - catches can catch multiple biols
        unique_FCB <- unique(FCB[,c("F","C")])
        unique_FCB <- cbind(unique_FCB,catch_posn = 1:nrow(unique_FCB))
        FCB <- merge(FCB, unique_FCB)
        # Note that FCB is now a data.frame

        # Ugh... loads of mucking about to get the right height, widths and lengths depending on FCB
        plot_width <- 1
        plot_height <- 1
        # Most number of blocks in a layer
        # Number of blocks in each layer
        no_fisheries <- max(FCB[,"F"])
        no_catches <- sum(tapply(FCB[,"C"],FCB[,"F"],max))
        no_biols <- max(FCB[,"B"])
        max_blocks <- max(no_fisheries, no_catches, no_biols)
        # Width based on biggest layer
        gap_width_block_width <- 1.1
        width <- plot_width / (gap_width_block_width * (max_blocks + 1) + max_blocks)

        # Height cannot be too big, else cannot fit vertical space in
        height_neck <- 4 # The ideal height / neck ratio - ideally, height is 4 x neck
        # max height / neck ratio is 5/3
        # This then makes the height of each box (based on 3 * boxes + 5 * necks)
        max_height <- (plot_height * height_neck) / (3 * height_neck + 5) # height if neck = height / height_neck
        # If we use this box height, he neck length is
        max_neck_length <- (plot_height - 3 * max_height) / 5 # actual neck length to accomodate the height
        # This might be too small so we set a minimum neck length
        min_neck_length <- 0.1
        neck_length <- max(min_neck_length, max_neck_length)
        # Base height on the neck length
        height <- (plot_width - 5 * neck_length) / 3
        # This height might be too big for the width, conversely the width might be too big for the height
        # We take the min
        height <- min(height, width)
        width <- height
        tail_length <- neck_length

        # Make the catches
        catches <- list()
        gap_width <- (plot_width - (no_catches * width)) / (no_catches +1)
        y_centre <- height + (3 * neck_length) + (height / 2)
        # Spread boxes equally across plotting space
        x_centres <- ((1:no_catches) * gap_width) + (((1:no_catches)-1) * width) + (width / 2)
        for (i in 1:no_catches){
            name <- paste("Catch",i,sep="")
            nchar_name <-  nchar(name)
            name_cex <- (width * 50) / nchar_name
            catches[[i]] <- new("catchBlock", height = height, width = width, x_centre = x_centres[i], y_centre = y_centre, tail_length=tail_length, name = name, name_cex=name_cex) 
        }

        # Make the fisheries - based on catches position
        fisheries <- list()
        y_centre <- (2.5 * height) + (5 * neck_length)
        for (i in 1:no_fisheries){
            # x_centre depends on number of catches in fishery and total number of catches and position in list
            # Catches out of all catches that fishery has
            FCs <- unique(FCB[FCB[,"F"]==i,"catch_posn"])
            # Block should sit between them
            catches_xrange <- range(unlist(lapply(catches[FCs], function(x) x@x_centre)))
            x_centre <- ((catches_xrange[2] - catches_xrange[1]) / 2) + catches_xrange[1]
            # tail_gap
            tail_gap <- catches_xrange[2] - catches_xrange[1]
            name <- paste("Fishery",i,sep="")
            nchar_name <-  nchar(name)
            name_cex <- (width * 50) / nchar_name
            fisheries[[i]] <- new("fisheryBlock", height = height, width = width, x_centre = x_centre, y_centre = y_centre, no_tails = length(FCs), tail_gap = tail_gap, neck_length=neck_length, tail_length=tail_length, name = name, name_cex=name_cex) 
        }

        # Make the Biols
        biols <- list()
        y_centre <- (height/2)
        circle_cex <- width * 20
        gap_width <- (plot_width  - (no_biols * width)) / (no_biols +1)
        # Spread boxes equally across plotting space
        x_centres <- ((1:no_biols) * gap_width) + (((1:no_biols)-1) * width) + (width / 2)
        # name_cex based on nchar
        for (i in 1:no_biols){
            name <- paste("Biology",i,sep="")
            nchar_name <-  nchar(name)
            name_cex <- (width * 50) / nchar_name
            biols[[i]] <- new("biolBlock", height = height, width = width, x_centre = x_centres[i], y_centre = y_centre, neck_length = neck_length, circle_cex = circle_cex, name = name, name_cex=name_cex) 
        }

        # Connections
        # List of coord arrays: x1,y1, x2,y2
        connectors <- list()
        for (i in 1:nrow(FCB)){
            # Position in list of biols - straightforward
            biol <- biols[[FCB[i,"B"]]]
            catch <- catches[[FCB[i,"catch_posn"]]]
            catch_y <- catch@y_centre - (catch@height/2) - catch@tail_length
            catch_x <- catch@x_centre
            biol_x <- biol@x_centre
            biol_y <- biol@y_centre + (biol@height/2) + biol@neck_length 
            connectors[[i]] <- c(catch_x = catch_x, catch_y = catch_y, biol_x = biol_x, biol_y = biol_y)
        }

        out@catchBlocks <- catches
        out@fisheryBlocks <- fisheries
        out@biolBlocks <- biols
        out@CBConnectors <- connectors
        return(out)
})

setMethod("draw", signature(object="FCBDrawing"),
    function(object){
        plot(x=c(0,1), y=c(0,1), type="n", xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", ,bty="n")
        invisible(lapply(object@catchBlocks, draw))
        invisible(lapply(object@fisheryBlocks, draw))
        invisible(lapply(object@CBConnectors, function(coords){
            lines(x=c(coords["catch_x"], coords["biol_x"]), y = c(coords["catch_y"], coords["biol_y"]))
        }))
        invisible(lapply(object@biolBlocks, draw))
})

setMethod("draw", signature(object="matrix"),
    function(object){
        fcbd <- FCBDrawing(object)
        draw(fcbd)
})

# Examples
#FCB1 <- matrix(c(1,1,1), nrow=1)
#FCB2 <- matrix(c(1,2,1,1,1,1), nrow=2)
#FCB3 <- matrix(c(1,1,1,1,2,2,2,1,2,2,2,3,2,2,4), nrow=5, byrow=TRUE)
#FCB4 <- matrix(c(1,1,1,1,2,2,2,1,1,2,2,2), nrow=4, byrow=TRUE)
#FCB5 <- matrix(c(1,1,1,1,2,2), nrow=2, byrow=TRUE)
#
#draw(FCB1)
#draw(FCB2)
#draw(FCB3)
#draw(FCB4)
#draw(FCB5)


