# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("fwdControl constructors")

source("expect_funs.R")


test_that("list constructor - single iter",{
    nyears <- round(runif(1,min=5,max=15))
    inityear <- round(runif(1,min=1990,max=2000)) 
    years <- inityear:(inityear+nyears-1)
    relinityear <- round(runif(1,min=1990,max=2000)) 
    relyears <- relinityear:(relinityear+nyears-1)
    value <- rnorm(nyears)
    min_value <- rnorm(nyears)
    max_value <- rnorm(nyears)
    quant <- sample(.qlevels,1)
    biol <- as.character(signif(rnorm(1)*1000,3))
    # Single value, single iter
    test <- fwdControl(list(year=years, quant=quant, value=value[1], biol=biol))
    expect_equal(nrow(test@target),nyears)
    expect_equal(test@target$year,years)
    expect_equal(as.character(test@target$quant),rep(quant,nyears))
    expect_equal(unname(test@iters[,"value",]),rep(value[1],nyears))
    expect_equal(as.character(test@target$biol),rep(biol,nyears))
    # Target vector, single iter
    test <- fwdControl(list(year=years, quant=quant, value=value, biol=biol))
    expect_equal(nrow(test@target),nyears)
    expect_equal(test@target$year,years)
    expect_equal(as.character(test@target$quant),rep(quant,nyears))
    expect_equal(unname(test@iters[,"value",]),value)
    expect_equal(as.character(test@target$biol),rep(biol,nyears))
    # Relative target
    test <- fwdControl(list(year=years, relYear=relyears, biol=biol, quant=quant, value=value))
    expect_equal(nrow(test@target),nyears)
    expect_equal(test@target$year,years)
    expect_equal(test@target$relYear,relyears)
    expect_equal(as.character(test@target$biol),rep(biol,nyears))
    expect_equal(as.character(test@target$quant),rep(quant,nyears))
    expect_equal(unname(test@iters[,"value",]),value)
    # Bounds min
    test <- fwdControl(list(year=years, quant=quant, biol=biol, min=min_value[1]))
    expect_equal(unname(test@iters[,"min",]), rep(min_value[1], nyears))
    expect_true(all(is.na(test@iters[,"value",])))
    expect_true(all(is.na(test@iters[,"max",])))
    test <- fwdControl(list(year=years, quant=quant, biol=biol, min=min_value))
    expect_equal(unname(test@iters[,"min",]), min_value)
    expect_true(all(is.na(test@iters[,"value",])))
    expect_true(all(is.na(test@iters[,"max",])))
    # Bounds max
    test <- fwdControl(list(year=years, quant=quant, biol=biol, max=max_value[1]))
    expect_equal(unname(test@iters[,"max",]), rep(max_value[1], nyears))
    expect_true(all(is.na(test@iters[,"value",])))
    expect_true(all(is.na(test@iters[,"min",])))
    test <- fwdControl(list(year=years, quant=quant, biol=biol, max=max_value))
    expect_equal(unname(test@iters[,"max",]), max_value)
    expect_true(all(is.na(test@iters[,"value",])))
    expect_true(all(is.na(test@iters[,"min",])))
    # Bounds min & max
    test <- fwdControl(list(year=years, quant=quant, biol=biol, max=max_value[1], min=min_value[1]))
    expect_equal(unname(test@iters[,"max",]), rep(max_value[1], nyears))
    expect_equal(unname(test@iters[,"min",]), rep(min_value[1], nyears))
    expect_true(all(is.na(test@iters[,"value",])))
    test <- fwdControl(list(year=years, quant=quant, biol=biol, max=max_value, min=min_value))
    expect_equal(unname(test@iters[,"max",]), max_value)
    expect_equal(unname(test@iters[,"min",]), min_value)
    expect_true(all(is.na(test@iters[,"value",])))
})

test_that("list constructor multiple lists",{
    nyears <- round(runif(1,min=5,max=15))
    inityear <- round(runif(1,min=1990,max=2000)) 
    years <- inityear:(inityear+nyears-1)
    relinityear <- round(runif(1,min=1990,max=2000)) 
    relyears <- relinityear:(relinityear+nyears-1)
    value1 <- rnorm(nyears)
    value2 <- rnorm(nyears)
    value3 <- rnorm(nyears)
    min_value1 <- rnorm(nyears)
    max_value1 <- rnorm(nyears)
    min_value2 <- rnorm(nyears)
    max_value2 <- rnorm(nyears)
    min_value3 <- rnorm(nyears)
    max_value3 <- rnorm(nyears)
    quant1 <- sample(.qlevels,1)
    quant2 <- sample(.qlevels,1)
    quant3 <- sample(.qlevels,1)
    biol <- as.character(signif(rnorm(1)*1000,3))
    fishery <- as.character(signif(rnorm(1)*1000,3))
    catch <- as.character(signif(rnorm(1)*1000,3))

    # Two lists
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1, biol=biol),
        list(year=years, quant=quant2, value=value2, fishery=fishery, catch=catch))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # Cannot guarantee order within years
    # Check that F, C and B have been set correctly
    # Test value - each 'year' should have the two values in the iter array
    for (yr in years){
        trgt <- subset(test@target,year==yr)
        expect_equal(nrow(trgt),2)
        # one row has F and C, the other B
        for (i in 1:nrow(trgt)){
            expect_true((all(is.na(trgt[i,c("fishery","catch")])) & (trgt[i,"biol"] == biol)) | ((trgt[i,"fishery"] == fishery) & (trgt[i,"catch"] == catch) & is.na(trgt[i,"biol"])))
        }
    }
    for (i in 1:nyears){
        expect_true(all(test@iters[((i-1)*2+1):((i-1)*2+2),"value",] %in% c(value1[i], value2[i])))
    }
    expect_true(all(is.na(test@iters[,"min",])))
    expect_true(all(is.na(test@iters[,"max",])))

    # Two lists with relYear
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1, biol=biol),
        list(year=years, relYear=relyears, quant=quant2, value=value2, fishery=fishery, catch=catch))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # Rel year and F, C, B
    for (yr in years){
        trgt <- subset(test@target,year==yr)
        expect_equal(nrow(trgt),2)
        # one row has relyear, F and C, the other B
        for (i in 1:nrow(trgt)){
            expect_true((all(is.na(trgt[i,c("fishery","catch", "relYear")])) & (trgt[i,"biol"] == biol)) | ((trgt[i,"relYear"] == relyears[which(years == yr)]) & (trgt[i,"fishery"] == fishery) & (trgt[i,"catch"] == catch) & is.na(trgt[i,"biol"])))
        }
    }
    # Test value
    for (i in 1:nyears){
        expect_true(all(test@iters[((i-1)*2+1):((i-1)*2+2),"value",] %in% c(value1[i], value2[i])))
    }
    expect_true(all(is.na(test@iters[,"min",])))
    expect_true(all(is.na(test@iters[,"max",])))

    # Two lists with relYear and min bounds
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1, biol=biol),
        list(year=years, relYear=relyears, quant=quant2, min=min_value2, fishery=fishery, catch=catch))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # Rel year and F, C, B
    for (yr in years){
        trgt <- subset(test@target,year==yr)
        expect_equal(nrow(trgt),2)
        # one row has relyear, F and C, the other B
        for (i in 1:nrow(trgt)){
            expect_true((all(is.na(trgt[i,c("fishery","catch", "relYear")])) & (trgt[i,"biol"] == biol)) | ((trgt[i,"relYear"] == relyears[which(years == yr)]) & (trgt[i,"fishery"] == fishery) & (trgt[i,"catch"] == catch) & is.na(trgt[i,"biol"])))
        }
    }
    expect_equal(nrow(test@iters),nyears*2)
    # Test value - we cannot guarantee that min / max always come last in order
    for (i in nrow(test@iters)){
        (is.na(test@iters[i,"min",]) & test@iters[i,"value",] %in% value1[ceiling(i/2)]) | (test@iters[i,"min",] %in% min_value2[ceiling(i/2)] & is.na(test@iters[i,"value",]))
    }
    expect_true(all(is.na(test@iters[,"max",])))

    # Two lists with relYear and max bounds
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1, biol=biol),
        list(year=years, relYear=relyears, quant=quant2, max=max_value2, fishery=fishery, catch=catch))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # Rel year and F, C, B
    for (yr in years){
        trgt <- subset(test@target,year==yr)
        expect_equal(nrow(trgt),2)
        # one row has relyear, F and C, the other B
        for (i in 1:nrow(trgt)){
            expect_true((all(is.na(trgt[i,c("fishery","catch", "relYear")])) & (trgt[i,"biol"] == biol)) | ((trgt[i,"relYear"] == relyears[which(years == yr)]) & (trgt[i,"fishery"] == fishery) & (trgt[i,"catch"] == catch) & is.na(trgt[i,"biol"])))
        }
    }
    expect_equal(nrow(test@iters),nyears*2)
    # Test value - we cannot guarantee that max / max always come last in order
    for (i in nrow(test@iters)){
        (is.na(test@iters[i,"max",]) & test@iters[i,"value",] %in% value1[ceiling(i/2)]) | (test@iters[i,"max",] %in% max_value2[ceiling(i/2)] & is.na(test@iters[i,"value",]))
    }
    expect_true(all(is.na(test@iters[,"min",])))

    # Two lists with relYear and min and max bounds - fails as above
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1, biol=biol),
        list(year=years, relYear=relyears, quant=quant2, min=min_value2, max=max_value2, fishery=fishery, catch=catch))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # Rel year and F, C, B
    for (yr in years){
        trgt <- subset(test@target,year==yr)
        expect_equal(nrow(trgt),2)
        # one row has relyear, F and C, the other B
        for (i in 1:nrow(trgt)){
            expect_true((all(is.na(trgt[i,c("fishery","catch", "relYear")])) & (trgt[i,"biol"] == biol)) | ((trgt[i,"relYear"] == relyears[which(years == yr)]) & (trgt[i,"fishery"] == fishery) & (trgt[i,"catch"] == catch) & is.na(trgt[i,"biol"])))
        }
    }
    # Test value - we cannot guarantee that max / max always come last in order
    for (i in nrow(test@iters)){
        (is.na(test@iters[i,"max",]) & is.na(test@iters[i,"min",]) & test@iters[i,"value",] %in% value1[ceiling(i/2)]) | (test@iters[i,"min",] %in% min_value2[ceiling(i/2)] & test@iters[i,"max",] %in% max_value2[ceiling(i/2)] & is.na(test@iters[i,"value",]))
    }

    # Multiple lists
    # Assume that order is order of the lists, then ordered by year
    # Else very complicated
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1, biol=biol),
        list(year=years, relYear = relyears, quant=quant2, max=max_value2, biol=biol),
        list(year=years, quant=quant3, min=min_value3, fishery=fishery, catch=catch))
    # Check year
    expect_equal(nrow(test@target), 3*nyears)
    expect_equal(test@target$year, rep(years, each=3))
    # Check relYear - second list
    list1indices <- (1:(3*nyears)) %in% seq(1, 3*nyears-2, by=3)
    list2indices <- (1:(3*nyears)) %in% seq(2, 3*nyears-1, by=3)
    list3indices <- (1:(3*nyears)) %in% seq(3, 3*nyears, by=3)
    expect_equal(test@target$relYear[list2indices], relyears)
    expect_true(all(is.na(test@target$relYear[!list2indices])))
    # F,C and B
    expect_equal(test$biol[list1indices], rep(biol, nyears))
    expect_true(all(is.na(test@target[list1indices, c("fishery","catch")])))
    expect_equal(test$biol[list2indices], rep(biol, nyears))
    expect_true(all(is.na(test@target[list2indices, c("fishery","catch")])))
    expect_equal(test$catch[list3indices], rep(catch, nyears))
    expect_equal(test$fishery[list3indices], rep(fishery, nyears))
    expect_true(all(is.na(test@target[list3indices, "biol"])))
    # Values - expect order to be determined by order of lists
    expect_equal(unname(test@iters[list1indices,"value",]), value1)
    expect_true(all(is.na(test@iters[!list1indices,"value",])))
    expect_equal(unname(test@iters[list2indices,"max",]), max_value2)
    expect_true(all(is.na(test@iters[!list2indices,"max",])))
    expect_equal(unname(test@iters[list3indices,"min",]), min_value3)
    expect_true(all(is.na(test@iters[!list3indices,"min",])))
})


test_that("list constructor - iters",{
    nyears <- round(runif(1,min=5,max=15))
    value <- rnorm(nyears)
    inityear <- round(runif(1,min=1990,max=2000)) 
    years <- inityear:(inityear+nyears-1)
    relinityear <- round(runif(1,min=1990,max=2000)) 
    relyears <- relinityear:(relinityear+nyears-1)
    quant1 <- sample(.qlevels,1)
    quant2 <- sample(.qlevels,1)
    biol <- as.character(signif(rnorm(1)*1000,3))
    fishery <- as.character(signif(rnorm(1)*1000,3))
    catch <- as.character(signif(rnorm(1)*1000,3))
    # Assume FCB columns and relyear and all that are OK - just check values

    # Multiple values over years and different over multiple iterations 
    # How are values recycled? by year or by iter?
    niters <- round(runif(1,min=5,max=15))
    nvalue <- rnorm(nyears * niters)
    test <- fwdControl(list(year=years, quant=quant1, value=nvalue))
    expect_equal(nrow(test@target), nyears)
    expect_equal(dim(test@iters)[1], nyears)
    expect_equal(dim(test@iters)[3], niters)
    # Check recycling - years before iters
    expect_equal(nvalue, c(test@iters[,"value",]))

    # Two lists, one with iterations and years in the value, one with a single value for all years and iters
    niters <- round(runif(1,min=5,max=15))
    nvalue <- rnorm(nyears * niters)
    test <- fwdControl(list(year=years, quant=quant1, value=nvalue), # iters
                       list(year=years, quant=quant2, min=value[1])) # no iters
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(dim(test@iters)[1], 2*nyears)
    expect_equal(dim(test@iters)[3], niters)
    # Values
    expect_equal(nvalue, c(test@iters[seq(1,(nyears*2)-1,2),"value",]))
    expect_equal(rep(value[1],nyears*niters), c(test@iters[seq(2,nyears*2,2),"min",]))

    # Two lists, one with iterations and years in the value, one with a single value for all years and iters
    niters <- round(runif(1,min=5,max=15))
    nvalue <- rnorm(nyears * niters)
    test <- fwdControl(list(year=years, quant=quant1, value=nvalue), # iters
                       list(year=years, quant=quant2, min=value)) # no iters
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(dim(test@iters)[1], 2*nyears)
    expect_equal(dim(test@iters)[3], niters)
    # Values - all iters of min are the same
    expect_equal(nvalue, c(test@iters[seq(1,(nyears*2)-1,2),"value",]))
    expect_equal(rep(value,niters), c(test@iters[seq(2,nyears*2,2),"min",]))




#    # Same single value over years but different over multiple iterations 
#    # Number of niters != nyears
#    # Pass in repetitions of the target?
#    niters <- nyears + 1
#    nvalue <- rnorm(niters)
#    test <- fwdControl(list(year=years, quant=quant1, value=nvalue))
#
#
#    # Same single value over years but different over multiple iterations 
#    # Number of niters != nyears
#    # And works when number of niters == nyears
#    # How
#    niters <- nyears
#    nvalue <- rnorm(niters)
#    test <- fwdControl(list(year=years, quant=quant1, value=nvalue))
#
#    # Without a list? What is this?
#    # this is dangerous - not allow this
#    test <- fwdControl(year=years, quant=quant1, value=nvalue)


})

test_that("Biol based targets with list of Biols",{
    nyears <- round(runif(1,min=5,max=15))
    value <- rnorm(nyears)
    inityear <- round(runif(1,min=1990,max=2000)) 
    years <- inityear:(inityear+nyears-1)
    fcb <- matrix(c(1,1,1,2,1,2), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
    # biol by number
    ctrl <- fwdControl(list(year=years, quant="catch", value=value, biol=G(1,2)), FCB=fcb)
    expect_equal(unname(ctrl@iters[,"value",]), value)
    expect_is(ctrl$biol, "list")
    dump <- lapply(ctrl$biol, function(x){expect_equal(x, c(1,2))})
    # biol by name
    ctrl <- fwdControl(list(year=years, quant="catch", value=value, biol=G("ple","sol")), FCB=fcb)
    expect_equal(unname(ctrl@iters[,"value",]), value)
    expect_is(ctrl$biol, "list")
    dump <- lapply(ctrl$biol, function(x){expect_equal(x, c("ple","sol"))})
    # iterations in the target
    niters <- round(runif(1,min=10,max=20))
    nvalue <- rnorm(niters*nyears)
    ctrl <- fwdControl(list(year=years, quant="catch", value=nvalue, biol=G("ple","sol")), FCB=fcb)
    expect_equal(unname(c(ctrl@iters[,"value",])), nvalue)
    # relative target
    ctrl <- fwdControl(list(year=years, quant="catch", value=value, biol=G("ple","sol"), relYear=years-1, relBiol=G("ple","sol")), FCB=fcb)
    expect_equal(unname(ctrl@iters[,"value",]), value)
    expect_is(ctrl$biol, "list")
    dump <- lapply(ctrl$biol, function(x){expect_equal(x, c("ple","sol"))})
    expect_is(ctrl$relBiol, "list")
    dump <- lapply(ctrl$relBiol, function(x){expect_equal(x, c("ple","sol"))})
})


test_that("Combined fishery / catch lists",{
    nyears <- round(runif(1,min=5,max=15))
    value <- rnorm(nyears)
    inityear <- round(runif(1,min=1990,max=2000)) 
    years <- inityear:(inityear+nyears-1)
    # Two fisheries, one catch each, fishing on same single biol
    fcb <- matrix(c(1,2,1,1,1,1), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
    ctrl <- fwdControl(list(year=years, quant="catch", value=value, fishery=G("bt", "gn"), catch=G("btc", "gnc")))
    expect_equal(unname(ctrl@iters[,"value",]), value)
    expect_is(ctrl$fishery, "list")
    expect_is(ctrl$catch, "list")
    dump <- lapply(ctrl$fishery, function(x){expect_equal(x, c("bt","gn"))})
    dump <- lapply(ctrl$catch, function(x){expect_equal(x, c("btc","gnc"))})
})

