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
    biol <- "Mars"
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
    # Bounds min & max - fails
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
    biol <- "Mars"

    # Two lists
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1),
        list(year=years, quant=quant2, value=value2))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # Test value
    for (i in 1:nyears){
        expect_true(all(test@iters[((i-1)*2+1):((i-1)*2+2),"value",] %in% c(value1[i], value2[i])))
    }
    expect_true(all(is.na(test@iters[,"min",])))
    expect_true(all(is.na(test@iters[,"max",])))

    # Two lists with relYear
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1),
        list(year=years, relYear=relyears, quant=quant2, value=value2))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # relyears every other year
    expect_equal(test@target$relYear[seq(2,nyears*2,by=2)], relyears)
    expect_equal(test@target$relYear[seq(1,(nyears*2)-1,by=2)], rep(as.integer(NA),nyears))
    # Test value
    for (i in 1:nyears){
        expect_true(all(test@iters[((i-1)*2+1):((i-1)*2+2),"value",] %in% c(value1[i], value2[i])))
    }
    expect_true(all(is.na(test@iters[,"min",])))
    expect_true(all(is.na(test@iters[,"max",])))

    # Two lists with relYear and min bounds
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1),
        list(year=years, relYear=relyears, quant=quant2, min=min_value2))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # relyears every other year
    expect_equal(test@target$relYear[seq(2,nyears*2,by=2)], relyears)
    expect_equal(test@target$relYear[seq(1,(nyears*2)-1,by=2)], rep(as.integer(NA),nyears))
    # Test value
    expect_equal(unname(test@iters[seq(1,(nyears*2)-1,by=2),"value",]), value1)
    expect_equal(unname(test@iters[seq(2,(nyears*2),by=2),"min",]), min_value2)
    expect_true(all(is.na(test@iters[,"max",])))

    # Two lists with relYear and max bounds
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1),
        list(year=years, relYear=relyears, quant=quant2, max=max_value2))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # relyears every other year
    expect_equal(test@target$relYear[seq(2,nyears*2,by=2)], relyears)
    expect_equal(test@target$relYear[seq(1,(nyears*2)-1,by=2)], rep(as.integer(NA),nyears))
    # Test value
    expect_equal(unname(test@iters[seq(1,(nyears*2)-1,by=2),"value",]), value1)
    expect_equal(unname(test@iters[seq(2,(nyears*2),by=2),"max",]), max_value2)
    expect_true(all(is.na(test@iters[,"min",])))

    # Two lists with relYear and min and max bounds - fails as above
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1),
        list(year=years, relYear=relyears, quant=quant2, min=min_value2, max=max_value2))
    expect_equal(nrow(test@target), 2*nyears)
    expect_equal(test@target$year, rep(years, each=2))
    # relyears every other year
    expect_equal(test@target$relYear[seq(2,nyears*2,by=2)], relyears)
    expect_equal(test@target$relYear[seq(1,(nyears*2)-1,by=2)], rep(as.integer(NA),nyears))
    # Test value
    expect_equal(unname(test@iters[seq(1,(nyears*2)-1,by=2),"value",]), value1)
    expect_equal(unname(test@iters[seq(2,(nyears*2),by=2),"max",]), max_value2)
    expect_equal(unname(test@iters[seq(2,(nyears*2),by=2),"min",]), min_value2)






    # Three lists Fails
    test <- fwdControl(
        list(year=years, quant=quant1, value=value1),
        list(year=years, quant=quant2, value=value2),
        list(year=years, quant=quant3, value=value3))
    test@target
    # Only two lists have been included - was expecting all three
    test@iters




})


test_that("list constructor - iters",{

    nyears <- round(runif(1,min=5,max=15))
    inityear <- round(runif(1,min=1990,max=2000)) 
    years <- inityear:(inityear+nyears-1)
    relinityear <- round(runif(1,min=1990,max=2000)) 
    relyears <- relinityear:(relinityear+nyears-1)
    value <- rnorm(nyears)
    min_value <- rnorm(nyears)
    max_value <- rnorm(nyears)
    quant <- sample(.qlevels,1)
    biol <- "Mars"

    # Iterations
               
    niters <- round(runif(1,min=5,max=15))
    nvalue <- rnorm(niters)
})
