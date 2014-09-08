/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../inst/include/FLStock.h"

// Default constructor
// Just an empty object
FLStock::FLStock(){
    name = std::string();
    desc = std::string(); 
    range = Rcpp::NumericVector();
    catches = FLQuant();
    catch_n = FLQuant();  
    catch_wt = FLQuant(); 
    discards = FLQuant();
    discards_n = FLQuant();
    discards_wt = FLQuant();
    landings = FLQuant();
    landings_n = FLQuant();
    landings_wt = FLQuant();
    stock = FLQuant();
    stock_n = FLQuant();
    stock_wt = FLQuant();
    m = FLQuant();
    mat = FLQuant();
    harvest = FLQuant();
    harvest_spwn = FLQuant();
    m_spwn = FLQuant();
}

// Constructor from a SEXP S4 FLStock
// Used as intrusive 'as'
FLStock::FLStock(SEXP fls_sexp){
    Rcpp::S4 fls_s4 = Rcpp::as<Rcpp::S4>(fls_sexp);
    name = Rcpp::as<std::string>(fls_s4.slot("name"));
    desc = Rcpp::as<std::string>(fls_s4.slot("desc"));
    range = fls_s4.slot("range");
    catches = fls_s4.slot("catch");
    catch_n = fls_s4.slot("catch.n");
    catch_wt = fls_s4.slot("catch.wt");
    discards = fls_s4.slot("discards");
    discards_n = fls_s4.slot("discards.n"); 
    discards_wt = fls_s4.slot("discards.wt"); 
    landings = fls_s4.slot("landings"); 
    landings_n = fls_s4.slot("landings.n"); 
    landings_wt = fls_s4.slot("landings.wt");
    stock = fls_s4.slot("stock");
    stock_n = fls_s4.slot("stock.n"); 
    stock_wt = fls_s4.slot("stock.wt"); 
    m = fls_s4.slot("m"); 
    mat = fls_s4.slot("mat"); 
    harvest = fls_s4.slot("harvest"); 
    harvest_spwn = fls_s4.slot("harvest.spwn"); 
    m_spwn = fls_s4.slot("m.spwn"); 
}

// Copy constructor - else members can be pointed at by multiple instances
FLStock::FLStock(const FLStock& FLStock_source){
    name = FLStock_source.name;
    desc = FLStock_source.desc;
    range = Rcpp::clone<Rcpp::NumericVector>(FLStock_source.range);
    catches = FLStock_source.catches;
    catch_n = FLStock_source.catch_n;
    catch_wt = FLStock_source.catch_wt;
    discards = FLStock_source.discards;
    discards_n = FLStock_source.discards_n;
    discards_wt = FLStock_source.discards_wt;
    landings = FLStock_source.landings;
    landings_n = FLStock_source.landings_n;
    landings_wt = FLStock_source.landings_wt;
    stock = FLStock_source.stock;
    stock_n = FLStock_source.stock_n;
    stock_wt = FLStock_source.stock_wt;
    m = FLStock_source.m;
    mat = FLStock_source.mat;
    harvest = FLStock_source.harvest;
    harvest_spwn = FLStock_source.harvest_spwn;
    m_spwn = FLStock_source.m_spwn;
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
FLStock& FLStock::operator = (const FLStock& FLStock_source){
	if (this != &FLStock_source){
        name = FLStock_source.name;
        desc = FLStock_source.desc;
        range = Rcpp::clone<Rcpp::NumericVector>(FLStock_source.range);
        catches = FLStock_source.catches;
        catch_n = FLStock_source.catch_n;
        catch_wt = FLStock_source.catch_wt;
        discards = FLStock_source.discards;
        discards_n = FLStock_source.discards_n;
        discards_wt = FLStock_source.discards_wt;
        landings = FLStock_source.landings;
        landings_n = FLStock_source.landings_n;
        landings_wt = FLStock_source.landings_wt;
        stock = FLStock_source.stock;
        stock_n = FLStock_source.stock_n;
        stock_wt = FLStock_source.stock_wt;
        m = FLStock_source.m;
        mat = FLStock_source.mat;
        harvest = FLStock_source.harvest;
        harvest_spwn = FLStock_source.harvest_spwn;
        m_spwn = FLStock_source.m_spwn;
	}
	return *this;
}

/* Intrusive 'wrap' */
FLStock::operator SEXP() const{
    //Rprintf("Wrapping FLStock.\n");
        Rcpp::S4 fls_s4("FLStock");
        fls_s4.slot("name") = name;
        fls_s4.slot("desc") = desc;
        fls_s4.slot("range") = range;
        fls_s4.slot("catch") = catches;
        fls_s4.slot("catch.n") = catch_n;
        fls_s4.slot("catch.wt") = catch_wt;
        fls_s4.slot("discards") = discards;
        fls_s4.slot("discards.n") = discards_n;
        fls_s4.slot("discards.wt") = discards_wt;
        fls_s4.slot("landings") = landings;
        fls_s4.slot("landings.n") = landings_n;
        fls_s4.slot("landings.wt") = landings_wt;
        fls_s4.slot("stock") = stock;
        fls_s4.slot("stock.n") = stock_n;
        fls_s4.slot("stock.wt") = stock_wt;
        fls_s4.slot("m") = m;
        fls_s4.slot("mat") = mat;
        fls_s4.slot("harvest") = harvest;
        fls_s4.slot("harvest.spwn") = harvest_spwn;
        fls_s4.slot("m.spwn") = m_spwn;
        return Rcpp::wrap(fls_s4);
}

