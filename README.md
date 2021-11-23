# FLasher <img src="man/figures/flasher.png" align="right" width="120" />

[![R-CMD-check](https://github.com/flr/FLasher/workflows/R-CMD-check/badge.svg)](https://github.com/flr/FLasher/actions)
[![License](https://eddelbuettel.github.io/badges/GPL2+.svg)](https://www.gnu.org/licenses/gpl-2.0.html)
[![Codecov test coverage](https://codecov.io/gh/flr/FLasher/branch/master/graph/badge.svg)](https://codecov.io/gh/flr/FLasher?branch=master)

## Overview

Projection of future population and fishery dynamics is carried out for a given set of management targets. A system of equations is solved, using Automatic Differentation (AD), for the levels of effort by fishery/fleet that will result in the required abundances, catches or fishing mortalities.

## Installation
To install this package, start R and enter:

```
install.packages("FLasher", repos="http://flr-project.org/R")
```

or directly from the github repository by using:

```
remotes::install_github("flr/FLasher")
```

**WARNING**: FLasher requires a 64 bit installation of R. Installation from source in R for Windows should be carried out using `--no-multiarch` for a 64 bit-only installation if both 32 and 64 bit R are available.

## Documentation
- [Forecasting on the Medium Term for advice using FLasher](https://flr-project.org/doc/Forecasting_on_the_Medium_Term_for_advice_using_FLasher.html)
- [Help pages](http://flr-project.org/FLasher)

## License
Copyright (c) 2016-21 European Union. Released under the [EUPL v1.2](https://eupl.eu/1.2/en/).

## Contact
You are welcome to:

- Submit suggestions and bug-reports at: <https://github.com/flr/FLasher/issues>
- Send a pull request on: <https://github.com/flr/FLasher/>
- Author: Finlay Scott and Iago Mosqueira (EC-JRC).
- Maintainer: Iago Mosqueira <iago.mosqueira@wur.nl>
