# FLasher
- Version: 0.6.0.9003
- Date: 2020-04-27
- Author: Finlay Scott and Iago Mosqueira.
- Maintainer: Iago Mosqueira <iago.mosqueira@wur.nl>
- Repository: <https://github.com/flr/FLasher/>
- Bug reports: <https://github.com/flr/FLasher/issues>

## Overview

Projection of future population and fishery dynamics is carried out for a given set of management targets. A system of equations is solved, using Automatic Differentation (AD), for the levels of effort by fishery/fleet that will result in the required abundances, catches or fishing mortalities.

To install this package, start R and enter:

	install.packages("FLasher", repos="http://flr-project.org/R")

To install the development version, you can use:

  devtools::install_github("flr/FLasher", INSTALL_opts=c("--no-multiarch"))

**WARNING**: FLasher requires a 64 bit installation of R. Installation from source in R for Windows should be carried out using `--no-multiarch` for a 64 bit-only installation.

## Documentation
- [Help pages](http://flr-project.org/FLasher)

## Bibliography

## Build Status
[![Travis Build Status](https://travis-ci.org/flr/FLasher.svg?branch=master)](https://travis-ci.org/flr/FLasher)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/flr/FLasher?branch=master&svg=true)](https://ci.appveyor.com/project/flr/FLasher)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FLasher)](https://cran.r-project.org/package=FLasher)

## License
Copyright (c) 2016-19 European Union. Released under the [EUPL v1.2](https://eupl.eu/1.2/en/).

## Contact
You are welcome to:

- Submit suggestions and bug-reports at: <https://github.com/flr/FLasher/issues>
- Send a pull request on: <https://github.com/flr/FLasher/>
- Compose a friendly e-mail to: <iago.mosqueira@wur.nl>
