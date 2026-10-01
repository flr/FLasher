# Appendix: Methodology of the `fwd()` Function in FLasher

## 1. Introduction

The `fwd()` function in the FLasher package provides a flexible framework for projecting fish stock dynamics forward in time under user-specified management scenarios. This appendix provides a comprehensive technical description of the methodology underlying `fwd()`, including the mathematical formulations, computational algorithms, and implementation details.

FLasher supports two distinct projection modes:

1. **Single-species, single-fishery projections** using `FLStock` objects
2. **Multi-species, multi-fishery projections** using `FLBiol` and `FLFisheries` objects

Both modes share common population dynamics equations but differ in their representation of fishing mortality and catch calculations. The implementation leverages templated C++ code with automatic differentiation (via the CppAD library) to efficiently solve for fishing efforts that achieve user-specified targets through Newton-Raphson optimization.

## 2. General Framework and Notation

### 2.1 Dimensions

FLasher operates on multi-dimensional arrays with the following dimensions:

- **age** ($a$): Age classes, indexed from minimum age to maximum age, with optional plus-group
- **year** ($t$): Time in years
- **unit** ($u$): Stock units (e.g., different sub-stocks or areas)
- **season** ($s$): Seasonal time steps within a year
- **area** ($r$): Spatial areas
- **iter** ($i$): Iterations for uncertainty propagation

### 2.2 Key Variables

| Symbol | Description |
|--------|-------------|
| $N_{a,t,s}$ | Abundance at age $a$, year $t$, season $s$ |
| $F_{a,t,s}$ | Fishing mortality rate at age $a$, year $t$, season $s$ |
| $M_{a,t,s}$ | Natural mortality rate at age $a$, year $t$, season $s$ |
| $Z_{a,t,s}$ | Total mortality rate: $Z = F + M$ |
| $W_{a,t,s}$ | Mean weight-at-age |
| $Mat_{a,t,s}$ | Maturity-at-age (proportion mature) |
| $Fec_{a,t,s}$ | Fecundity-at-age |
| $\sigma_{a,t,s}$ | Spawning time (proportion of year/season before spawning) |
| $SSB_t$ | Spawning stock biomass in year $t$ |
| $R_t$ | Recruitment in year $t$ |

## 3. Core Population Dynamics

### 3.1 The Baranov Catch Equation

The fundamental equation governing population dynamics in FLasher is the **Baranov equation**, which describes the survival of a cohort over a time period:

$$
N_{a+1,t+1,s} = N_{a,t,s} \exp(-Z_{a,t,s})
$$

where the total instantaneous mortality rate is:

$$
Z_{a,t,s} = F_{a,t,s} + M_{a,t,s}
$$

The number of fish surviving from age $a$ at the start of a time step to the end is:

$$
S_{a,t,s} = N_{a,t,s} \exp(-Z_{a,t,s})
$$

### 3.2 Age Progression

At the transition between time steps, survivors from age $a$ become fish of age $a+1$:

$$
N_{a+1,t+1,1} = S_{a,t,s_{max}}
$$

where $s_{max}$ is the final season of year $t$.

For a plus-group at age $a_{max}$, survivors accumulate:

$$
N_{a_{max},t+1,1} = S_{a_{max}-1,t,s_{max}} + S_{a_{max},t,s_{max}}
$$

### 3.3 Catch Calculation

The **Baranov catch equation** calculates the number of fish caught:

$$
C_{a,t,s} = \frac{F_{a,t,s}}{Z_{a,t,s}} N_{a,t,s} \left(1 - \exp(-Z_{a,t,s})\right)
$$

This represents the portion of total mortality attributable to fishing. In biomass terms:

$$
Y_{a,t,s} = C_{a,t,s} \times W_{a,t,s}
$$

where $Y$ is catch weight and $W$ is mean weight-at-age.

### 3.4 Spawning Stock Biomass

Spawning stock biomass is calculated as the total biomass of mature fish at the time of spawning. The calculation depends on when spawning occurs relative to fishing and natural mortality within the season.

**SSB at start of season:**

$$
SSB^{start}_{t,s} = \sum_{a} N_{a,t,s} \times W_{a,t,s} \times Mat_{a,t,s}
$$

**SSB at spawning time** (accounting for mortality before spawning):

$$
SSB^{spawn}_{t,s} = \sum_{a} N_{a,t,s} \times \exp(-Z_{a,t,s} \times \sigma_{a,t,s}) \times W_{a,t,s} \times Mat_{a,t,s}
$$

where $\sigma_{a,t,s}$ is the proportion of the time step that has elapsed before spawning.

**SSB at end of season:**

$$
SSB^{end}_{t,s} = \sum_{a} S_{a,t,s} \times W_{a,t,s} \times Mat_{a,t,s}
$$

### 3.5 Spawning Reproductive Potential

For stock-recruitment relationships that use fecundity rather than SSB, the spawning reproductive potential (SRP) is calculated as:

$$
SRP_{t,s} = \sum_{a} N_{a,t,s} \times \exp(-Z_{a,t,s} \times \sigma_{a,t,s}) \times Fec_{a,t,s}
$$

## 4. Stock-Recruitment Relationships

Recruitment is predicted using parametric stock-recruitment relationships (SRRs) with optional deviations. FLasher implements multiple SRR models:

### 4.1 Beverton-Holt

$$
R_t = \frac{\alpha \times SSB_t}{\beta + SSB_t} \times \exp(\delta_t)
$$

where:
- $\alpha$ is the maximum recruitment (density-independent parameter)
- $\beta$ is the SSB at which recruitment is half of maximum
- $\delta_t$ is the recruitment deviation (log-scale)

### 4.2 Ricker

$$
R_t = \alpha \times SSB_t \times \exp(-\beta \times SSB_t) \times \exp(\delta_t)
$$

where:
- $\alpha$ is the density-independent productivity parameter
- $\beta$ is the density-dependent parameter

### 4.3 Segmented Regression (Hockey Stick)

$$
R_t = \begin{cases}
\alpha \times SSB_t \times \exp(\delta_t) & \text{if } SSB_t < \beta \\
\alpha \times \beta \times \exp(\delta_t) & \text{if } SSB_t \geq \beta
\end{cases}
$$

### 4.4 Other Models

FLasher also implements:
- **Constant/Mean recruitment**: $R_t = \alpha \times \exp(\delta_t)$
- **Cushing**: $R_t = \alpha \times SSB_t^{\beta} \times \exp(\delta_t)$
- **BevholtSS3**: Beverton-Holt parameterization used in Stock Synthesis
- **Bevholtsig**: Sigmoid Beverton-Holt with additional steepness parameter
- **SegregDa**: Deriso-Schnute formulation of segmented regression
- **SurvSRR**: Survival-based stock-recruitment
- **MixedSRR**: Weighted mixture of multiple SRR models

### 4.5 Recruitment Deviations

Deviations can be specified as either **multiplicative** (default) or **additive**:

**Multiplicative** (log-normal):
$$
R_t = f(SSB_t; \alpha, \beta) \times \exp(\delta_t)
$$

**Additive** (normal):
$$
R_t = f(SSB_t; \alpha, \beta) + \delta_t
$$

where $f(SSB_t; \alpha, \beta)$ is the deterministic SRR function.

### 4.6 Recruitment Timing

Recruitment enters the population at age 0 (or the minimum age). The timing within the year/season is controlled by the spawning time parameter ($\sigma$). For age 0, natural mortality before recruitment settlement is:

$$
M_{0,t,s}^{effective} = M_{0,t,s} \times (1 - \sigma_{0,t,s})
$$

This adjustment accounts for the fact that recruits only experience mortality after they have settled into the population.

## 5. FLStock Projections (Single-Species, Single-Fishery)

### 5.1 Fishing Mortality Structure

In the `FLStock` framework, fishing mortality is represented directly as age-specific rates $F_{a,t,s}$. Users can specify targets in terms of:

- **Fbar**: Mean fishing mortality over a specified age range

$$
\bar{F}_{t,s} = \frac{1}{a_{max}^{F} - a_{min}^{F} + 1} \sum_{a=a_{min}^{F}}^{a_{max}^{F}} F_{a,t,s}
$$

- **Catch**: Total catch in weight
- **SSB**: Spawning stock biomass at specified timing
- **Biomass**: Total biomass at specified timing

### 5.2 Projection Algorithm

For `FLStock` objects, the projection proceeds as follows:

1. **Initialize**: Set starting abundances $N_{a,t_0,1}$ from the stock object
2. **For each year** $t$ **and season** $s$:
   a. **Apply fishing mortality**: Use current $F_{a,t,s}$ values
   b. **Calculate survivors**: $S_{a,t,s} = N_{a,t,s} \exp(-Z_{a,t,s})$
   c. **Calculate catches**: Using Baranov equation
   d. **Calculate SSB**: At appropriate timing (start/spawn/end)
   e. **Check targets**: Compare achieved values to target values
   f. **Solve for effort**: If targets not met, adjust $F$ using Newton-Raphson (see Section 7)
   g. **Age progression**: Move survivors to next age/time step
   h. **Predict recruitment**: Use SRR to get $R_{t+1}$
   i. **Insert recruits**: Set $N_{0,t+1,1} = R_{t+1}$

3. **Return**: Updated stock object with projected abundances, catches, and SSB

### 5.3 Fishing Mortality Scaling

When solving for effort to hit targets, fishing mortality is scaled multiplicatively:

$$
F_{a,t,s}^{new} = E_{t,s} \times F_{a,t,s}^{base}
$$

where $E_{t,s}$ is the effort multiplier and $F_{a,t,s}^{base}$ is a baseline fishing mortality pattern.

### 5.4 Conversion from FLStock to FLFishery Framework

When converting an `FLStock` object to the `FLFishery` framework (using the `as()` method from the FLFishery package), fishing mortality must be decomposed into effort and selectivity components. The conversion process calculates effort from the observed fishing mortality pattern as follows:

**Step 1: Extract selectivity pattern**

The selectivity pattern is taken from the catch selectivity of the stock:

$$
Sel_{a,t,s} = \frac{C_{a,t,s}}{\max_a(C_{a,t,s})}
$$

where catch selectivity is typically pre-calculated and stored in the `FLStock` object.

**Step 2: Calculate effort from fishing mortality**

Effort is derived by inverting the fishing mortality equation. For the standard case ($\alpha = 1$, $\beta = 0$):

$$
E_{t,s} = \frac{F_{a,t,s}}{Sel_{a,t,s}}
$$

To avoid issues with zero selectivity or fishing mortality, the effort is calculated as the **mean ratio across the Fbar age range**:

$$
E_{t,s} = \frac{1}{a_{max}^{F} - a_{min}^{F} + 1} \sum_{a=a_{min}^{F}}^{a_{max}^{F}} \frac{F_{a,t,s}}{Sel_{a,t,s}}
$$

This ensures that the effort value represents the intensity of fishing mortality over the fully-selected ages.

**Step 3: Handle edge cases**

- If $Sel_{a,t,s} = 0$ or $F_{a,t,s} = 0$, those ages are excluded from the mean calculation
- If the calculated effort is NA or zero, it is set to a small value (e.g., $10^{-12}$) to avoid numerical issues
- The resulting effort is stored with empty units to indicate it is dimensionless

This conversion preserves the overall fishing mortality pattern while enabling the multi-fishery framework to handle multiple fleets operating on the same stock with different selectivities and efforts.

## 6. FLBiol and FLFisheries Projections (Multi-Species, Multi-Fishery)

### 6.1 Architecture

The multi-species, multi-fishery framework separates biological populations (`FLBiol` objects) from fishing activities (`FLFishery` and `FLCatch` objects):

- **FLBiol**: Represents a biological population with abundance, weights, maturity, natural mortality
- **FLFishery**: Represents a fishing fleet
- **FLCatch**: Represents the catch of a specific stock by a specific fishery, with selectivity and catchability

### 6.2 Fishing Mortality Composition

In this framework, fishing mortality on a biological population is the sum of fishing mortalities from all fisheries catching that stock:

$$
F_{b,a,t,s} = \sum_{f} F_{f,b,a,t,s}
$$

where:
- $b$ indexes biological populations (biols)
- $f$ indexes fisheries
- $F_{f,b,a,t,s}$ is the fishing mortality from fishery $f$ on biol $b$

The fishing mortality from a specific fishery-catch combination follows a **generalized catchability model**:

$$
F_{f,b,a,t,s} = E_{f,t,s} \times \alpha_{f,b} \times Sel_{f,b,a,t,s} \times B_{b,a,t,s}^{-\beta_{f,b}}
$$

where:
- $E_{f,t,s}$ is the effort of fishery $f$ in year $t$, season $s$
- $\alpha_{f,b}$ is the catchability scaling parameter (dimensionless, typically = 1)
- $Sel_{f,b,a,t,s}$ is the selectivity (age-specific vulnerability, range 0-1)
- $B_{b,a,t,s} = N_{b,a,t,s} \times W_{b,a,t,s}$ is the biomass at age
- $\beta_{f,b}$ is the biomass-dependency exponent (typically = 0 for density-independent catchability)

This formulation allows for **density-dependent catchability**: when $\beta > 0$, fishing mortality decreases with biomass (hyperdepletion); when $\beta < 0$, fishing mortality increases with biomass (hyperstability). The standard case of density-independent catchability has $\beta = 0$, reducing to:

$$
F_{f,b,a,t,s} = E_{f,t,s} \times \alpha_{f,b} \times Sel_{f,b,a,t,s}
$$

The catchability parameters $(\alpha, \beta)$ are stored in the `catch.q` slot of each `FLCatch` object as an `FLPar` with named parameters.

### 6.3 Effort and Capacity

Fishing effort can be bounded by fleet capacity:

$$
E_{f,t,s}^{actual} = \min(E_{f,t,s}, Capacity_{f,t,s})
$$

The effort multiplier solved by the Newton-Raphson algorithm is applied to a baseline effort:

$$
E_{f,t,s} = \mu_{f,t,s} \times E_{f,t,s}^{base}
$$

### 6.4 Catch Calculations by Fishery

The catch in numbers from fishery $f$ of biol $b$ is calculated using a partial fishing mortality:

$$
C_{f,b,a,t,s} = \frac{F_{f,b,a,t,s}}{Z_{b,a,t,s}} N_{b,a,t,s} \left(1 - \exp(-Z_{b,a,t,s})\right)
$$

Note that total mortality $Z$ includes contributions from all fisheries:

$$
Z_{b,a,t,s} = M_{b,a,t,s} + \sum_{f} F_{f,b,a,t,s}
$$

### 6.5 FCB Matrix

The **Fishery-Catch-Biol (FCB) matrix** defines which fisheries catch which biological populations. This is a key data structure that maps the relationships:

$$
FCB = \begin{bmatrix}
f_1 & c_1 & b_1 \\
f_2 & c_2 & b_2 \\
\vdots & \vdots & \vdots \\
\end{bmatrix}
$$

where each row specifies that fishery $f_i$ has a catch component $c_i$ that catches biol $b_i$.

### 6.6 Multi-Species Projection Algorithm

1. **Initialize**: Set starting abundances for all biols
2. **For each year** $t$ **and season** $s$:
   a. **Calculate composite F**: For each biol, sum F from all fisheries using FCB matrix
   b. **Calculate total Z**: $Z = M + F$ for each biol
   c. **Calculate survivors**: $S_{b,a,t,s} = N_{b,a,t,s} \exp(-Z_{b,a,t,s})$ for each biol
   d. **Calculate catches**: For each fishery-catch-biol combination
   e. **Calculate target quantities**: SSB, biomass, revenue, etc.
   f. **Solve for efforts**: Adjust effort multipliers to hit targets (see Section 7)
   g. **Age progression**: For each biol
   h. **Predict recruitment**: For each biol using its SRR
   i. **Insert recruits**: Into each biol

3. **Return**: Updated biols and fisheries with projected dynamics

### 6.7 Revenue Targets

In multi-fishery models, economic targets can be specified. Revenue from fishery $f$ is:

$$
Revenue_{f,t,s} = \sum_{b,a} C_{f,b,a,t,s} \times W_{b,a,t,s} \times Price_{f,b,t,s}
$$

where $Price_{f,b,t,s}$ is the price per unit weight for biol $b$ caught by fishery $f$.

## 7. Target-Based Projections and the Newton-Raphson Solver

### 7.1 Control Object

Projections are driven by a `fwdControl` object that specifies targets for each time step. A target is defined by:

- **Type**: effort, fbar, catch, landings, SSB, biomass, revenue, etc.
- **Value**: The desired value to achieve
- **Year, season, area, unit, iteration**: Temporal and spatial specification
- **Fishery, catch, biol**: For multi-fishery targets (via FCB)
- **Min/Max age**: For fbar targets
- **Relative**: Whether target is relative to a previous time period

### 7.2 The Optimization Problem

For each time step with targets, we need to solve for effort multipliers $\mathbf{E} = (E_1, E_2, \ldots, E_n)$ such that:

$$
g_j(\mathbf{E}) - T_j = 0, \quad j = 1, \ldots, m
$$

where:
- $g_j(\mathbf{E})$ is the achieved value of target $j$ as a function of efforts
- $T_j$ is the target value for target $j$
- $n$ is the number of effort multipliers (fisheries or time steps)
- $m$ is the number of targets

This is a system of $m$ nonlinear equations in $n$ unknowns (typically $m = n$).

### 7.3 Newton-Raphson Algorithm

The Newton-Raphson method iteratively refines the effort vector:

$$
\mathbf{E}^{(k+1)} = \mathbf{E}^{(k)} - \mathbf{J}^{-1}(\mathbf{E}^{(k)}) \times \mathbf{f}(\mathbf{E}^{(k)})
$$

where:
- $\mathbf{E}^{(k)}$ is the effort vector at iteration $k$
- $\mathbf{f}(\mathbf{E}) = [g_1(\mathbf{E}) - T_1, \ldots, g_m(\mathbf{E}) - T_m]^T$ is the residual vector
- $\mathbf{J}(\mathbf{E})$ is the Jacobian matrix:

$$
J_{ij} = \frac{\partial g_i}{\partial E_j}
$$

### 7.4 Automatic Differentiation

FLasher uses the **CppAD library** to compute derivatives automatically. The projection code is templated to work with both regular doubles and `CppAD::AD<double>` types. When running with AD types:

1. **Tape the operations**: All mathematical operations are recorded
2. **Compute function values**: $\mathbf{f}(\mathbf{E})$
3. **Compute Jacobian**: $\mathbf{J} = \frac{\partial \mathbf{f}}{\partial \mathbf{E}}$ using automatic differentiation
4. **Solve linear system**: $\mathbf{J} \Delta\mathbf{E} = -\mathbf{f}$ using LU decomposition
5. **Update efforts**: $\mathbf{E}^{new} = \mathbf{E}^{old} + \Delta\mathbf{E}$

This approach avoids the need for manual derivative calculations and ensures numerical accuracy.

### 7.5 Handling Multiple Iterations

For stochastic projections with multiple iterations, the solver handles each iteration independently. The Jacobian matrix has a block-diagonal structure:

$$
\mathbf{J} = \begin{bmatrix}
\mathbf{J}_1 & 0 & \cdots & 0 \\
0 & \mathbf{J}_2 & \cdots & 0 \\
\vdots & \vdots & \ddots & \vdots \\
0 & 0 & \cdots & \mathbf{J}_n
\end{bmatrix}
$$

where $\mathbf{J}_i$ is the Jacobian for iteration $i$. This structure is exploited for computational efficiency.

### 7.6 Convergence Criteria

The Newton-Raphson iteration continues until:

$$
\|\mathbf{f}(\mathbf{E}^{(k)})\| < \epsilon
$$

where $\epsilon$ is a tolerance (default: $1.5 \times 10^{-8}$) and $\|\cdot\|$ is the Euclidean norm. The maximum number of iterations is capped (default: 50).

### 7.7 Bounds on Effort

Effort multipliers can be constrained to remain within bounds:

$$
E_{min} \leq E_j \leq E_{max}
$$

During the Newton-Raphson iteration, if an updated effort would violate bounds, it is clipped:

$$
E_j^{new} = \max(E_{min}, \min(E_{max}, E_j^{proposed}))
$$

### 7.8 Numerical Stability

Several measures ensure numerical stability:

1. **Zero effort handling**: Efforts below machine epsilon ($\approx 2.2 \times 10^{-16}$) are set to $\sqrt{\epsilon}$ to avoid log(0) errors
2. **Singular Jacobian detection**: If the Jacobian is singular (determinant ≈ 0), the solver terminates with a warning
3. **Sparse Jacobian**: For large problems, CppAD computes sparse Jacobian matrices to reduce memory and computation
4. **LU decomposition**: The linear system $\mathbf{J} \Delta\mathbf{E} = -\mathbf{f}$ is solved using LU decomposition, which is numerically stable for well-conditioned systems

## 8. Implementation Details

### 8.1 C++ Class Structure

FLasher's core algorithms are implemented in templated C++ classes:

- **`fwdBiol_base<T>`**: Represents a biological population with abundance, weights, etc. Template parameter `T` can be `double` or `CppAD::AD<double>`
- **`fwdSR_base<T>`**: Encapsulates stock-recruitment relationship
- **`operatingModel<T>`**: Orchestrates multi-species projections
- **`FLQuant_base<T>`**: Multi-dimensional array with age × year × unit × season × area × iter structure
- **`solver`**: Newton-Raphson solver using CppAD

### 8.2 R to C++ Interface

The R function `fwd()` validates inputs, prepares data structures, and calls C++ functions via Rcpp:

1. **Input validation**: Check dimensions, NA values, ensure consistency
2. **Deviance handling**: Extend deviances to match biol dimensions, replace NAs with 1.0
3. **Control preparation**: Convert year/season/area/unit names to integer indices
4. **Effort initialization**: Set initial effort values, apply min/max constraints
5. **Call C++**: Invoke `operatingModelRun()` or equivalent C++ function
6. **Extract results**: Convert C++ objects back to R FLR objects

### 8.3 Index Conversion

FLasher uses **zero-based indexing** internally (C++ convention) but presents **one-based indexing** to R users. Years in the control object are converted to position indices:

$$
\text{position} = \text{year} - \text{min\_year} + 1
$$

Similarly, season, area, and unit dimensions are converted from named factors to integer positions.

### 8.4 Performance Optimizations

- **Templated code**: Single codebase works for both regular computation and automatic differentiation
- **Sparse Jacobians**: For problems with many targets, only non-zero Jacobian elements are computed
- **Parallel iterations**: Each stochastic iteration is solved independently, enabling parallelization (future enhancement)
- **In-place operations**: Arrays are updated in-place where possible to reduce memory allocation

## 9. Special Cases and Extensions

### 9.1 Seasonal Models

For seasonal models with $n_s > 1$ seasons per year:

- Fishing mortality and natural mortality can vary by season
- SSB can be calculated at different timings within each season
- Recruitment can occur in any season (controlled by `spwn` timing parameter)
- Age progression occurs at year boundaries, not season boundaries

The within-year age structure remains constant; only abundance changes within the year due to mortality.

### 9.2 Multiple Units and Areas

FLasher supports multiple stock units and spatial areas:

- Each unit can have different biological parameters, selectivities, and fishing mortalities
- Areas can represent distinct spatial zones with separate fishing activities
- The FCB matrix can map fisheries to specific unit-area combinations of biols

### 9.3 Minimum and Maximum Bounds on Targets

Targets can specify minimum and maximum values rather than exact values:

- **Minimum target**: $g_j(\mathbf{E}) \geq T_j^{min}$
- **Maximum target**: $g_j(\mathbf{E}) \leq T_j^{max}$
- **Range target**: $T_j^{min} \leq g_j(\mathbf{E}) \leq T_j^{max}$

The solver treats these as inequality constraints, using penalty methods or barrier functions to incorporate them into the Newton-Raphson framework.

### 9.4 Relative Targets

Targets can be specified relative to a previous time step:

$$
T_j = \text{relValue} \times g_j(\mathbf{E}_{t-1})
$$

For example, "catch in year $t$ should be 90% of catch in year $t-1$". This is useful for implementing gradual reduction strategies.

### 9.5 Mixed Target Types

A single projection can include multiple target types simultaneously, for example:

- Year 1: Fix effort at 1.0
- Year 2: Achieve $\bar{F} = 0.3$
- Year 3: Achieve catch = 10,000 tons
- Year 4: Achieve SSB = 50,000 tons

The solver handles each time step's targets sequentially, using the population state from the previous step.

## 10. Validation and Testing

### 10.1 Verification Against Analytical Solutions

For simple cases (single age class, no recruitment), the Baranov equation has analytical solutions:

$$
N_t = N_0 \exp(-Z \times t)
$$

FLasher results are verified against these analytical solutions to ensure correctness of the core dynamics.

### 10.2 Comparison with Other Models

FLasher projections have been compared with:

- **FLash**: The previous FLR projection package
- **SS3**: Stock Synthesis projections
- **Age-structured simulation models**: Custom R implementations

### 10.3 Numerical Precision

The Newton-Raphson solver typically achieves convergence to within the specified tolerance ($1.5 \times 10^{-8}$) in fewer than 10 iterations for well-posed problems. Ill-conditioned problems (e.g., trying to achieve impossible targets) may fail to converge.

## 11. Limitations and Assumptions

### 11.1 Key Assumptions

1. **Closed population**: No immigration or emigration (except recruitment)
2. **Constant parameters within time steps**: M, F, weights, etc. are constant within each season
3. **Separability**: F is separable into age-specific selectivity and overall effort
4. **Instantaneous processes**: Mortality and growth occur continuously (exponential decay)
5. **Deterministic dynamics given parameters**: Uncertainty only through recruitment deviations and parameter iterations

### 11.2 Known Limitations

1. **No length structure**: FLasher is age-based; length-based dynamics require different approaches
2. **No movement**: Spatial areas are independent; no migration between areas
3. **No predation interactions**: Multi-species framework allows multiple biols but no predator-prey interactions
4. **Linear solver**: Newton-Raphson may not converge for highly nonlinear target relationships
5. **Parameter uncertainty**: Only propagated through iterations; no within-iteration variability

### 11.3 Computational Constraints

- Large problems (many age classes × many fisheries × many iterations × many time steps) can be memory-intensive
- Automatic differentiation adds computational overhead (typically 2-5× slower than fixed F projections)
- Sparse Jacobian computation helps but very large systems may still be slow

## 12. Conclusion

The `fwd()` function in FLasher provides a robust, flexible, and computationally efficient framework for projecting fish stock dynamics under a wide range of management scenarios. The combination of classical age-structured population dynamics (Baranov equation), parametric stock-recruitment relationships, and modern numerical methods (automatic differentiation, Newton-Raphson optimization) enables users to:

- Project single or multiple stocks forward in time
- Incorporate multiple fisheries with different selectivities and catchabilities
- Specify complex management targets (effort, F, catch, SSB, biomass, revenue)
- Propagate uncertainty through multiple iterations
- Handle seasonal, multi-unit, and multi-area population structures

The implementation in templated C++ ensures computational efficiency while maintaining the flexibility needed for Management Strategy Evaluation and other applications in fisheries science.

## References

**Key Algorithms:**
- Baranov, F.I. (1918). On the question of the biological basis of fisheries. Nauchn. Issled. Ikhtiologicheskii Inst. Izv. 1, 81-128.
- Beverton, R.J.H. and Holt, S.J. (1957). On the dynamics of exploited fish populations. Fish. Invest. Lond., Ser. 2, 19, 533.
- Ricker, W.E. (1954). Stock and recruitment. Journal of the Fisheries Research Board of Canada, 11(5), 559-623.

**Numerical Methods:**
- Griewank, A. and Walther, A. (2008). Evaluating Derivatives: Principles and Techniques of Algorithmic Differentiation. SIAM.
- Press, W.H., Teukolsky, S.A., Vetterling, W.T., and Flannery, B.P. (2007). Numerical Recipes: The Art of Scientific Computing (3rd ed.). Cambridge University Press.

**FLR Framework:**
- Kell, L.T., Mosqueira, I., Grosjean, P., Fromentin, J-M., Garcia, D., Hillary, R., Jardim, E., Mardle, S., Pastoors, M.A., Poos, J.J., Scott, F., and Scott, R.D. (2007). FLR: an open-source framework for the evaluation and development of management strategies. ICES Journal of Marine Science, 64(4), 640-646.

**CppAD Library:**
- Bell, B.M. (2023). CppAD: A Package for Differentiation of C++ Algorithms. https://coin-or.github.io/CppAD/

---

*This document describes the methodology implemented in FLasher version 0.7.0 and later. For the most up-to-date implementation details, consult the package source code at https://github.com/flr/FLasher.*
