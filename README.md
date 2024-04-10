`riptw` is an R package that easily allows Inverse Probability of Treatment Weighting (IPTW) analysis

# Inverse Probability of Treatment Weighting (IPTW)
IPTW is a statistical method used to estimate ***causal effects of a specific variable*** where randomized controlled trials are impractical or unethical. IPTW was originally developed for better estimation of actual treatment effect in cancer studies, thus the variable of interest is normally indicated as ***"treatment"*** variable.  Today, IPTW has gained widespread adoption, extending its utility beyond oncology to various fields within medical research and other scientific disciplines.

Briefly, IPTW creates a pseudo-population by balancing covariates across a specific treatment variable groups, thus nullifying potential confounding bias. This technique involves assigning weights to each subject based on the inverse probability of receiving their actual treatment, conditional on covariates. The resulting weights are then applied in further analyses, such as regression models or survival analysis, to estimate actual treatment effects, free of covariates influence.

Key Advantages of IPTW:
- *Minimizes Confounding*: Enhances causal inference in observational data by balancing covariates across groups.
- *Versatile*: Applicable across various settings and compatible with different statistical analyses.
- *Mitigates Selection Bias*: Particularly useful for non-random treatment assignments.
- *Improves Comparability*: Strengthens the validity of treatment-control comparisons.

Now let's have a look at our package!

# Install riptw
Installing `riptw` is as simple as:
```
if (!require("devtools")) install.packages("devtools")
if (!require("BiocManager")) install.packages("BiocManager")

remotes::install_github( "cccnrc/riptw", repos = BiocManager::repositories() )
```
If you know a bit of R code (no worries, you don't really need to) you noticed that it only requires [devtools](https://devtools.r-lib.org/) and [BiocManager](https://cran.r-project.org/web/packages/BiocManager/vignettes/BiocManager.html).

The rest of dependencies you need will be installed directly with `riptw`, that's why installation will probably take a while (but you need to perform it only once :wink:)

# Run riptw
Let's try how simple is to use `riptw` on the R built-in dataset [quakes](https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/quakes) which comes along R by default
```
library(riptw)

### import the built-in dataset quakes
quakes <- datasets::quakes

### create a fake categorical variable "treatment" to normalize covariates on
quakes$treatment <- sample( c( rep( 0, nrow(quakes)/8*6 ), rep( 1, nrow(quakes)/8*2 ) ))

IPTW <- riptw( data = quakes,
                outcome = 'treatment',
                covariates = c( 'lat', 'long', 'depth', 'mag', 'stations' ) )
```
_That's all!_ You now have your `IPTW` adjusted covariates in the IPTW object.

`IPTW` (in the example above) is the object returned by `riptw()` function, and it is composed of 4 parts, that you can access through `IPTW$data` or `IPTW$plot` etc:
- *data*: the input database with some new columns:
    - Propensity Score for each treatment variable group (each column starts with `PS_` and is followed by the group name)
    - Inverse Probability of Treatment Weighting (`iptw`) for each sample based on its treatment group
    - Standardised Weights (`sw`): this is <u>what you are really interested in</u>, your dataset [weights](https://www.statology.org/weighted-least-squares-in-r/) to be used in regressions or other analysis you want to operate
- *unadjusted*: covariates stratification table before IPTW-adjustment
- *adjusted*: covariates stratification table after IPTW-adjustment
- *plot*: this is a graphical representation of how normalized IPTW is able to reduce each covariate effect on the treatment variable. Standardised Mean Difference (SMD) for each variable stratification with treatment groups are used for comparison, ***adjusted*** SMD are represented as a continuous line and ***unadjusted*** SMD as a dotted line. The red vertical bar at 0.1 represent the cutoff which is normally used to determine if the variable significantly stratificates with the group, thus is a covariate significantly influencing treatment variable. Here an example from the code above:
![riptw() SMD reduction plot](plots/smd0.png)
