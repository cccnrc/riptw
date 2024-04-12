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

remotes::install_github( "cccnrc/riptw" )
```
If you know a bit of R code (no worries, you don't really need to) you noticed that it only requires [devtools](https://devtools.r-lib.org/).

The rest of dependencies you need will be installed directly with `riptw`, that's why installation will probably take a while (but you need to perform it only once :wink:)

# Run riptw
Let's try how simple is to use `riptw` on the R built-in dataset [quakes](https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/quakes) which comes along R by default
```
library(riptw)

### import the built-in dataset quakes
quakes <- datasets::quakes

### create a fake categorical variable "treatment" to normalize covariates on
quakes$treatment <- sample( c( rep( 0, nrow(quakes)/8*6 ), rep( 1, nrow(quakes)/8*2 ) ))

IPTW <- riptw( data = quakes, formula = 'treatment ~ lat + long + depth + mag + stations' )
```
_That's all!_ You now have your `IPTW` adjusted covariates in the IPTW object.

`riptw` will also check all the variables you passed and print out the type (numeric vs. categorical) you have stored in your data.
Be sure to correct if some `factor` are stored as `numeric` in your data!

`IPTW` (in the example above) is the object returned by `riptw()` function, and it is composed of 4 parts, that you can access through `IPTW$data` or `IPTW$plot` etc:
- *data*: the input database with some new columns:
    - Propensity Score for each treatment variable group (each column starts with `PS_` and is followed by the group name)
    - Inverse Probability of Treatment Weighting (`iptw` column) for each sample based on its treatment group
    - Standardised Weights (`sw` column): this is _what you are really interested in_, your dataset [weights](https://www.statology.org/weighted-least-squares-in-r/) to be used in regressions or other analysis you want to operate
- *unadjusted*: covariates stratification table before IPTW-adjustment
- *adjusted*: covariates stratification table after IPTW-adjustment
- *plot*: this is a graphical representation of how normalized IPTW is able to reduce each covariate effect on the treatment variable. Standardised Mean Difference (SMD) for each variable stratification with treatment groups are used for comparison, ***adjusted*** SMD are represented as a continuous line and ***unadjusted*** SMD as a dotted line. The red vertical bar at 0.1 represent the cutoff which is normally used to determine if the variable significantly stratificates with the group, thus if a covariate is significantly influencing treatment variable. Here an example from the code above which shows how we nullify `stations`, `mag` and `lat` potential bias through IPTW adjustment:
![riptw() SMD reduction plot](plots/smd0.png)
*Please Note*: your plot might look different if you try the code above as we are assigning `quakes$treatment` values randomly with [sample()](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sample) function

# Use riptw results
Now you can simply use your `sw` column weights to correct regression or survival analysis in which you want to eliminate possible confounding factors and analyze actual effect of your treatment variable!

You will simply need to analyze outcome and treatment variable on a weighted model, without further need of including any covariate! :wink:

You will find plenty of examples of how to perform weighted regression or survival analysis on the internet! Just as example, if your outcome of interest is binary:
```
glm( outcome ~ treatment, family=binomial, data=IPTW$data, weights=sw)
```

# Customize riptw
`riptw` allows for a lot of more detailed analysis, such as implementing Restricted Cubic Spline transformation of your continuous variables to account for possible non-linear effects, and much more customization. Take a look at:
```
?riptw
```
