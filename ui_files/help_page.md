# Power Calculations for Single Stage Studies of a Proportion

### Acceptance Criteria Risk Level

In each of the calculators, there is a drop down selector corresponding to the 
level of risk for the requirement.
These risk levels affect the choice of test statistic (point estimate, upper confidence limit, or lower confidence limit):

+ **Low**: For requirements specifying a minimum value (e.g. sens/spec), the UCL is the test statistic. For requirements specifying a maximum value (e.g. no-call rate), the LCL is the test statistic.
+ **Medium**: The point estimate is the test statistic.
+ **High**: For requirements specifying a minimum value, the LCL is the test statistic. For requirements specifying a maximum value, the UCL is the test statistic.
+ **High Delta**: The point estimate must clear the requirement, and in addition for requirements specifying a minimum value the LCL must clear some minimum level (requirement minus delta). For requirements specifying a maximum value, the UCL must be below some maximum level (requirement plus delta).

### Explore Sample Sizes

In this tab, the goal is to find appropriate sample sizes for the problem at hand.

+ The x-axis of the plot will be the **sample sizes** in the range you have selected.
+ The y-axis of the plot is the probability of failing (or passing if you check the "Flip Y-Axis"" box) the acceptance criteria. 
+ Different levels of true probability (e.g. true sensitivity, true specificity) can be input as comma-separated values and will be plotted as separate lines.

### Explore Effect Sizes

In this tab, the goal is to find how probability of passing/failing the acceptance criteria changes for given sample sizes when the effect size changes.
It might be easier to use this calculator after you have decided on candidate sample sizes, possibly from using the aforementioned the sample size calculator.

+ The x-axis of the plot will be the **effect sizes** in the range you have selected. The effect size range slider automatically looks at effect sizes in the positive and negative direction. Keep in mind that effect sizes represent deviations from the specified requirement.
+ The y-axis is the probability of failing (or passing if you check the "Flip Y-Axis"" box) the acceptance criteria, similar to how it is defined above. 
+ Different sample sizes can be input as comma-separated values and will be plotted as separate lines.

### Visualize CIs given a sample size and effect size

In this tab, you can drill down to investigate a particular sample size and effect size in more detail.
The number of incorrect samples before failure is automatically listed, as well as a plot
displaying all observable values of the point estimate, corresponding confidence intervals,
and the value of the binomial cumulative distribution function.
