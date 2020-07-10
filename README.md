# RSBID: Resampling Strategies for Binary Imbalanced Datasets

This package contains functions of resampling strategies to make the binary imbalanced datasets be more balanced. It is important for an imbalanced dataset before applying a classification algorithm, for the reason that class imbalance will lead to a bad performance of classifiers. 

![]()

## Installation

`RSBID` is available on the github now.

```
# install.packages("devtools")
devtools::install_github("dongyuanwu/RSBID")
devtools::install_github("dongyuanwu/RSBID", build_vignettes=TRUE)  # If you would like to view the vignettes
```

## Available Strategies

`RSBID` contains five strategies now:

### Over-sampling

- Random Over-Sampling Algorithm (`ROS`)
- Synthetic Minority Over-sampling TEchnique (`SMOTE`)
- Synthetic Minority Over-sampling TEchnique-Nominal Continuous (`SMOTE_NC`)

### Under-sampling

- Random Under-Sampling Algorithm (`RUS`)
- Under-Sampling Based on Clustering Algorithm (`SBC`)

## ShinyApp

We also have an online <a href="https://dongyuanwu.shinyapps.io/RSBID" target="_blank">ShinyApp</a>.
