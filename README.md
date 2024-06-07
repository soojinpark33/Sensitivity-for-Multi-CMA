# Sensitivity Analysis for Pretreatment Confounding With Multiple Mediators

Soojin Park<sup>1</sup> and Kevin M. Esterling<sup>2</sup>

<sup>1</sup> School of Education, University of California, Riverside  
<sup>2</sup> School of Public Policy, University of California, Riverside



## Overview

The causal mediation literature has developed techniques to assess the sensitivity of an inference to pretreatment confounding, but these techniques are limited to the case of a single mediator. In this article, we extend sensitivity analysis to possible violations of pretreatment confounding in the case of multiple mediators. In particular, we develop sensitivity analyses under three alternative approaches to effect decomposition: 1) jointly considered mediators, 2) identifiable direct and indirect paths, and 3) interventional analogues effects. With reasonable assumptions, each approach reduces to a single procedure to assess sensitivity in the presence of simultaneous pre- and post-treatment confounding. We demonstrate our sensitivity analysis techniques with a framing experiment that examines whether anxiety mediates respondents' attitudes toward immigration in response to an information prompt.

For more details of our proposed methods, see [our paper](https://journals.sagepub.com/doi/abs/10.3102/1076998620934500). 
Here, we provide `R` codes to reproduce our simulation study and replicate our data analysis. 

## Case Study

* Data
  
The 'framing' data used for the case study can be downloaded from the 'mediation' R package or by clicking [here](https://github.com/kosukeimai/mediation/tree/master/data). 

* `Analysis.R` 
 
   This `R` file replicates Table 1 and Figure 1 of our study.

* `multicma.R` 
 
   This `R` file provides source functions for 'Analysis.R'.

These supplementary materials are provided solely for the purpose of reproducibility and must be used in compliance with academic ethical guidelines. If you reference these materials in your own work, please ensure proper citation of the original sources.
