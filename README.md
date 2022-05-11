# Weighting Methods for Rare Event Identification from Imbalanced Datasets
This is a R inplementation of "Weighting Methods for Rare Event Identification from Imbalanced Datasets", as described in the paper.
https://www.frontiersin.org/articles/10.3389/fdata.2021.715320/full#h9
## Abstract
In machine learning, we often face the situation where the event we are interested in has very few data points buried in a massive amount of data. This is typical in network monitoring, where data are streamed from sensing or measuring units continuously but most data are not for events. With imbalanced datasets, the classifiers tend to be biased in favor of the main class. Rare event detection has received much attention in machine learning, and yet it is still a challenging problem. In this paper, we propose a remedy for the standing problem. Weighting and sampling are two fundamental approaches to address the problem. We focus on the weighting method in this paper. We first propose a boosting-style algorithm to compute class weights, which is proved to have excellent theoretical property. Then we propose an adaptive algorithm, which is suitable for real-time applications. The adaptive nature of the two algorithms allows a controlled tradeoff between true positive rate and false positive rate and avoids excessive weight on the rare class, which leads to poor performance on the main class. Experiments on power grid data and some public datasets show that the proposed algorithms outperform the existing weighting and boosting methods, and that their superiority is more noticeable with noisy data.

## Code

### Prerequisites

python3

R Studio






