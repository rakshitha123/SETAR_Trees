# SETAR_Trees

This repository contains the experiments related to a new and accurate tree-based global forecasting algorithm named, SETAR-Tree. 

We use the underlying concept of a Self Exciting Threshold Autoregressive (SETAR) model to develop this new tree algorithm. In contrast to the traditional tree-based algorithms which consider the average of the training outputs in
leaf nodes to forecast new instances, our algorithm trains separate global Pooled Regression (PR) models in each leaf node allowing the model to learn cross-series information during
training. The depth of the tree is internally controlled by conducting a statistical linearity test and measuring the error reduction percentage at each node split. Thus, the proposed
tree model requires minimal external hyperparameter tuning compared to the state-of-theart tree-based algorithms and provides decent results under its default configuration. We
also use this tree algorithm to develop a forest where the forecasts provided by a collection of diverse SETAR-Trees are combined during the forecasting process. The proposed tree and
forest models can also be trained with external covariates.

# Instructions for execution

## Executing the SETAR-Tree Model
You can directly execute the exepriments related to the proposed SETAR-Tree model using the "do_setar_forecasting" function implemented in "./experiments/setar_tree_experiments.R" script.
The function parameters are explained in detail in the script. 
The forecasts, errors, execution times and tree related information (tree depth, number of nodes in the leaf level and number of instances per each leaf node) related to the SETAR-Tree model will be stored into "./results/forecasts/setar_tree", "./results/errors", "./results/execution_times/setar_tree" and "./results/tree_info" folders, respectively.
See the examples provided in "./experiments/setar_tree_experiments.R" script for more details.

## Executing the SETAR Forest Model
You can directly execute the exepriments related to the proposed SETAR Forest model using the "do_setar_forest_forecasting" function implemented in "./experiments/setar_forest_experiments.R" script.
The function parameters are explained in detail in the script. 
The forecasts, errors and execution times related to the SETAR forest model will be stored into "./results/forecasts/setar_forest", "./results/errors" and "./results/execution_times/setar_forest" folders, respectively.
See the examples provided in "./experiments/setar_forest_experiments.R" script for more details.

## Executing the Benchmark Models
In our paper, we have compared the performance of our proposed SETAR-Tree and forest models against a number of benchmarks including 2 traditional univariate forecasting models:
Exponential Smoothing (ETS) and Auto-Regressive Integrated Moving Average (ARIMA), and 7 global forecasting models: PR, Feed-Forward Neural Network (FFNN),
Regression Tree, LightGBM, CatBoost, eXtreme Gradient Boosting (XGBoost) and Random Forest.

The traditional univariate forecasting models can be executed using the "do_local_forecasting" function implemented in "./experiments/local_model_experiments.R" script.
The function parameters are explained in detail in the script. 
See the examples provided in "./experiments/local_model_experiments.R" script for more details.

The global forecasting models can be executed using the "do_global_forecasting" function implemented in "./experiments/global_model_experiments.R" script.
The function parameters are explained in detail in the script. 
See the examples provided in "./experiments/global_model_experiments.R" script for more details.

# Experimental Datasets
The experimental datasets are available in the "datasets" folder.


# Citing Our Work
When using this repository, please cite:

```{r} 
@misc{godahewa2022setar,
  title = {SETAR-Tree: A Novel and Accurate Tree Algorithm for Global Time Series Forecasting},
  author = {Godahewa, Rakshitha and Webb, Geoffrey I. and Bergmeir, Christoph},
  year = {2022}
}
```
