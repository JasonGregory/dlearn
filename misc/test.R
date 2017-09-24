i_data <- readr::read_rds("inst/other_data/propertyData.rds")
titanic <- readr::read_csv("inst/other_data/train.csv")

library(dplyr)
library(dlearn)
options(max.print=999999)
options(scipen=999)

# test different datasets -------------------------------------------------
explore_importance(mtcars, "mpg") %>% arrange(bst_rank) %>% select(variable, bst_gain)
explore_importance(select(titanic, -PassengerId), "Survived", "reg:logistic") %>% arrange(bst_rank) %>% select(variable, bst_gain)
temp <- explore_importance(select(i_data, -parcelid, -taxvaluedollarcnt), "taxamount")

# updated code ------------------------------------------------------------

explore_importance <- function(data, var_outcome, objective = "reg:linear") {
  # Preparing importance data
  treatment <- vtreat::designTreatmentsZ(data, colnames(data), verbose = FALSE)
  treated <- as_tibble(vtreat::prepare(treatment, data)) %>%
    select(-contains("catP"))
  importance <- tibble(variable = names(treated)) %>%
    left_join(treatment$scoreFrame, c("variable" = "varName")) %>%
    select(variable, origName, code) %>%
    group_by(origName) %>%
    mutate(var_count = n()) %>%
    filter((var_count == 2 & code != "isBAD") | var_count == 1) %>%
    select(-var_count) %>%
    filter(row_number() == 1) %>%
    ungroup
  var_outcome <- importance %>% filter(origName == var_outcome) %>% lselect(variable)
  treated <- select(treated, one_of(importance$variable)) %>%
    select(var_outcome, everything())

  trainIndex <- caret::createDataPartition(lselect(treated, var_outcome),
                                           p = .75,
                                           list = FALSE,
                                           times = 1)

  dataTrain <- treated[trainIndex,]
  dataTest <- treated[-trainIndex,]

  dTrain <- xgboost::xgb.DMatrix(data = as.matrix(dataTrain[,-1]), label = lselect(dataTrain, var_outcome))
  dTest <- xgboost::xgb.DMatrix(data = as.matrix(dataTest[,-1]), label = lselect(dataTest, var_outcome))

  # Boosted Model (will need a paramater to adjust for the objective function)
  watchlist <- list(train=dTrain, test=dTest)
  bst <- xgboost::xgb.train(data=dTrain, nround=400, watchlist=watchlist, objective = objective,
                   early_stopping_rounds = 5, verbose = FALSE, eta = .1)

  bst_importance <- as_tibble(xgboost::xgb.importance(colnames(dataTrain[,-1]), model = bst))
  bst_importance <- bst_importance %>%
    mutate(bst_rank = row_number(desc(Gain))) %>%
    rename(variable = Feature, bst_gain = Gain) %>%
    select(variable, bst_gain, bst_rank)

  importance <- importance %>%
    left_join(bst_importance, c("variable")) %>%
    mutate(bst_gain = if_else(is.na(bst_gain), 0, bst_gain),
           bst_rank = if_else(is.na(bst_rank), max(bst_importance$bst_rank), bst_rank)
           ) %>%
    select(-variable) %>%
    rename(variable = origName) %>%
    arrange(bst_rank)

  return(importance)
}


# old code ----------------------------------------------------------------

explore_importance <- function(data, var_outcome) {
  # Preparing importance data
  treatment <- designTreatmentsZ(data, colnames(data), verbose = FALSE)
  treated <- as_tibble(prepare(treatment, data))
  treated <- select(treated, -contains("catP"))
  describe <- prep_describe(treated)
  importance <- describe %>%
    left_join(treatment$scoreFrame, c("variable" = "varName")) %>%
    select(variable, origName, code, class, total_count, dist_count)

  var_outcome <- importance %>% filter(origName == var_outcome) %>% prep_select(variable)

  # Individual correlation section
  num_data <- select_if(treated, is.numeric)

  summaryFns = list(
    spearman = function(x) cor(x, select(treated, var_outcome), method = "spearman"),
    pearson = function(x) cor(x, select(treated, var_outcome), method = "pearson"),
    r_squared = function(x) cor(x, select(treated, var_outcome), method = "pearson")^2
    # Pearson correlation evaluated the linear relationship. Spearman evaluates monotonic relationship (change together but not at the same rate).
    # Spearman is looking at one type of non-linear relationship.
  )

  summary_data <- as.data.frame(sapply(summaryFns, function(fn){num_data %>% summarise_all(fn)}))
  if (ncol(summary_data) == 0) {
    summary_data <- tibble(
      variable = "none"
    )
  } else {
    summary_data <- summary_data %>%
      tibble::rownames_to_column(var = "variable") %>%
      tidyr::unnest()
  }

  lm <- tidy(lm(paste(var_outcome, "~ ."), data = treated))

  correlation <- summary_data %>%
    full_join(lm, by = c("variable" = "term")) %>%
    dplyr::rename(linear_stdError = std.error, linear_pValue = p.value) %>%
    mutate(spearman = round(spearman, 5),
           pearson = round(pearson, 5),
           r_squared = round(r_squared,5),
           linear_pValue = round(linear_pValue,5)) %>%
    select(variable, spearman, pearson, r_squared, linear_pValue) %>%
    filter(variable != "(Intercept)") %>%
    as.tibble

  # Multi-correlation section (create a seperate function for this section)

  multi_correlation <- rcorr(as.matrix(treated))
  multi_correlation <- as.tibble(as.data.frame(round(multi_correlation$r,5)))
  multi_correlation <- multi_correlation %>%
    rownames_to_column(var = "variable") %>%
    gather(-variable, key = "comparison_variable", value = "correlation") %>%
    select(variable, comparison_variable, correlation, everything()) %>%
    arrange(variable, desc(abs(correlation))) %>%
    filter(variable != comparison_variable)

  multi_correlation <- multi_correlation %>%
    group_by(variable) %>%
    mutate(avg_corr = mean(correlation),
           max_corr = max(abs(correlation)),
           rank_corr = row_number(desc(abs(correlation)))
    ) %>%
    ungroup() %>%
    filter(max_corr == correlation)

  multi_correlation <- multi_correlation %>%
    rename(top_corrVariable = comparison_variable) %>%
    select(variable, top_corrVariable, max_corr, avg_corr)

  # Decision tree section
  trainIndex <- caret::createDataPartition(prep_select(treated, var_outcome),
                                           p = .75,
                                           list = FALSE,
                                           times = 1)

  dataTrain <- treated[trainIndex,]
  dataTest <- treated[-trainIndex,]

  dTrain <- xgb.DMatrix(data = as.matrix(dataTrain[,-1]), label = prep_select(dataTrain, var_outcome))
  dTest <- xgb.DMatrix(data = as.matrix(dataTest[,-1]), label = prep_select(dataTest, var_outcome))

  # Boosted Model (will need a paramater to adjust for the objective function)
  watchlist <- list(train=dTrain, test=dTest)
  bst <- xgb.train(data=dTrain, nround=200, watchlist=watchlist, objective = "reg:linear",
                   eval_metric = "rmse", early_stopping_rounds = 5, verbose = FALSE, eta = .1)

  bst_importance <- as.tibble(xgb.importance(model = bst))
  bst_importance <- bst_importance %>%
    mutate(bst_rank = row_number(desc(Gain))) %>%
    rename(variable = Feature, bst_gain = Gain) %>%
    select(variable, bst_gain, bst_rank)


  # Random Forest Model (Useful for comparison purposes for correlated variables. Need paramater for objective function)
  rfm <- xgboost(data = dTrain, max.depth = 4, tree_method = "approx",
                 nthread = 10, num_parallel_tree = 200, subsample = 0.5, colsample_bytree =0.5,
                 nround = 1, objective = "reg:linear", metrics = "rmse", verbose = FALSE)
  rfm_importance <- as.tibble(xgb.importance(model = rfm))
  rfm_importance <- rfm_importance %>%
    mutate(rfm_rank = row_number(desc(Gain))) %>%
    rename(variable = Feature, rfm_gain = Gain) %>%
    select(variable, rfm_gain, rfm_rank)

  importance <- importance %>%
    left_join(correlation, c("variable")) %>%
    left_join(multi_correlation, c("variable")) %>%
    left_join(bst_importance, c("variable")) %>%
    left_join(rfm_importance, c("variable"))

  return(importance)
}

