####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                           ESTABLECER RUTAS                            ####

path_root <- 'C:/Users/jmche/OneDrive/Documentos/Universidad/TFG/Scripts R'
path_src  <- file.path(path_root, 'code/src')
path_data <- file.path(path_root, 'data')
memory.limit(size = 4000000)

####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                    CARGAR LIBRERÍAS Y FUNCIONES                       ####
library(data.table) 
library(caret)
library(ranger)
library(tidyverse)
library(pROC)
library(gridExtra)
library(latex2exp)


source(file.path(path_src, 'CodeManagement.R'))


####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                             CARGAR DATOS                              ####

dataENS.dt <- readRDS(file.path(path_data, "dataENS.dt.rds"))
str(dataENS.dt)

for (j in seq_len(ncol(dataENS.dt))){
  
  set(dataENS.dt, which(is.na(dataENS.dt[[j]])), j, '*')
  
}

dataENS.dt <- dataENS.dt[CNO_AS_true != '*']


CNO3_levels <- sort(union(unique(dataENS.dt$CNO_AS_ed), unique(dataENS.dt$CNO_AS_raw)))
CNO2_levels <- sort(union(unique(Code3toCode2(dataENS.dt$CNO_AS_ed)), unique(Code3toCode2(dataENS.dt$CNO_AS_raw))))
CNO1_levels <- sort(union(unique(CNO2toCNO1(Code3toCode2(dataENS.dt$CNO_AS_ed))), unique(CNO2toCNO1(Code3toCode2(dataENS.dt$CNO_AS_raw)))))
CS_levels   <- sort(union(unique(CNOtoCS(dataENS.dt$CNO_AS_raw)), unique(CNOtoCS(dataENS.dt$CNO_AS_true))))

CNAE3_levels <- sort(union(unique(dataENS.dt$CNAE_AS), unique(dataENS.dt$CNAE_AS)))
CNAE2_levels <- sort(union(unique(Code3toCode2(dataENS.dt$CNAE_AS)), unique(Code3toCode2(dataENS.dt$CNAE_AS))))
CNAE1_levels <- sort(union(unique(CNAE2toCNAE1(Code3toCode2(dataENS.dt$CNAE_AS))), unique(CNAE2toCNAE1(Code3toCode2(dataENS.dt$CNAE_AS)))))

### Se definen nuevas variables y se pasan las variables categóricas a factores 
dataENS.dt[
  , grupoEdad        := factor(ageGroup(EDADa))][
  , CNO1_AS_raw      := factor(CNO2toCNO1(Code3toCode2(CNO_AS_raw)), levels = CNO1_levels)][
  , CNO2_AS_raw      := factor(Code3toCode2(CNO_AS_raw), levels = CNO2_levels)][
  , CNO3_AS_raw      := factor(CNO_AS_raw, levels = CNO3_levels)][
  , CNO_AS_raw       := NULL][
  , CNO1_AS_true     := factor(CNO2toCNO1(Code3toCode2(CNO_AS_true)), levels = CNO1_levels)][
  , CNO2_AS_true     := factor(Code3toCode2(CNO_AS_true), levels = CNO2_levels)][
  , CNO3_AS_true     := factor(CNO_AS_true, levels = CNO3_levels)][
  , CNO_AS_true      := NULL][
  , claseSocial_raw  := factor(CNOtoCS(CNO3_AS_raw), levels = CS_levels)][
  , claseSocial_true := factor(CNOtoCS(CNO3_AS_true), levels = CS_levels)][  
  , error_CNO1       := factor(1L * (CNO1_AS_raw != CNO1_AS_true))][
  , error_CNO2       := factor(1L * (CNO2_AS_raw != CNO2_AS_true))][
  , error_CNO3       := factor(1L * (CNO3_AS_raw != CNO3_AS_true))][
  , error_claseSocial:= factor(1L * (claseSocial_raw != claseSocial_true))][
  , CNAE1_AS         := factor(CNAE2toCNAE1(Code3toCode2(CNAE_AS)), levels = CNAE1_levels)][
  , CNAE2_AS         := factor(Code3toCode2(CNAE_AS), levels = CNAE2_levels)][
  , CNAE3_AS         := factor(CNAE_AS, levels = CNAE3_levels)]

# Se reimputan los valores NA a *
for (j in seq_len(ncol(dataENS.dt))){
  
  set(dataENS.dt, which(is.na(dataENS.dt[[j]])), j, '*')
  
}

####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                               ITERATIONS                              ####
nIter <- 30
seeds <- c(267, 137, 596, 738, 595,  50, 589, 174, 451, 
           603, 661, 572, 751, 725, 454, 699, 868,  57, 
           891, 411, 721, 106, 782, 232,  48,  16, 946, 
           816, 484, 269)
auc.dt <- data.table(
  iteration = integer(),
  model     = character(),
  rank      = character(),
  dataset   = character(),
  variable  = character(),
  auc       = numeric()
)

roc.list <- vector('list', nIter)
roc_down.list <- vector('list', nIter)
roc_up.list <- vector('list', nIter)

importance.dt <- data.table(
  iteration  = integer(),
  model      = character(),
  regressor  = character(),
  importance = character(),
  class      = character(),
  variable   = character()
)

CNO1_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CNO1_AS_running    = character(),
  ned                = numeric(),
  CNO1_running_total = numeric(),
  pseudobias         = numeric()
)

CNO1_total_score.dt <- CNO1_total_prob.dt

CNO2_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CNO2_AS_running    = character(),
  ned                = numeric(),
  CNO2_running_total = numeric(),
  pseudobias         = numeric()
)

CNO2_total_score.dt <- CNO2_total_prob.dt

CNO3_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CNO3_AS_running    = character(),
  ned                = numeric(),
  CNO3_running_total = numeric(),
  pseudobias         = numeric()
)

CNO3_total_score.dt <- CNO3_total_prob.dt

CS_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CS_AS_running      = character(),
  ned                = numeric(),
  CS_running_total   = numeric(),
  pseudobias         = numeric()
)

CS_total_score.dt <- CS_total_prob.dt


####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                       CONSTRUCCION DE FORMULAS                        ####

# Construyo las formulas. 
# Como regresores aparecen todas las variables salvo las variables relacionadas con el valor true o con el error.

target_CNO1     <- 'error_CNO1'
regressors_CNO1 <- c(
  'PROXY_0', 'A7_2a', 'SEXOa', 'grupoEdad', 'CCAA',
  'F7_2', 'F16a_2', 'F16m_2', 'F17a_2', 'F17m_2', 'F8_2', 'F18', 'F9',  
  'D28', 'A10_i',
  'ESTRATO',
  'CNAE1_AS', 'CNAE2_AS', 'CNAE3_AS', 'CNO1_AS_raw', 'CNO2_AS_raw', 'CNO3_AS_raw', 'claseSocial_raw')
formula_CNO1 <- as.formula(
  paste(target_CNO1, paste(regressors_CNO1, collapse = ' + '), sep = ' ~ '))

target_CNO2     <- 'error_CNO2'
regressors_CNO2 <- regressors_CNO1
formula_CNO2    <- as.formula(
  paste(target_CNO2, paste(regressors_CNO2, collapse = ' + '), sep = ' ~ '))

target_CNO3     <- 'error_CNO3'
regressors_CNO3 <- regressors_CNO1
formula_CNO3    <- as.formula(
  paste(target_CNO3, paste(regressors_CNO3, collapse = ' + '), sep = ' ~ '))

target_CS     <- 'error_claseSocial'
regressors_CS <- regressors_CNO1
formula_CS    <- as.formula(
  paste(target_CS, paste(regressors_CS, collapse = ' + '), sep = ' ~ '))

for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  # Division de toda la base de 
  # datos en train (80%)
  # y test (20%)
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing base model CNO1...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                          * MODELO BASE                                ####
  
  ## ..............................  CNO1  .................................. ####
  # ** Ajuste del modelo de random forest                                     ####
  
  set.seed(seeds[iter])
  normalRF_CNO1 <- ranger(
    formula = formula_CNO1, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO1_train <- roc(dataENS_train.dt$error_CNO1, normalRF_CNO1$predictions[, '1'])
  auc_normalRF_prob_CNO1_train <- auc(roc_normalRF_prob_CNO1_train)
  
  pred_normalRF_prob_CNO1      <- predict(normalRF_CNO1, data = dataENS_test.dt)
  roc_normalRF_prob_CNO1_test  <- roc(dataENS_test.dt$error_CNO1, pred_normalRF_prob_CNO1$predictions[, '1'])
  auc_normalRF_prob_CNO1_test  <- auc(roc_normalRF_prob_CNO1_test)
  
  roc_normalRF_score_CNO1_train <- roc(dataENS_train.dt$error_CNO1, dataENS_train.dt$FACTORADULTO * normalRF_CNO1$predictions[, '1'])
  auc_normalRF_score_CNO1_train <- auc(roc_normalRF_score_CNO1_train)
  
  roc_normalRF_score_CNO1_test  <- roc(dataENS_test.dt$error_CNO1, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1$predictions[, '1'])
  auc_normalRF_score_CNO1_test  <- auc(roc_normalRF_score_CNO1_test)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO1_train, 
                                       auc_normalRF_prob_CNO1_test,
                                       auc_normalRF_score_CNO1_train, 
                                       auc_normalRF_score_CNO1_test), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'base',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO1',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO1_train, 
    test_prob   = roc_normalRF_prob_CNO1_test,
    train_score = roc_normalRF_score_CNO1_train, 
    test_score  = roc_normalRF_score_CNO1_test)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO1 <- importance(normalRF_CNO1)
  importance_normalRF_CNO1.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO1)),
    model      = 'base',
    regressor  = names(importance_normalRF_CNO1),
    importance = importance_normalRF_CNO1,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO1)]),
    variable   = 'CNO1')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO1.dt
  ))
  cat('ok.\n')
  
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO1$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1$predictions[, '1'],
    CNO1_AS_true = dataENS_test.dt$CNO1_AS_true,
    CNO1_AS_raw  = dataENS_test.dt$CNO1_AS_raw)
  
  pseudobias_eval_prob.dt <- copy(pseudobias_eval.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob.dt, prob.rank)
  CNO1_running_total_prob.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob.dt)){
    pseudobias_eval_prob.dt[
      1:i, CNO1_AS_running := CNO1_AS_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_AS_raw]
    CNO1_running_total_prob.dt <- rbindlist(list(
      CNO1_running_total_prob.dt, 
      pseudobias_eval_prob.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_prob.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'prob'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  setcolorder(CNO1_running_total_prob.dt, names(CNO1_total_prob.dt))
  
  CNO1_total_prob.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_running_total_prob.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score.dt <- copy(pseudobias_eval.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score.dt, score.rank)
  CNO1_running_total_score.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score.dt)){
    pseudobias_eval_score.dt[
      1:i, CNO1_AS_running := CNO1_AS_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_AS_raw]
    CNO1_running_total_score.dt <- rbindlist(list(
      CNO1_running_total_score.dt, 
      pseudobias_eval_score.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_score.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'score'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  
  setcolorder(CNO1_running_total_score.dt, names(CNO1_total_score.dt))
  
  CNO1_total_score.dt <- rbindlist(list(
    CNO1_total_score.dt, CNO1_running_total_score.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SUBMUESTREO                         ####
  
  ## ..............................  CNO1  .................................. ####
  ## ** Ajuste del modelo de random forest con submuestreo en train           ####
  
  cat('    Computing downsampling model...\n')
  
  length(which(dataENS_train.dt$error_CNO1 == 1)) # 831
  length(which(dataENS_train.dt$error_CNO1 == 0)) # 13949
  
  dataENS_train_down.dt = downSample(x = dataENS_train.dt, y = dataENS_train.dt$error_CNO1)
  length(which(dataENS_train_down.dt$error_CNO1 == 1)) # 831
  length(which(dataENS_train_down.dt$error_CNO1 == 0)) # 831
  
  set.seed(1)
  normalRF_CNO1_down <- ranger(
    formula = formula_CNO1, 
    data    = dataENS_train_down.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO1_train_down <- roc(dataENS_train_down.dt$error_CNO1, normalRF_CNO1_down$predictions[, '1'])
  auc_normalRF_prob_CNO1_train_down <- auc(roc_normalRF_prob_CNO1_train_down)
  
  pred_normalRF_prob_CNO1_down      <- predict(normalRF_CNO1_down, data = dataENS_test.dt)
  roc_normalRF_prob_CNO1_test_down  <- roc(dataENS_test.dt$error_CNO1, pred_normalRF_prob_CNO1_down$predictions[, '1'])
  auc_normalRF_prob_CNO1_test_down  <- auc(roc_normalRF_prob_CNO1_test_down)
  
  roc_normalRF_score_CNO1_train_down <- roc(dataENS_train_down.dt$error_CNO1, dataENS_train_down.dt$FACTORADULTO * normalRF_CNO1_down$predictions[, '1'])
  auc_normalRF_score_CNO1_train_down <- auc(roc_normalRF_score_CNO1_train_down)
  
  roc_normalRF_score_CNO1_test_down  <- roc(dataENS_test.dt$error_CNO1, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1_down$predictions[, '1'])
  auc_normalRF_score_CNO1_test_down  <- auc(roc_normalRF_score_CNO1_test_down)
  
  auc_table_down <- cbind(data = c("train", "test", "train", "test"),
                          rank = c("prob", "prob", "score", "score"),
                          AUC     = round(c(auc_normalRF_prob_CNO1_train_down,
                                            auc_normalRF_prob_CNO1_test_down,
                                            auc_normalRF_score_CNO1_train_down,
                                            auc_normalRF_score_CNO1_test_down), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'downsampling',
    rank      = auc_table_down[, 'rank'],
    dataset   = auc_table_down[, 'data'],
    variable  = 'CNO1',
    auc       = auc_table_down[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  roc.list_down <- list(train_prob  = roc_normalRF_prob_CNO1_train_down,
                        test_prob   = roc_normalRF_prob_CNO1_test_down,
                        train_score = roc_normalRF_score_CNO1_train_down,
                        test_score  = roc_normalRF_score_CNO1_test_down)
  
  
  roc_down.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO1_train_down, 
    test_prob   = roc_normalRF_prob_CNO1_test_down,
    train_score = roc_normalRF_score_CNO1_train_down, 
    test_score  = roc_normalRF_score_CNO1_test_down)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO1_down <- importance(normalRF_CNO1_down)
  importance_normalRF_CNO1_down.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO1_down)),
    model      = 'downsampling',
    regressor  = names(importance_normalRF_CNO1_down),
    importance = importance_normalRF_CNO1_down,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO1_down)]),
    variable   = 'CNO1')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO1_down.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_down.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO1_down$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1_down$predictions[, '1'],
    CNO1_AS_true = dataENS_test.dt$CNO1_AS_true,
    CNO1_AS_raw  = dataENS_test.dt$CNO1_AS_raw)
  
  pseudobias_eval_prob_down.dt <- copy(pseudobias_eval_down.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_down.dt, prob.rank)
  CNO1_running_total_prob_down.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_down.dt)){
    pseudobias_eval_prob_down.dt[
      1:i, CNO1_AS_running := CNO1_AS_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_AS_raw]
    CNO1_running_total_prob_down.dt <- rbindlist(list(
      CNO1_running_total_prob_down.dt, 
      pseudobias_eval_prob_down.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_prob_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  setcolorder(CNO1_running_total_prob_down.dt, names(CNO1_total_prob.dt))
  
  CNO1_total_prob.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_running_total_prob_down.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_down.dt <- copy(pseudobias_eval_down.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_down.dt, score.rank)
  CNO1_running_total_score_down.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_down.dt)){
    pseudobias_eval_score_down.dt[
      1:i, CNO1_AS_running := CNO1_AS_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_AS_raw]
    CNO1_running_total_score_down.dt <- rbindlist(list(
      CNO1_running_total_score_down.dt, 
      pseudobias_eval_score_down.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_score_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'score'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  
  setcolorder(CNO1_running_total_score_down.dt, names(CNO1_total_score.dt))
  
  CNO1_total_score.dt <- rbindlist(list(
    CNO1_total_score.dt, CNO1_running_total_score_down.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SOBREMUESTREO                       ####
  
  ## ..............................  CNO1  .................................. ####
  ## ** Ajuste del modelo de random forest con sobremuestreo en train         ####
  
  cat('    Computing upsampling model...\n')
  
  length(which(dataENS_train.dt$error_CNO1 == 1)) # 831
  length(which(dataENS_train.dt$error_CNO1 == 0)) # 13949
  
  dataENS_train_up.dt = upSample(x = dataENS_train.dt, y = dataENS_train.dt$error_CNO1)
  length(which(dataENS_train_down.dt$error_CNO1 == 1)) # 13949
  length(which(dataENS_train_down.dt$error_CNO1 == 0)) # 13949
  
  set.seed(1)
  normalRF_CNO1_up <- ranger(
    formula = formula_CNO1, 
    data    = dataENS_train_up.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  
  roc_normalRF_prob_CNO1_train_up <- roc(dataENS_train_up.dt$error_CNO1, normalRF_CNO1_up$predictions[, '1'])
  auc_normalRF_prob_CNO1_train_up <- auc(roc_normalRF_prob_CNO1_train_up)
  
  pred_normalRF_prob_CNO1_up      <- predict(normalRF_CNO1_up, data = dataENS_test.dt)
  roc_normalRF_prob_CNO1_test_up  <- roc(dataENS_test.dt$error_CNO1, pred_normalRF_prob_CNO1_up$predictions[, '1'])
  auc_normalRF_prob_CNO1_test_up  <- auc(roc_normalRF_prob_CNO1_test_up)
  
  roc_normalRF_score_CNO1_train_up <- roc(dataENS_train_up.dt$error_CNO1, dataENS_train_up.dt$FACTORADULTO * normalRF_CNO1_up$predictions[, '1'])
  auc_normalRF_score_CNO1_train_up <- auc(roc_normalRF_score_CNO1_train_up)
  
  roc_normalRF_score_CNO1_test_up  <- roc(dataENS_test.dt$error_CNO1, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1_up$predictions[, '1'])
  auc_normalRF_score_CNO1_test_up  <- auc(roc_normalRF_score_CNO1_test_up)
  
  auc_table_up <- cbind(data = c("train", "test", "train", "test"), 
                        rank = c("prob", "prob", "score", "score"), 
                        AUC     = round(c(auc_normalRF_prob_CNO1_train_up, 
                                          auc_normalRF_prob_CNO1_test_up,
                                          auc_normalRF_score_CNO1_train_up, 
                                          auc_normalRF_score_CNO1_test_up), 3))
  
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'upsampling',
    rank      = auc_table_up[, 'rank'],
    dataset   = auc_table_up[, 'data'],
    variable  = 'CNO1',
    auc       = auc_table_up[, 'AUC']
  )
  
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list_up <- list(train_prob  = roc_normalRF_prob_CNO1_train_up, 
                      test_prob   = roc_normalRF_prob_CNO1_test_up,
                      train_score = roc_normalRF_score_CNO1_train_up, 
                      test_score  = roc_normalRF_score_CNO1_test_up)
  
  roc_up.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO1_train_up, 
    test_prob   = roc_normalRF_prob_CNO1_test_up,
    train_score = roc_normalRF_score_CNO1_train_up, 
    test_score  = roc_normalRF_score_CNO1_test_up)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO1_up <- importance(normalRF_CNO1_up)
  importance_normalRF_CNO1_up.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO1_up)),
    model      = 'upsampling',
    regressor  = names(importance_normalRF_CNO1_up),
    importance = importance_normalRF_CNO1_up,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO1_up)]),
    variable   = 'CNO1')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO1_up.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_up.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO1_up$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1_up$predictions[, '1'],
    CNO1_AS_true = dataENS_test.dt$CNO1_AS_true,
    CNO1_AS_raw  = dataENS_test.dt$CNO1_AS_raw)
  
  pseudobias_eval_prob_up.dt <- copy(pseudobias_eval_up.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_up.dt, prob.rank)
  CNO1_running_total_prob_up.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_up.dt)){
    pseudobias_eval_prob_up.dt[
      1:i, CNO1_AS_running := CNO1_AS_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_AS_raw]
    CNO1_running_total_prob_up.dt <- rbindlist(list(
      CNO1_running_total_prob_up.dt, 
      pseudobias_eval_prob_up.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_prob_up.dt[
    , iteration := as.integer(iter)][
    , model := 'upsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  setcolorder(CNO1_running_total_prob_up.dt, names(CNO1_total_prob.dt))
  
  CNO1_total_prob.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_running_total_prob_up.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_up.dt <- copy(pseudobias_eval_up.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_up.dt, score.rank)
  CNO1_running_total_score_up.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_up.dt)){
    pseudobias_eval_score_up.dt[
      1:i, CNO1_AS_running := CNO1_AS_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_AS_raw]
    CNO1_running_total_score_up.dt <- rbindlist(list(
      CNO1_running_total_score_up.dt, 
      pseudobias_eval_score_up.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_score_up.dt[
  , iteration := as.integer(iter)][
  , model := 'upsampling'][
  , rank := 'score'][
  , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  
  setcolorder(CNO1_running_total_score_up.dt, names(CNO1_total_score.dt))
  
  CNO1_total_score.dt <- rbindlist(list(
    CNO1_total_score.dt, CNO1_running_total_score_up.dt))
  cat('ok.\n')  
  
  CNO1_total.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_total_score.dt))  
}

# saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
# saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))
# saveRDS(CNO1_total_prob.dt, file = file.path(path_data, 'CNO1_total_prob.dt'))
# saveRDS(CNO1_total_score.dt, file = file.path(path_data, 'CNO1_total_score.dt'))
# saveRDS(CNO1_total.dt, file = file.path(path_data, 'CNO1_total.dt'))

for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  # Division de toda la base de 
  # datos en train (80%)
  # y test (20%)
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing base model CNO2...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                          * MODELO BASE                                ####
  
  ## ..............................  CNO2  .................................. ####
  # ** Ajuste del modelo de random forest                                     ####
  
  set.seed(seeds[iter])
  normalRF_CNO2 <- ranger(
    formula = formula_CNO2, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO2_train <- roc(dataENS_train.dt$error_CNO2, normalRF_CNO2$predictions[, '1'])
  auc_normalRF_prob_CNO2_train <- auc(roc_normalRF_prob_CNO2_train)
  
  pred_normalRF_prob_CNO2      <- predict(normalRF_CNO2, data = dataENS_test.dt)
  roc_normalRF_prob_CNO2_test  <- roc(dataENS_test.dt$error_CNO2, pred_normalRF_prob_CNO2$predictions[, '1'])
  auc_normalRF_prob_CNO2_test  <- auc(roc_normalRF_prob_CNO2_test)
  
  roc_normalRF_score_CNO2_train <- roc(dataENS_train.dt$error_CNO2, dataENS_train.dt$FACTORADULTO * normalRF_CNO2$predictions[, '1'])
  auc_normalRF_score_CNO2_train <- auc(roc_normalRF_score_CNO2_train)
  
  roc_normalRF_score_CNO2_test  <- roc(dataENS_test.dt$error_CNO2, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2$predictions[, '1'])
  auc_normalRF_score_CNO2_test  <- auc(roc_normalRF_score_CNO2_test)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO2_train, 
                                       auc_normalRF_prob_CNO2_test,
                                       auc_normalRF_score_CNO2_train, 
                                       auc_normalRF_score_CNO2_test), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'base',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO2',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO2_train, 
    test_prob   = roc_normalRF_prob_CNO2_test,
    train_score = roc_normalRF_score_CNO2_train, 
    test_score  = roc_normalRF_score_CNO2_test)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO2 <- importance(normalRF_CNO2)
  importance_normalRF_CNO2.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO2)),
    model      = 'base',
    regressor  = names(importance_normalRF_CNO2),
    importance = importance_normalRF_CNO2,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO2)]),
    variable   = 'CNO2')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO2.dt
  ))
  cat('ok.\n')
  
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO2$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2$predictions[, '1'],
    CNO2_AS_true = dataENS_test.dt$CNO2_AS_true,
    CNO2_AS_raw  = dataENS_test.dt$CNO2_AS_raw)
  
  pseudobias_eval_prob.dt <- copy(pseudobias_eval.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob.dt, prob.rank)
  CNO2_running_total_prob.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob.dt)){
    pseudobias_eval_prob.dt[
      1:i, CNO2_AS_running := CNO2_AS_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_AS_raw]
    CNO2_running_total_prob.dt <- rbindlist(list(
      CNO2_running_total_prob.dt, 
      pseudobias_eval_prob.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_prob.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'prob'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  setcolorder(CNO2_running_total_prob.dt, names(CNO2_total_prob.dt))
  
  CNO2_total_prob.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_running_total_prob.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score.dt <- copy(pseudobias_eval.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score.dt, score.rank)
  CNO2_running_total_score.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score.dt)){
    pseudobias_eval_score.dt[
      1:i, CNO2_AS_running := CNO2_AS_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_AS_raw]
    CNO2_running_total_score.dt <- rbindlist(list(
      CNO2_running_total_score.dt, 
      pseudobias_eval_score.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_score.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'score'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  
  setcolorder(CNO2_running_total_score.dt, names(CNO2_total_score.dt))
  
  CNO2_total_score.dt <- rbindlist(list(
    CNO2_total_score.dt, CNO2_running_total_score.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SUBMUESTREO                         ####
  
  ## ..............................  CNO2  .................................. ####
  ## ** Ajuste del modelo de random forest con submuestreo en train           ####
  
  cat('    Computing downsampling model...\n')
  
  length(which(dataENS_train.dt$error_CNO2 == 1)) # 831
  length(which(dataENS_train.dt$error_CNO2 == 0)) # 13949
  
  dataENS_train_down.dt = downSample(x = dataENS_train.dt, y = dataENS_train.dt$error_CNO2)
  length(which(dataENS_train_down.dt$error_CNO2 == 1)) # 831
  length(which(dataENS_train_down.dt$error_CNO2 == 0)) # 831
  
  set.seed(1)
  normalRF_CNO2_down <- ranger(
    formula = formula_CNO2, 
    data    = dataENS_train_down.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO2_train_down <- roc(dataENS_train_down.dt$error_CNO2, normalRF_CNO2_down$predictions[, '1'])
  auc_normalRF_prob_CNO2_train_down <- auc(roc_normalRF_prob_CNO2_train_down)
  
  pred_normalRF_prob_CNO2_down      <- predict(normalRF_CNO2_down, data = dataENS_test.dt)
  roc_normalRF_prob_CNO2_test_down  <- roc(dataENS_test.dt$error_CNO2, pred_normalRF_prob_CNO2_down$predictions[, '1'])
  auc_normalRF_prob_CNO2_test_down  <- auc(roc_normalRF_prob_CNO2_test_down)
  
  roc_normalRF_score_CNO2_train_down <- roc(dataENS_train_down.dt$error_CNO2, dataENS_train_down.dt$FACTORADULTO * normalRF_CNO2_down$predictions[, '1'])
  auc_normalRF_score_CNO2_train_down <- auc(roc_normalRF_score_CNO2_train_down)
  
  roc_normalRF_score_CNO2_test_down  <- roc(dataENS_test.dt$error_CNO2, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2_down$predictions[, '1'])
  auc_normalRF_score_CNO2_test_down  <- auc(roc_normalRF_score_CNO2_test_down)
  
  auc_table_down <- cbind(data = c("train", "test", "train", "test"),
                          rank = c("prob", "prob", "score", "score"),
                          AUC     = round(c(auc_normalRF_prob_CNO2_train_down,
                                            auc_normalRF_prob_CNO2_test_down,
                                            auc_normalRF_score_CNO2_train_down,
                                            auc_normalRF_score_CNO2_test_down), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'downsampling',
    rank      = auc_table_down[, 'rank'],
    dataset   = auc_table_down[, 'data'],
    variable  = 'CNO2',
    auc       = auc_table_down[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  roc.list_down <- list(train_prob  = roc_normalRF_prob_CNO2_train_down,
                        test_prob   = roc_normalRF_prob_CNO2_test_down,
                        train_score = roc_normalRF_score_CNO2_train_down,
                        test_score  = roc_normalRF_score_CNO2_test_down)
  
  
  roc_down.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO2_train_down, 
    test_prob   = roc_normalRF_prob_CNO2_test_down,
    train_score = roc_normalRF_score_CNO2_train_down, 
    test_score  = roc_normalRF_score_CNO2_test_down)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO2_down <- importance(normalRF_CNO2_down)
  importance_normalRF_CNO2_down.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO2_down)),
    model      = 'downsampling',
    regressor  = names(importance_normalRF_CNO2_down),
    importance = importance_normalRF_CNO2_down,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO2_down)]),
    variable   = 'CNO2')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO2_down.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_down.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO2_down$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2_down$predictions[, '1'],
    CNO2_AS_true = dataENS_test.dt$CNO2_AS_true,
    CNO2_AS_raw  = dataENS_test.dt$CNO2_AS_raw)
  
  pseudobias_eval_prob_down.dt <- copy(pseudobias_eval_down.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_down.dt, prob.rank)
  CNO2_running_total_prob_down.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_down.dt)){
    pseudobias_eval_prob_down.dt[
      1:i, CNO2_AS_running := CNO2_AS_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_AS_raw]
    CNO2_running_total_prob_down.dt <- rbindlist(list(
      CNO2_running_total_prob_down.dt, 
      pseudobias_eval_prob_down.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_prob_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  setcolorder(CNO2_running_total_prob_down.dt, names(CNO2_total_prob.dt))
  
  CNO2_total_prob.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_running_total_prob_down.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_down.dt <- copy(pseudobias_eval_down.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_down.dt, score.rank)
  CNO2_running_total_score_down.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_down.dt)){
    pseudobias_eval_score_down.dt[
      1:i, CNO2_AS_running := CNO2_AS_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_AS_raw]
    CNO2_running_total_score_down.dt <- rbindlist(list(
      CNO2_running_total_score_down.dt, 
      pseudobias_eval_score_down.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_score_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'score'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  
  setcolorder(CNO2_running_total_score_down.dt, names(CNO2_total_score.dt))
  
  CNO2_total_score.dt <- rbindlist(list(
    CNO2_total_score.dt, CNO2_running_total_score_down.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SOBREMUESTREO                       ####
  
  ## ..............................  CNO2  .................................. ####
  ## ** Ajuste del modelo de random forest con sobremuestreo en train         ####
  
  cat('    Computing upsampling model...\n')
  
  length(which(dataENS_train.dt$error_CNO2 == 1)) # 831
  length(which(dataENS_train.dt$error_CNO2 == 0)) # 13949
  
  dataENS_train_up.dt = upSample(x = dataENS_train.dt, y = dataENS_train.dt$error_CNO2)
  length(which(dataENS_train_down.dt$error_CNO2 == 1)) # 13949
  length(which(dataENS_train_down.dt$error_CNO2 == 0)) # 13949
  
  set.seed(1)
  normalRF_CNO2_up <- ranger(
    formula = formula_CNO2, 
    data    = dataENS_train_up.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  
  roc_normalRF_prob_CNO2_train_up <- roc(dataENS_train_up.dt$error_CNO2, normalRF_CNO2_up$predictions[, '1'])
  auc_normalRF_prob_CNO2_train_up <- auc(roc_normalRF_prob_CNO2_train_up)
  
  pred_normalRF_prob_CNO2_up      <- predict(normalRF_CNO2_up, data = dataENS_test.dt)
  roc_normalRF_prob_CNO2_test_up  <- roc(dataENS_test.dt$error_CNO2, pred_normalRF_prob_CNO2_up$predictions[, '1'])
  auc_normalRF_prob_CNO2_test_up  <- auc(roc_normalRF_prob_CNO2_test_up)
  
  roc_normalRF_score_CNO2_train_up <- roc(dataENS_train_up.dt$error_CNO2, dataENS_train_up.dt$FACTORADULTO * normalRF_CNO2_up$predictions[, '1'])
  auc_normalRF_score_CNO2_train_up <- auc(roc_normalRF_score_CNO2_train_up)
  
  roc_normalRF_score_CNO2_test_up  <- roc(dataENS_test.dt$error_CNO2, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2_up$predictions[, '1'])
  auc_normalRF_score_CNO2_test_up  <- auc(roc_normalRF_score_CNO2_test_up)
  
  auc_table_up <- cbind(data = c("train", "test", "train", "test"), 
                        rank = c("prob", "prob", "score", "score"), 
                        AUC     = round(c(auc_normalRF_prob_CNO2_train_up, 
                                          auc_normalRF_prob_CNO2_test_up,
                                          auc_normalRF_score_CNO2_train_up, 
                                          auc_normalRF_score_CNO2_test_up), 3))
  
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'upsampling',
    rank      = auc_table_up[, 'rank'],
    dataset   = auc_table_up[, 'data'],
    variable  = 'CNO2',
    auc       = auc_table_up[, 'AUC']
  )
  
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list_up <- list(train_prob  = roc_normalRF_prob_CNO2_train_up, 
                      test_prob   = roc_normalRF_prob_CNO2_test_up,
                      train_score = roc_normalRF_score_CNO2_train_up, 
                      test_score  = roc_normalRF_score_CNO2_test_up)
  
  roc_up.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO2_train_up, 
    test_prob   = roc_normalRF_prob_CNO2_test_up,
    train_score = roc_normalRF_score_CNO2_train_up, 
    test_score  = roc_normalRF_score_CNO2_test_up)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO2_up <- importance(normalRF_CNO2_up)
  importance_normalRF_CNO2_up.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO2_up)),
    model      = 'upsampling',
    regressor  = names(importance_normalRF_CNO2_up),
    importance = importance_normalRF_CNO2_up,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO2_up)]),
    variable   = 'CNO2')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO2_up.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_up.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO2_up$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2_up$predictions[, '1'],
    CNO2_AS_true = dataENS_test.dt$CNO2_AS_true,
    CNO2_AS_raw  = dataENS_test.dt$CNO2_AS_raw)
  
  pseudobias_eval_prob_up.dt <- copy(pseudobias_eval_up.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_up.dt, prob.rank)
  CNO2_running_total_prob_up.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_up.dt)){
    pseudobias_eval_prob_up.dt[
      1:i, CNO2_AS_running := CNO2_AS_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_AS_raw]
    CNO2_running_total_prob_up.dt <- rbindlist(list(
      CNO2_running_total_prob_up.dt, 
      pseudobias_eval_prob_up.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_prob_up.dt[
    , iteration := as.integer(iter)][
    , model := 'upsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  setcolorder(CNO2_running_total_prob_up.dt, names(CNO2_total_prob.dt))
  
  CNO2_total_prob.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_running_total_prob_up.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_up.dt <- copy(pseudobias_eval_up.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_up.dt, score.rank)
  CNO2_running_total_score_up.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_up.dt)){
    pseudobias_eval_score_up.dt[
      1:i, CNO2_AS_running := CNO2_AS_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_AS_raw]
    CNO2_running_total_score_up.dt <- rbindlist(list(
      CNO2_running_total_score_up.dt, 
      pseudobias_eval_score_up.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_score_up.dt[
    , iteration := as.integer(iter)][
    , model := 'upsampling'][
    , rank := 'score'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  
  setcolorder(CNO2_running_total_score_up.dt, names(CNO2_total_score.dt))
  
  CNO2_total_score.dt <- rbindlist(list(
    CNO2_total_score.dt, CNO2_running_total_score_up.dt))
  cat('ok.\n')  
  
  CNO2_total.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_total_score.dt))  
}

# saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
# saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))
# saveRDS(CNO2_total_prob.dt, file = file.path(path_data, 'CNO2_total_prob.dt'))
# saveRDS(CNO2_total_score.dt, file = file.path(path_data, 'CNO2_total_score.dt'))
# saveRDS(CNO2_total.dt, file = file.path(path_data, 'CNO2_total.dt'))

for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  # Division de toda la base de 
  # datos en train (80%)
  # y test (20%)
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing base model CNO3...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                          * MODELO BASE                                ####
  ## ..............................  CNO3  .................................. ####
  # ** Ajuste del modelo de random forest                                     ####
  
  set.seed(seeds[iter])
  normalRF_CNO3 <- ranger(
    formula = formula_CNO3, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO3_train <- roc(dataENS_train.dt$error_CNO3, normalRF_CNO3$predictions[, '1'])
  auc_normalRF_prob_CNO3_train <- auc(roc_normalRF_prob_CNO3_train)
  
  pred_normalRF_prob_CNO3      <- predict(normalRF_CNO3, data = dataENS_test.dt)
  roc_normalRF_prob_CNO3_test  <- roc(dataENS_test.dt$error_CNO3, pred_normalRF_prob_CNO3$predictions[, '1'])
  auc_normalRF_prob_CNO3_test  <- auc(roc_normalRF_prob_CNO3_test)
  
  roc_normalRF_score_CNO3_train <- roc(dataENS_train.dt$error_CNO3, dataENS_train.dt$FACTORADULTO * normalRF_CNO3$predictions[, '1'])
  auc_normalRF_score_CNO3_train <- auc(roc_normalRF_score_CNO3_train)
  
  roc_normalRF_score_CNO3_test  <- roc(dataENS_test.dt$error_CNO3, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3$predictions[, '1'])
  auc_normalRF_score_CNO3_test  <- auc(roc_normalRF_score_CNO3_test)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO3_train, 
                                       auc_normalRF_prob_CNO3_test,
                                       auc_normalRF_score_CNO3_train, 
                                       auc_normalRF_score_CNO3_test), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'base',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO3',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO3_train, 
    test_prob   = roc_normalRF_prob_CNO3_test,
    train_score = roc_normalRF_score_CNO3_train, 
    test_score  = roc_normalRF_score_CNO3_test)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO3 <- importance(normalRF_CNO3)
  importance_normalRF_CNO3.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO3)),
    model      = 'base',
    regressor  = names(importance_normalRF_CNO3),
    importance = importance_normalRF_CNO3,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO3)]),
    variable   = 'CNO3')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO3.dt
  ))
  cat('ok.\n')
  
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO3$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3$predictions[, '1'],
    CNO3_AS_true = dataENS_test.dt$CNO3_AS_true,
    CNO3_AS_raw  = dataENS_test.dt$CNO3_AS_raw)
  
  pseudobias_eval_prob.dt <- copy(pseudobias_eval.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob.dt, prob.rank)
  CNO3_running_total_prob.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob.dt)){
    pseudobias_eval_prob.dt[
      1:i, CNO3_AS_running := CNO3_AS_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_AS_raw]
    CNO3_running_total_prob.dt <- rbindlist(list(
      CNO3_running_total_prob.dt, 
      pseudobias_eval_prob.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_prob.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'prob'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  setcolorder(CNO3_running_total_prob.dt, names(CNO3_total_prob.dt))
  
  CNO3_total_prob.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_running_total_prob.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score.dt <- copy(pseudobias_eval.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score.dt, score.rank)
  CNO3_running_total_score.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score.dt)){
    pseudobias_eval_score.dt[
      1:i, CNO3_AS_running := CNO3_AS_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_AS_raw]
    CNO3_running_total_score.dt <- rbindlist(list(
      CNO3_running_total_score.dt, 
      pseudobias_eval_score.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_score.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'score'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  
  setcolorder(CNO3_running_total_score.dt, names(CNO3_total_score.dt))
  
  CNO3_total_score.dt <- rbindlist(list(
    CNO3_total_score.dt, CNO3_running_total_score.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SUBMUESTREO                         ####
  
  ## ..............................  CNO3  .................................. ####
  ## ** Ajuste del modelo de random forest con submuestreo en train           ####
  
  cat('    Computing downsampling model...\n')
  
  length(which(dataENS_train.dt$error_CNO3 == 1)) # 831
  length(which(dataENS_train.dt$error_CNO3 == 0)) # 13949
  
  dataENS_train_down.dt = downSample(x = dataENS_train.dt, y = dataENS_train.dt$error_CNO3)
  length(which(dataENS_train_down.dt$error_CNO3 == 1)) # 831
  length(which(dataENS_train_down.dt$error_CNO3 == 0)) # 831
  
  set.seed(1)
  normalRF_CNO3_down <- ranger(
    formula = formula_CNO3, 
    data    = dataENS_train_down.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO3_train_down <- roc(dataENS_train_down.dt$error_CNO3, normalRF_CNO3_down$predictions[, '1'])
  auc_normalRF_prob_CNO3_train_down <- auc(roc_normalRF_prob_CNO3_train_down)
  
  pred_normalRF_prob_CNO3_down      <- predict(normalRF_CNO3_down, data = dataENS_test.dt)
  roc_normalRF_prob_CNO3_test_down  <- roc(dataENS_test.dt$error_CNO3, pred_normalRF_prob_CNO3_down$predictions[, '1'])
  auc_normalRF_prob_CNO3_test_down  <- auc(roc_normalRF_prob_CNO3_test_down)
  
  roc_normalRF_score_CNO3_train_down <- roc(dataENS_train_down.dt$error_CNO3, dataENS_train_down.dt$FACTORADULTO * normalRF_CNO3_down$predictions[, '1'])
  auc_normalRF_score_CNO3_train_down <- auc(roc_normalRF_score_CNO3_train_down)
  
  roc_normalRF_score_CNO3_test_down  <- roc(dataENS_test.dt$error_CNO3, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3_down$predictions[, '1'])
  auc_normalRF_score_CNO3_test_down  <- auc(roc_normalRF_score_CNO3_test_down)
  
  auc_table_down <- cbind(data = c("train", "test", "train", "test"),
                          rank = c("prob", "prob", "score", "score"),
                          AUC     = round(c(auc_normalRF_prob_CNO3_train_down,
                                            auc_normalRF_prob_CNO3_test_down,
                                            auc_normalRF_score_CNO3_train_down,
                                            auc_normalRF_score_CNO3_test_down), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'downsampling',
    rank      = auc_table_down[, 'rank'],
    dataset   = auc_table_down[, 'data'],
    variable  = 'CNO3',
    auc       = auc_table_down[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  roc.list_down <- list(train_prob  = roc_normalRF_prob_CNO3_train_down,
                        test_prob   = roc_normalRF_prob_CNO3_test_down,
                        train_score = roc_normalRF_score_CNO3_train_down,
                        test_score  = roc_normalRF_score_CNO3_test_down)
  
  
  roc_down.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO3_train_down, 
    test_prob   = roc_normalRF_prob_CNO3_test_down,
    train_score = roc_normalRF_score_CNO3_train_down, 
    test_score  = roc_normalRF_score_CNO3_test_down)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO3_down <- importance(normalRF_CNO3_down)
  importance_normalRF_CNO3_down.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO3_down)),
    model      = 'downsampling',
    regressor  = names(importance_normalRF_CNO3_down),
    importance = importance_normalRF_CNO3_down,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO3_down)]),
    variable   = 'CNO3')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO3_down.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_down.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO3_down$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3_down$predictions[, '1'],
    CNO3_AS_true = dataENS_test.dt$CNO3_AS_true,
    CNO3_AS_raw  = dataENS_test.dt$CNO3_AS_raw)
  
  pseudobias_eval_prob_down.dt <- copy(pseudobias_eval_down.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_down.dt, prob.rank)
  CNO3_running_total_prob_down.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_down.dt)){
    pseudobias_eval_prob_down.dt[
      1:i, CNO3_AS_running := CNO3_AS_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_AS_raw]
    CNO3_running_total_prob_down.dt <- rbindlist(list(
      CNO3_running_total_prob_down.dt, 
      pseudobias_eval_prob_down.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_prob_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  setcolorder(CNO3_running_total_prob_down.dt, names(CNO3_total_prob.dt))
  
  CNO3_total_prob.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_running_total_prob_down.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_down.dt <- copy(pseudobias_eval_down.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_down.dt, score.rank)
  CNO3_running_total_score_down.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_down.dt)){
    pseudobias_eval_score_down.dt[
      1:i, CNO3_AS_running := CNO3_AS_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_AS_raw]
    CNO3_running_total_score_down.dt <- rbindlist(list(
      CNO3_running_total_score_down.dt, 
      pseudobias_eval_score_down.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_score_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'score'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  
  setcolorder(CNO3_running_total_score_down.dt, names(CNO3_total_score.dt))
  
  CNO3_total_score.dt <- rbindlist(list(
    CNO3_total_score.dt, CNO3_running_total_score_down.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SOBREMUESTREO                       ####
  
  ## ..............................  CNO3  .................................. ####
  ## ** Ajuste del modelo de random forest con sobremuestreo en train         ####
  
  cat('    Computing upsampling model...\n')
  
  length(which(dataENS_train.dt$error_CNO3 == 1)) # 831
  length(which(dataENS_train.dt$error_CNO3 == 0)) # 13949
  
  dataENS_train_up.dt = upSample(x = dataENS_train.dt, y = dataENS_train.dt$error_CNO3)
  length(which(dataENS_train_down.dt$error_CNO3 == 1)) # 13949
  length(which(dataENS_train_down.dt$error_CNO3 == 0)) # 13949
  
  set.seed(1)
  normalRF_CNO3_up <- ranger(
    formula = formula_CNO3, 
    data    = dataENS_train_up.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  
  roc_normalRF_prob_CNO3_train_up <- roc(dataENS_train_up.dt$error_CNO3, normalRF_CNO3_up$predictions[, '1'])
  auc_normalRF_prob_CNO3_train_up <- auc(roc_normalRF_prob_CNO3_train_up)
  
  pred_normalRF_prob_CNO3_up      <- predict(normalRF_CNO3_up, data = dataENS_test.dt)
  roc_normalRF_prob_CNO3_test_up  <- roc(dataENS_test.dt$error_CNO3, pred_normalRF_prob_CNO3_up$predictions[, '1'])
  auc_normalRF_prob_CNO3_test_up  <- auc(roc_normalRF_prob_CNO3_test_up)
  
  roc_normalRF_score_CNO3_train_up <- roc(dataENS_train_up.dt$error_CNO3, dataENS_train_up.dt$FACTORADULTO * normalRF_CNO3_up$predictions[, '1'])
  auc_normalRF_score_CNO3_train_up <- auc(roc_normalRF_score_CNO3_train_up)
  
  roc_normalRF_score_CNO3_test_up  <- roc(dataENS_test.dt$error_CNO3, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3_up$predictions[, '1'])
  auc_normalRF_score_CNO3_test_up  <- auc(roc_normalRF_score_CNO3_test_up)
  
  auc_table_up <- cbind(data = c("train", "test", "train", "test"), 
                        rank = c("prob", "prob", "score", "score"), 
                        AUC     = round(c(auc_normalRF_prob_CNO3_train_up, 
                                          auc_normalRF_prob_CNO3_test_up,
                                          auc_normalRF_score_CNO3_train_up, 
                                          auc_normalRF_score_CNO3_test_up), 3))
  
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'upsampling',
    rank      = auc_table_up[, 'rank'],
    dataset   = auc_table_up[, 'data'],
    variable  = 'CNO3',
    auc       = auc_table_up[, 'AUC']
  )
  
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list_up <- list(train_prob  = roc_normalRF_prob_CNO3_train_up, 
                      test_prob   = roc_normalRF_prob_CNO3_test_up,
                      train_score = roc_normalRF_score_CNO3_train_up, 
                      test_score  = roc_normalRF_score_CNO3_test_up)
  
  roc_up.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO3_train_up, 
    test_prob   = roc_normalRF_prob_CNO3_test_up,
    train_score = roc_normalRF_score_CNO3_train_up, 
    test_score  = roc_normalRF_score_CNO3_test_up)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO3_up <- importance(normalRF_CNO3_up)
  importance_normalRF_CNO3_up.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO3_up)),
    model      = 'upsampling',
    regressor  = names(importance_normalRF_CNO3_up),
    importance = importance_normalRF_CNO3_up,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO3_up)]),
    variable   = 'CNO3')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO3_up.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_up.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO3_up$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3_up$predictions[, '1'],
    CNO3_AS_true = dataENS_test.dt$CNO3_AS_true,
    CNO3_AS_raw  = dataENS_test.dt$CNO3_AS_raw)
  
  pseudobias_eval_prob_up.dt <- copy(pseudobias_eval_up.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_up.dt, prob.rank)
  CNO3_running_total_prob_up.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_up.dt)){
    pseudobias_eval_prob_up.dt[
      1:i, CNO3_AS_running := CNO3_AS_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_AS_raw]
    CNO3_running_total_prob_up.dt <- rbindlist(list(
      CNO3_running_total_prob_up.dt, 
      pseudobias_eval_prob_up.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_prob_up.dt[
    , iteration := as.integer(iter)][
    , model := 'upsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  setcolorder(CNO3_running_total_prob_up.dt, names(CNO3_total_prob.dt))
  
  CNO3_total_prob.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_running_total_prob_up.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_up.dt <- copy(pseudobias_eval_up.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_up.dt, score.rank)
  CNO3_running_total_score_up.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_up.dt)){
    pseudobias_eval_score_up.dt[
      1:i, CNO3_AS_running := CNO3_AS_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_AS_raw]
    CNO3_running_total_score_up.dt <- rbindlist(list(
      CNO3_running_total_score_up.dt, 
      pseudobias_eval_score_up.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_score_up.dt[
    , iteration := as.integer(iter)][
    , model := 'upsampling'][
    , rank := 'score'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  
  setcolorder(CNO3_running_total_score_up.dt, names(CNO3_total_score.dt))
  
  CNO3_total_score.dt <- rbindlist(list(
    CNO3_total_score.dt, CNO3_running_total_score_up.dt))
  cat('ok.\n')  
  
  CNO3_total.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_total_score.dt))  
}

#saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
#saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))
#saveRDS(CNO3_total_prob.dt, file = file.path(path_data, 'CNO3_total_prob.dt'))
#saveRDS(CNO3_total_score.dt, file = file.path(path_data, 'CNO3_total_score.dt'))
#saveRDS(CNO3_total.dt, file = file.path(path_data, 'CNO3_total.dt'))


for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  # Division de toda la base de 
  # datos en train (80%)
  # y test (20%)
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing base model CS...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                          * MODELO BASE                                ####
  ## ..............................  CS  .................................. ####
  # ** Ajuste del modelo de random forest                                     ####
  
  set.seed(seeds[iter])
  normalRF_CS <- ranger(
    formula = formula_CS, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CS_train <- roc(dataENS_train.dt$error_claseSocial, normalRF_CS$predictions[, '1'])
  auc_normalRF_prob_CS_train <- auc(roc_normalRF_prob_CS_train)
  
  pred_normalRF_prob_CS      <- predict(normalRF_CS, data = dataENS_test.dt)
  roc_normalRF_prob_CS_test  <- roc(dataENS_test.dt$error_claseSocial, pred_normalRF_prob_CS$predictions[, '1'])
  auc_normalRF_prob_CS_test  <- auc(roc_normalRF_prob_CS_test)
  
  roc_normalRF_score_CS_train <- roc(dataENS_train.dt$error_claseSocial, dataENS_train.dt$FACTORADULTO * normalRF_CS$predictions[, '1'])
  auc_normalRF_score_CS_train <- auc(roc_normalRF_score_CS_train)
  
  roc_normalRF_score_CS_test  <- roc(dataENS_test.dt$error_claseSocial, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS$predictions[, '1'])
  auc_normalRF_score_CS_test  <- auc(roc_normalRF_score_CS_test)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CS_train, 
                                       auc_normalRF_prob_CS_test,
                                       auc_normalRF_score_CS_train, 
                                       auc_normalRF_score_CS_test), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'base',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CS',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CS_train, 
    test_prob   = roc_normalRF_prob_CS_test,
    train_score = roc_normalRF_score_CS_train, 
    test_score  = roc_normalRF_score_CS_test)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CS <- importance(normalRF_CS)
  importance_normalRF_CS.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CS)),
    model      = 'base',
    regressor  = names(importance_normalRF_CS),
    importance = importance_normalRF_CS,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CS)]),
    variable   = 'CS')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CS.dt
  ))
  cat('ok.\n')
  
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CS$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS$predictions[, '1'],
    claseSocial_true = dataENS_test.dt$claseSocial_true,
    claseSocial_raw  = dataENS_test.dt$claseSocial_raw)
  
  pseudobias_eval_prob.dt <- copy(pseudobias_eval.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob.dt, prob.rank)
  CS_running_total_prob.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob.dt)){
    pseudobias_eval_prob.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_prob.dt <- rbindlist(list(
      CS_running_total_prob.dt, 
      pseudobias_eval_prob.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_prob.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'prob'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  setcolorder(CS_running_total_prob.dt, names(CS_total_prob.dt))
  
  CS_total_prob.dt <- rbindlist(list(
    CS_total_prob.dt, CS_running_total_prob.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score.dt <- copy(pseudobias_eval.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score.dt, score.rank)
  CS_running_total_score.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score.dt)){
    pseudobias_eval_score.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_score.dt <- rbindlist(list(
      CS_running_total_score.dt, 
      pseudobias_eval_score.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_score.dt[
    , iteration := as.integer(iter)][
    , model := 'base'][
    , rank := 'score'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  
  setcolorder(CS_running_total_score.dt, names(CS_total_score.dt))
  
  CS_total_score.dt <- rbindlist(list(
    CS_total_score.dt, CS_running_total_score.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SUBMUESTREO                         ####
  
  ## ..............................  CS  .................................. ####
  ## ** Ajuste del modelo de random forest con submuestreo en train           ####
  
  cat('    Computing downsampling model...\n')
  
  length(which(dataENS_train.dt$error_claseSocial == 1)) # 831
  length(which(dataENS_train.dt$error_claseSocial == 0)) # 13949
  
  dataENS_train_down.dt = downSample(x = dataENS_train.dt, y = dataENS_train.dt$error_claseSocial)
  length(which(dataENS_train_down.dt$error_claseSocial == 1)) # 831
  length(which(dataENS_train_down.dt$error_claseSocial == 0)) # 831
  
  set.seed(1)
  normalRF_CS_down <- ranger(
    formula = formula_CS, 
    data    = dataENS_train_down.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CS_train_down <- roc(dataENS_train_down.dt$error_claseSocial, normalRF_CS_down$predictions[, '1'])
  auc_normalRF_prob_CS_train_down <- auc(roc_normalRF_prob_CS_train_down)
  
  pred_normalRF_prob_CS_down      <- predict(normalRF_CS_down, data = dataENS_test.dt)
  roc_normalRF_prob_CS_test_down  <- roc(dataENS_test.dt$error_claseSocial, pred_normalRF_prob_CS_down$predictions[, '1'])
  auc_normalRF_prob_CS_test_down  <- auc(roc_normalRF_prob_CS_test_down)
  
  roc_normalRF_score_CS_train_down <- roc(dataENS_train_down.dt$error_claseSocial, dataENS_train_down.dt$FACTORADULTO * normalRF_CS_down$predictions[, '1'])
  auc_normalRF_score_CS_train_down <- auc(roc_normalRF_score_CS_train_down)
  
  roc_normalRF_score_CS_test_down  <- roc(dataENS_test.dt$error_claseSocial, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS_down$predictions[, '1'])
  auc_normalRF_score_CS_test_down  <- auc(roc_normalRF_score_CS_test_down)
  
  auc_table_down <- cbind(data = c("train", "test", "train", "test"),
                          rank = c("prob", "prob", "score", "score"),
                          AUC     = round(c(auc_normalRF_prob_CS_train_down,
                                            auc_normalRF_prob_CS_test_down,
                                            auc_normalRF_score_CS_train_down,
                                            auc_normalRF_score_CS_test_down), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'downsampling',
    rank      = auc_table_down[, 'rank'],
    dataset   = auc_table_down[, 'data'],
    variable  = 'CS',
    auc       = auc_table_down[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  roc.list_down <- list(train_prob  = roc_normalRF_prob_CS_train_down,
                        test_prob   = roc_normalRF_prob_CS_test_down,
                        train_score = roc_normalRF_score_CS_train_down,
                        test_score  = roc_normalRF_score_CS_test_down)
  
  
  roc_down.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CS_train_down, 
    test_prob   = roc_normalRF_prob_CS_test_down,
    train_score = roc_normalRF_score_CS_train_down, 
    test_score  = roc_normalRF_score_CS_test_down)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CS_down <- importance(normalRF_CS_down)
  importance_normalRF_CS_down.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CS_down)),
    model      = 'downsampling',
    regressor  = names(importance_normalRF_CS_down),
    importance = importance_normalRF_CS_down,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CS_down)]),
    variable   = 'CS')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CS_down.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_down.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CS_down$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS_down$predictions[, '1'],
    claseSocial_true = dataENS_test.dt$claseSocial_true,
    claseSocial_raw  = dataENS_test.dt$claseSocial_raw)
  
  pseudobias_eval_prob_down.dt <- copy(pseudobias_eval_down.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_down.dt, prob.rank)
  CS_running_total_prob_down.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_down.dt)){
    pseudobias_eval_prob_down.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_prob_down.dt <- rbindlist(list(
      CS_running_total_prob_down.dt, 
      pseudobias_eval_prob_down.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_prob_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  setcolorder(CS_running_total_prob_down.dt, names(CS_total_prob.dt))
  
  CS_total_prob.dt <- rbindlist(list(
    CS_total_prob.dt, CS_running_total_prob_down.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_down.dt <- copy(pseudobias_eval_down.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_down.dt, score.rank)
  CS_running_total_score_down.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_down.dt)){
    pseudobias_eval_score_down.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_score_down.dt <- rbindlist(list(
      CS_running_total_score_down.dt, 
      pseudobias_eval_score_down.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_score_down.dt[
    , iteration := as.integer(iter)][
    , model := 'downsampling'][
    , rank := 'score'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  
  setcolorder(CS_running_total_score_down.dt, names(CS_total_score.dt))
  
  CS_total_score.dt <- rbindlist(list(
    CS_total_score.dt, CS_running_total_score_down.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      * MODELO CON SOBREMUESTREO                       ####
  
  ## ..............................  CS  .................................. ####
  ## ** Ajuste del modelo de random forest con sobremuestreo en train         ####
  
  cat('    Computing upsampling model...\n')
  
  length(which(dataENS_train.dt$error_claseSocial == 1)) # 831
  length(which(dataENS_train.dt$error_claseSocial == 0)) # 13949
  
  dataENS_train_up.dt = upSample(x = dataENS_train.dt, y = dataENS_train.dt$error_claseSocial)
  length(which(dataENS_train_down.dt$error_claseSocial == 1)) # 13949
  length(which(dataENS_train_down.dt$error_claseSocial == 0)) # 13949
  
  set.seed(1)
  normalRF_CS_up <- ranger(
    formula = formula_CS, 
    data    = dataENS_train_up.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity')
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  
  roc_normalRF_prob_CS_train_up <- roc(dataENS_train_up.dt$error_claseSocial, normalRF_CS_up$predictions[, '1'])
  auc_normalRF_prob_CS_train_up <- auc(roc_normalRF_prob_CS_train_up)
  
  pred_normalRF_prob_CS_up      <- predict(normalRF_CS_up, data = dataENS_test.dt)
  roc_normalRF_prob_CS_test_up  <- roc(dataENS_test.dt$error_claseSocial, pred_normalRF_prob_CS_up$predictions[, '1'])
  auc_normalRF_prob_CS_test_up  <- auc(roc_normalRF_prob_CS_test_up)
  
  roc_normalRF_score_CS_train_up <- roc(dataENS_train_up.dt$error_claseSocial, dataENS_train_up.dt$FACTORADULTO * normalRF_CS_up$predictions[, '1'])
  auc_normalRF_score_CS_train_up <- auc(roc_normalRF_score_CS_train_up)
  
  roc_normalRF_score_CS_test_up  <- roc(dataENS_test.dt$error_claseSocial, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS_up$predictions[, '1'])
  auc_normalRF_score_CS_test_up  <- auc(roc_normalRF_score_CS_test_up)
  
  auc_table_up <- cbind(data = c("train", "test", "train", "test"), 
                        rank = c("prob", "prob", "score", "score"), 
                        AUC     = round(c(auc_normalRF_prob_CS_train_up, 
                                          auc_normalRF_prob_CS_test_up,
                                          auc_normalRF_score_CS_train_up, 
                                          auc_normalRF_score_CS_test_up), 3))
  
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'upsampling',
    rank      = auc_table_up[, 'rank'],
    dataset   = auc_table_up[, 'data'],
    variable  = 'CS',
    auc       = auc_table_up[, 'AUC']
  )
  
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list_up <- list(train_prob  = roc_normalRF_prob_CS_train_up, 
                      test_prob   = roc_normalRF_prob_CS_test_up,
                      train_score = roc_normalRF_score_CS_train_up, 
                      test_score  = roc_normalRF_score_CS_test_up)
  
  roc_up.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CS_train_up, 
    test_prob   = roc_normalRF_prob_CS_test_up,
    train_score = roc_normalRF_score_CS_train_up, 
    test_score  = roc_normalRF_score_CS_test_up)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CS_up <- importance(normalRF_CS_up)
  importance_normalRF_CS_up.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CS_up)),
    model      = 'upsampling',
    regressor  = names(importance_normalRF_CS_up),
    importance = importance_normalRF_CS_up,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CS_up)]),
    variable   = 'CS')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CS_up.dt
  ))
  cat('ok.\n')
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_up.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CS_up$predictions[, '1'],
    score        = dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS_up$predictions[, '1'],
    claseSocial_true = dataENS_test.dt$claseSocial_true,
    claseSocial_raw  = dataENS_test.dt$claseSocial_raw)
  
  pseudobias_eval_prob_up.dt <- copy(pseudobias_eval_up.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_up.dt, prob.rank)
  CS_running_total_prob_up.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_up.dt)){
    pseudobias_eval_prob_up.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_prob_up.dt <- rbindlist(list(
      CS_running_total_prob_up.dt, 
      pseudobias_eval_prob_up.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  
  n <- nrow(dataENS_test.dt)
  CS_running_total_prob_up.dt[
    , iteration := as.integer(iter)][
    , model := 'upsampling'][
    , rank := 'prob'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  setcolorder(CS_running_total_prob_up.dt, names(CS_total_prob.dt))
  
  CS_total_prob.dt <- rbindlist(list(
    CS_total_prob.dt, CS_running_total_prob_up.dt))
  cat('ok. \n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_up.dt <- copy(pseudobias_eval_up.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_up.dt, score.rank)
  CS_running_total_score_up.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_up.dt)){
    pseudobias_eval_score_up.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_score_up.dt <- rbindlist(list(
      CS_running_total_score_up.dt, 
      pseudobias_eval_score_up.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_score_up.dt[
    , iteration := as.integer(iter)][
    , model := 'upsampling'][
    , rank := 'score'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  
  setcolorder(CS_running_total_score_up.dt, names(CS_total_score.dt))
  
  CS_total_score.dt <- rbindlist(list(
    CS_total_score.dt, CS_running_total_score_up.dt))
  cat('ok.\n')  
  
  CS_total.dt <- rbindlist(list(
    CS_total_prob.dt, CS_total_score.dt))  
}

# saveRDS(CS_total_prob.dt, file = file.path(path_data, 'CS_total_prob.dt'))
# saveRDS(CS_total_score.dt, file = file.path(path_data, 'CS_total_score.dt'))
# saveRDS(CS_total.dt, file = file.path(path_data, 'CS_total.dt'))
# saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
# saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))

