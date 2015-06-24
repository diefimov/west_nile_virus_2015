import warnings
import sklearn
from sklearn.ensemble import GradientBoostingRegressor, GradientBoostingClassifier, \
    RandomForestRegressor, RandomForestClassifier, ExtraTreesClassifier, ExtraTreesRegressor
from sklearn.linear_model import LogisticRegression
import sklearn.metrics as sk_metrics
import custom_metrics
import argparse
import pandas as pd
import timeit
import numpy as np
import ml_metrics
import ast
import cPickle as Pickle
import itertools
import os
import math
import random
from sklearn.pipeline import Pipeline
import six
import inspect


def get_class(kls):
    parts = kls.split('.')
    module = ".".join(parts[:-1])
    m = __import__(module)
    for comp in parts[1:]:
        m = getattr(m, comp)
    return m


def predict_test(feat_importance_fun, mappings, model, na_fill_value, predict, silent, staged_predict,
                 target_col, test_data_file, test_metric, test_pred_file, x_cols, metric_type, weight_col):
    if not silent:
        print("Predicting : %s to %s" % (test_data_file, test_pred_file))

    test_x = load_pd_df(test_data_file)
    if mappings is not None:
        for col in test_x.columns:
            if col in mappings:
                test_x[col] = test_x[col].map(mappings[col]).fillna(na_fill_value)
            else:
                test_x[col] = test_x[col].fillna(na_fill_value)
    test_y = None
    if target_col in test_x.columns:
        test_y = test_x[target_col][test_x[target_col] != na_fill_value]
        test_y2 = test_x[target_col][pd.notnull(test_x[target_col])]
        if len(test_y) != len(test_x) or len(test_y2) != len(test_x):
            test_y = None
        del test_y2

    test_weight = None
    if weight_col is not None:
        if weight_col in test_x.columns:
            test_weight = test_x[weight_col]
            del test_x[weight_col]

    test_x = test_x[x_cols]

    test_pred = predict((model, test_x))
    if test_pred.shape[1] == 1:
        test_pred = pd.DataFrame({'pred': test_pred[:, 0]})
    elif test_pred.shape[1] == 2:
        test_pred = pd.DataFrame({'pred': test_pred[:, 1]})
    else:
        test_pred_df = None
        for c in xrange(test_pred.shape[1]):
            if test_pred_df is None:
                test_pred_df = pd.DataFrame({'pred0': test_pred[:, c]})
            else:
                test_pred_df['pred' + str(c)] = test_pred[:, c]
        test_pred = test_pred_df

    if not silent and test_y is not None:
        print_stages(test_y=test_y, stage_predictions=staged_predict((model, test_x)),
                     test_metric=test_metric, metric_type=metric_type, test_weight=test_weight)

    if not silent:
        feat_importance = feat_importance_fun(model)
        if feat_importance is not None:
            feat_importance = pd.DataFrame({'Features': x_cols,
                                            'Importance': feat_importance})
            pd.set_option('max_columns', len(test_x.columns))
            pd.set_option('max_rows', len(test_x))
            print("Feature importances:")
            feat_importance.sort(columns='Importance', ascending=False, inplace=True)
            feat_importance.index = range(1, len(feat_importance) + 1)
            print(feat_importance)

    test_pred.to_csv(test_pred_file, index=False)


# noinspection PyUnusedLocal
def load_pd_df(file_name, del_old=False, bin_suffix='.bin.pkl'):
    ret_val = None
    bin_file_name = file_name + bin_suffix
    if os.path.isfile(bin_file_name):
        if not os.path.isfile(file_name) or os.path.getmtime(bin_file_name) > os.path.getmtime(file_name):
            ret_val = load_model_bin(model_file=bin_file_name)
            print "Loading %s cache file" % bin_file_name

    if ret_val is None:
        print "Loading %s raw file" % file_name
        ret_val = pd.read_csv(file_name)
        print "Saving %s cache file" % bin_file_name
        save_model_bin(model=ret_val, model_file=bin_file_name)
        if del_old:
            print "Erasing %s raw file" % file_name
            os.remove(file_name)

    return ret_val


def data_filter(data, filter_dict):
    if len(filter_dict) > 0:
        for filter_col in filter_dict:
            data = data[data[filter_col] == filter_dict[filter_col]]
    return data


def train_and_predict(train_data_file, test_data_file, target_col, test_pred_file,
                      test_data_file2, test_pred_file2,
                      model_type, model_file, fit_args, test_metric, na_fill_value,
                      silent, skip_mapping, load_model, train_filter, metric_type, load_type,
                      bootstrap, bootstrap_seed, weight_col):
    start = timeit.default_timer()

    train_x = load_pd_df(train_data_file)

    len_train_before = len(train_x)
    train_x = data_filter(train_x, train_filter)
    if not silent:
        print "Train has %d instances (was %d before filtering)" % (len(train_x), len_train_before)

    mappings = None if skip_mapping else dict()
    if mappings is not None:
        data_all = train_x.append(load_pd_df(test_data_file))
        if test_data_file2 is not None:
            data_all = data_all.append(load_pd_df(test_data_file2))
        if not silent:
            print "Mapping unkown and category values..."
        for col in train_x.columns:
            if col not in ['target_col']:
                if data_all[col].dtype == np.dtype('object'):
                    s = np.unique(data_all[col].fillna(na_fill_value).values)
                    mappings[col] = pd.Series([x[0] for x in enumerate(s)], index=s)
                    train_x[col] = train_x[col].map(mappings[col]).fillna(na_fill_value)
                else:
                    train_x[col] = train_x[col].fillna(na_fill_value)
        del data_all
    train_y = train_x[target_col]
    del train_x[target_col]

    extra_fit_args = dict()
    if weight_col is not None:
        extra_fit_args['sample_weight'] = train_x[weight_col].values
        del train_x[weight_col]

    if 0 < bootstrap < 1.0:
        if bootstrap_seed is not None:
            if not silent:
                print "Setting bootstrap seed to %d" % bootstrap_seed
            np.random.seed(bootstrap_seed)
            random.seed(bootstrap_seed)
        bootstrap_len = int(math.floor(bootstrap * len(train_x)))
        bootstrap_ix = random.sample(range(len(train_x)), bootstrap_len)
        train_x = train_x.iloc[bootstrap_ix]
        train_x.reset_index()
        train_y = train_y.iloc[bootstrap_ix]
        train_y.reset_index()

    x_cols = train_x.columns
    feat_importance_fun = lambda (fitted_model): fitted_model.feature_importances_
    predict = lambda (fitted_model, pred_x): fitted_model.predict(pred_x)
    staged_predict = lambda (fitted_model, pred_x): [predict((fitted_model, pred_x))]

    model = None
    if load_model and os.path.exists(model_file):
        if not silent:
            print "Loading model %s" % model_file
        model = load_model_bin(model_file=model_file)

    if model_type == "RandomForestRegressor":
        if model is None:
            model = RandomForestRegressor(**fit_args)
            model.fit(X=train_x, y=train_y, **extra_fit_args)
        predict = lambda (fitted_model, pred_x): continuous_predict(model=fitted_model, x=pred_x)

    elif model_type == "RandomForestClassifier":
        if model is None:
            model = RandomForestClassifier(**fit_args)
            model.fit(X=train_x, y=train_y, **extra_fit_args)
        predict = lambda (fitted_model, pred_x): pred_proba(model=fitted_model, x=pred_x)
        staged_predict = lambda (fitted_model, pred_x): [predict((fitted_model, pred_x))]

    elif model_type == "ExtraTreesRegressor":
        if model is None:
            model = ExtraTreesRegressor(**fit_args)
            model.fit(X=train_x, y=train_y, **extra_fit_args)
        predict = lambda (fitted_model, pred_x): continuous_predict(model=fitted_model, x=pred_x)

    elif model_type == "ExtraTreesClassifier":
        if model is None:
            model = ExtraTreesClassifier(**fit_args)
            model.fit(X=train_x, y=train_y, **extra_fit_args)
        predict = lambda (fitted_model, pred_x): pred_proba(model=fitted_model, x=pred_x)
        staged_predict = lambda (fitted_model, pred_x): [predict((fitted_model, pred_x))]

    elif model_type == "GradientBoostingRegressor":
        if model is None:
            model = GradientBoostingRegressor(**fit_args)
            model.fit(X=train_x, y=train_y, **extra_fit_args)
        elif load_type == "fit_more":
            model.warm_start = True
            model.n_estimators += fit_args['n_estimators']
            model.fit(X=train_x, y=train_y)
        predict = lambda (fitted_model, pred_x): continuous_predict(model=fitted_model, x=pred_x)
        staged_predict = lambda (fitted_model, pred_x): staged_pred_continuous(model=fitted_model, x=pred_x)
        if load_type == "pred_at" and fit_args['n_estimators'] < model.n_estimators:
            if not silent:
                print ("Predict using %d trees" % fit_args['n_estimators'])
            predict = lambda (fitted_model, pred_x): staged_pred_continuous_at_n(model=fitted_model, x=pred_x,
                                                                                 n=fit_args['n_estimators'])
    elif model_type == "GradientBoostingClassifier":
        if model is None:
            model = GradientBoostingClassifier(**fit_args)
            model.fit(X=train_x, y=train_y, **extra_fit_args)
        elif load_type == "fit_more":
            model.warm_start = True
            model.n_estimators += fit_args['n_estimators']
            model.fit(X=train_x, y=train_y)
        staged_predict = lambda (fitted_model, pred_x): staged_pred_proba(model=fitted_model, x=pred_x)
        predict = lambda (fitted_model, pred_x): pred_proba(model=fitted_model, x=pred_x)
        if load_type == "pred_at" and fit_args['n_estimators'] < model.n_estimators:
            if not silent:
                print ("Predict using %d trees" % fit_args['n_estimators'])
            predict = lambda (fitted_model, pred_x): staged_pred_proba_at_n(model=fitted_model, x=pred_x,
                                                                            n=fit_args['n_estimators'])
    elif model_type == "LogisticRegression":
        if model is None:
            model = LogisticRegression(**fit_args)
            model.fit(X=train_x, y=train_y)
        predict = lambda (fitted_model, pred_x): pred_proba(model=fitted_model, x=pred_x)
        staged_predict = lambda (fitted_model, pred_x): [predict((fitted_model, pred_x))]
        feat_importance_fun = lambda (fitted_model): None

    elif model_type == "SVC":
        if model is None:
            model = sklearn.svm.SVC(**fit_args)
            model.fit(X=train_x, y=train_y)
        predict = lambda (fitted_model, pred_x): pred_proba(model=fitted_model, x=pred_x)
        staged_predict = lambda (fitted_model, pred_x): [predict((fitted_model, pred_x))]
        feat_importance_fun = lambda (fitted_model): None

    elif model_type == "Pipeline":
        if model is None:
            model = Pipeline([
                ('pre_process', get_class(fit_args['pre_process']['name'])(**fit_args['pre_process']['args'])),
                ('model', get_class(fit_args['model']['name'])(**fit_args['model']['args']))
            ])
            model.fit(X=train_x, y=train_y)
        predict = lambda (fitted_model, pred_x): pred_proba(model=fitted_model, x=pred_x)
        staged_predict = lambda (fitted_model, pred_x): [predict((fitted_model, pred_x))]
        feat_importance_fun = lambda (fitted_model): None

    if not silent:
        print "Saving model %s" % model_file
    save_model_bin(model=model, model_file=model_file)

    if not silent:
        stop = timeit.default_timer()
        print "Train time: %d s" % (stop - start)

    del train_x, train_y

    start_pred = timeit.default_timer()
    predict_test(feat_importance_fun=feat_importance_fun,
                 mappings=mappings,
                 model=model,
                 na_fill_value=na_fill_value,
                 predict=predict,
                 silent=silent,
                 staged_predict=staged_predict,
                 target_col=target_col,
                 test_data_file=test_data_file,
                 test_metric=test_metric,
                 test_pred_file=test_pred_file,
                 x_cols=x_cols,
                 metric_type=metric_type,
                 weight_col=weight_col)
    if not silent:
        stop = timeit.default_timer()
        print "Predict time: %d s" % (stop - start_pred)

    if not test_data_file2 is None:
        start_pred = timeit.default_timer()
        predict_test(feat_importance_fun=lambda fitted_model: None,
                     mappings=mappings,
                     model=model,
                     na_fill_value=na_fill_value,
                     predict=predict,
                     silent=silent,
                     staged_predict=staged_predict,
                     target_col=target_col,
                     test_data_file=test_data_file2,
                     test_metric=test_metric,
                     test_pred_file=test_pred_file2,
                     x_cols=x_cols,
                     metric_type=metric_type,
                     weight_col=weight_col)
        if not silent:
            stop = timeit.default_timer()
            print "Predict2 time: %d s" % (stop - start_pred)

    if not silent:
        stop = timeit.default_timer()
        print "Total time: %d s" % (stop - start)


def staged_pred_proba(model, x):
    for pred in model.staged_predict_proba(x):
        yield prob_pred(pred)


def staged_pred_proba_at_n(model, x, n):
    return nth(staged_pred_proba(model=model, x=x), n)


def pred_proba(model, x):
    return prob_pred(model.predict_proba(X=x))


def prob_pred(pred):
    return pred


def staged_pred_continuous(model, x):
    for pred in model.staged_predict(x):
        yield to_2dim(pred)


def staged_pred_continuous_at_n(model, x, n):
    return nth(staged_pred_continuous(model=model, x=x), n)


def continuous_predict(model, x):
    return to_2dim(model.predict(X=x))


def to_2dim(array_val):
    return np.array(array_val, ndmin=2).transpose()


def nth(iterable, n):
    return next(itertools.islice(iterable, n, None))


def avg_eval_metric(eval_metric, test_y, prediction, metric_type):
    if prediction.shape[1] == 1:
        return eval_metric(test_y, prediction[:, 0])
    elif prediction.shape[1] == 2:
        return eval_metric(test_y, prediction[:, 1])
    else:
        metric_val = 0.0
        metric_count = 0.0
        if metric_type == "cumulative":
            cur_pred = np.zeros(prediction.shape[0])
            for c in xrange(prediction.shape[1] - 1):
                cur_actual = np.array(np.array(test_y) <= c).astype(int)
                cur_pred += prediction[:, c]
                metric_val += eval_metric(cur_actual, cur_pred)
                metric_count += 1.0
        else:
            for c in xrange(prediction.shape[1]):
                cur_actual = np.array(np.array(test_y) == c).astype(int)
                metric_val += eval_metric(cur_actual, prediction[:, c])
                metric_count += 1.0
        if metric_type == "sum":
			metric_count = 1.0
        return metric_val / metric_count


def print_stages(test_y, stage_predictions, test_metric, metric_type, test_weight):
    if hasattr(custom_metrics, test_metric):
        eval_metric = getattr(custom_metrics, test_metric)
    elif hasattr(ml_metrics, test_metric):
        eval_metric = getattr(ml_metrics, test_metric)
    else:
        eval_metric = getattr(sk_metrics, test_metric)
    if test_weight is not None:
        metric_args = inspect.getargspec(eval_metric)[0]
        if 'weight' in metric_args:
            eval_metric_orig = eval_metric
            eval_metric = lambda act, pred: eval_metric_orig(act, pred, test_weight)
    count = 0
    iters = []
    loss = []
    count_factor = 50
    for prediction in stage_predictions:
        count += 1
        if count in [1, 10, 50] or count % count_factor == 0:
            iters.append(count)
            loss.append(avg_eval_metric(eval_metric, test_y, prediction, metric_type=metric_type))
        if count > 1000:
            count_factor = 500
        elif count > 500:
            count_factor = 200
        elif count > 250:
            count_factor = 100
    loss_df = pd.DataFrame({'Iteration': iters, 'Loss': loss})
    loss_df.rename(columns={'Loss': test_metric}, inplace=True)
    pd.set_option('max_columns', len(loss_df.columns))
    pd.set_option('max_rows', len(loss_df))
    print("Loss:")
    print(loss_df)


def load_model_bin(model_file):
    model_file = open(model_file, 'rb')
    return Pickle.load(model_file)


def save_model_bin(model, model_file):
    model_file = open(model_file, 'wb')
    Pickle.dump(model, model_file, Pickle.HIGHEST_PROTOCOL)


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Train and predict data using some sklearn algorithms.',
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('-train_data_file',
                        #default='../data/output-py/gbr_01/gbr_01_k_1_tr.csv',
                        required=True,
                        type=str,
                        help='CSV with training data.')

    parser.add_argument('-test_data_file',
                        #default='../data/output-py/gbr_01/gbr_01_k_1_test.csv',
                        required=True,
                        type=str,
                        help='CSV with testing data.')

    parser.add_argument('-test_data_file2',
                        default=None,
                        type=str,
                        help='CSV with testing data.')

    parser.add_argument('-test_pred_file',
                        #default='../data/output-py/gbr_01/gbr_01_k_1_test_pred.csv',
                        required=True,
                        type=str,
                        help='Path to output testing predictions.')

    parser.add_argument('-test_pred_file2',
                        default=None,
                        type=str,
                        help='Path to output testing predictions.')

    parser.add_argument('-test_metric',
                        #default='normalized_weighted_gini',
                        required=True,
                        type=str,
                        help='Metric to compute on test set. Any metric on ml_metrics or sklearn.metrics')

    parser.add_argument('-target_col',
                        #default='target',
                        required=True,
                        type=str,
                        help='Name of target variable.')

    parser.add_argument('-weight_col',
                        #default='weight',
                        default=None,
                        type=str,
                        help='Name of weight column.')

    parser.add_argument('-metric_type',
                        default='auto',
                        type=str,
                        help='Type of metric to evaluate.',
                        choices=[
                            "auto",
                            "cumulative",
                            "sum"])

    parser.add_argument('-model_type',
                        #default='GradientBoostingRegressor',
                        required=True,
                        type=str,
                        help='Type of model to fit.',
                        choices=["RandomForestRegressor",
                                 "RandomForestClassifier",
                                 "ExtraTreesRegressor",
                                 "ExtraTreesClassifier",
                                 "GradientBoostingRegressor",
                                 "GradientBoostingClassifier",
                                 "LogisticRegression",
                                 "SVC",
                                 "Pipeline"])

    parser.add_argument('-model_file',
                        #default='../data/output-py/gbr_01/gbr_01_k_1_tr.csv.pkl',
                        required=True,
                        type=str,
                        help='File to save the model to.')

    parser.add_argument('-na_fill_value',
                        default=-20000,
                        type=int,
                        help='Value to fill in NAs.')

    parser.add_argument('-skip_mapping',
                        #default=True,
                        default=False,
                        action='store_true',
                        help='Skip na filling and category mapping.')

    parser.add_argument('-fit_args',
                        # default='{\"n_estimators\": 10, \"learning_rate\": 0.001,  \"loss\": \"ls\",  '
                        #         '\"max_features\": 5, \"max_depth\": 7,  \"random_state\": 788954,  '
                        #         '\"subsample\": 1, \"verbose\": 50}',
                        required=True,
                        type=str,
                        help='String in dictionary form of fit params.')

    parser.add_argument('-silent',
                        default=False,
                        action='store_true',
                        help="Don't print execution information.")

    parser.add_argument('-train_filter',
                        default='{}',
                        type=str,
                        help="Don't print execution information.")

    parser.add_argument('-load_model',
                        default=False,
                        action='store_true',
                        help="Loads saved model if exists.")

    parser.add_argument('-load_type',
                        default='fit_more',
                        type=str,
                        help='Type of model loading',
                        choices=[
                            "fit_more",
                            "pred_at"])

    parser.add_argument('-bootstrap',
                        default=0,
                        type=float,
                        help="Do bootstrap sampling.")

    parser.add_argument('-bootstrap_seed',
                        default=None,
                        type=int,
                        help='Bootstrap seed.')

    args = vars(parser.parse_args())

    args['train_filter'] = ast.literal_eval(args['train_filter'])
    args['fit_args'] = ast.literal_eval(args['fit_args'])
    for key in args['fit_args']:
        if isinstance(args['fit_args'][key], six.string_types):
            if args['fit_args'][key] in args:
                args['fit_args'][key] = args[args['fit_args'][key]]

    if not args['silent']:
        print(args)

    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", category=Warning)
        train_and_predict(**args)
