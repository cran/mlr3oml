
# mlr3oml

[![tic](https://github.com/mlr-org/mlr3oml/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3oml/actions?query=workflow%3Atic)
[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3oml)](https://cran.r-project.org/package=mlr3oml)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)

**mlr3oml** allows to create [mlr3](https://mlr3.mlr-org.com/) tasks
directly from [OpenML](https://openml.org) data sets. Furthermore, you
can also obtain the data and the resampling for a given OpenML task.
Caching can be enabled by setting the option `"mlr3oml.cache"`.
Uploading to OpenML is currently not supported, use the [OpenML
package](https://cran.r-project.org/package=OpenML) package for this.

## Short Demo

``` r
library("mlr3")
library("mlr3oml")

# new parametrized task "oml"
tsk("oml", data_id = 31)
```

    ## INFO  [20:55:12.445] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/31}
    ## INFO  [20:55:12.836] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/features/31}
    ## INFO  [20:55:17.364] Downloading ARFF {url: https://www.openml.org/data/v1/download/31/credit-g.arff}

    ## <TaskClassif:credit-g> (1000 x 21)
    ## * Target: class
    ## * Properties: twoclass
    ## * Features (20):
    ##   - fct (13): checking_status, credit_history, employment,
    ##     foreign_worker, housing, job, other_parties, other_payment_plans,
    ##     own_telephone, personal_status, property_magnitude, purpose,
    ##     savings_status
    ##   - int (7): age, credit_amount, duration, existing_credits,
    ##     installment_commitment, num_dependents, residence_since

``` r
tsk("oml", task_id = 59)
```

    ## INFO  [20:55:17.579] Retrieving JSON {url: https://www.openml.org/api/v1/json/task/59}
    ## INFO  [20:55:17.778] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/61}
    ## INFO  [20:55:17.838] Downloading ARFF {url: https://www.openml.org/data/v1/download/61/iris.arff}

    ## <TaskClassif:Task 59: iris (Supervised Classification)> (150 x 5)
    ## * Target: class
    ## * Properties: multiclass
    ## * Features (4):
    ##   - dbl (4): petallength, petalwidth, sepallength, sepalwidth

``` r
# same for resampling
rsmp("oml", task_id = 59)
```

    ## INFO  [20:55:17.907] Retrieving JSON {url: https://www.openml.org/api/v1/json/task/59}
    ## INFO  [20:55:17.971] Downloading ARFF {url: https://www.openml.org/api_splits/get/59/Task_59_splits.arff}
    ## INFO  [20:55:18.071] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/61}
    ## INFO  [20:55:18.134] Downloading ARFF {url: https://www.openml.org/data/v1/download/61/iris.arff}

    ## <ResamplingCustom> with 10 iterations
    ## * Instantiated: TRUE
    ## * Parameters: list()

``` r
# R6 class for data sets
oml_data = OMLData$new(61)
oml_data$name
```

    ## INFO  [20:55:18.224] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/61}

    ## [1] "iris"

``` r
oml_data$nrow
```

    ## INFO  [20:55:18.296] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/qualities/61}

    ## [1] 150

``` r
oml_data$ncol
```

    ## INFO  [20:55:18.382] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/features/61}

    ## [1] 5

``` r
oml_data$data
```

    ## INFO  [20:55:22.961] Downloading ARFF {url: https://www.openml.org/data/v1/download/61/iris.arff}

    ##      sepallength sepalwidth petallength petalwidth          class
    ##   1:         5.1        3.5         1.4        0.2    Iris-setosa
    ##   2:         4.9        3.0         1.4        0.2    Iris-setosa
    ##   3:         4.7        3.2         1.3        0.2    Iris-setosa
    ##   4:         4.6        3.1         1.5        0.2    Iris-setosa
    ##   5:         5.0        3.6         1.4        0.2    Iris-setosa
    ##  ---                                                             
    ## 146:         6.7        3.0         5.2        2.3 Iris-virginica
    ## 147:         6.3        2.5         5.0        1.9 Iris-virginica
    ## 148:         6.5        3.0         5.2        2.0 Iris-virginica
    ## 149:         6.2        3.4         5.4        2.3 Iris-virginica
    ## 150:         5.9        3.0         5.1        1.8 Iris-virginica

``` r
# R6 class for tasks
oml_task = OMLTask$new(31)
oml_task$name
```

    ## INFO  [20:55:23.049] Retrieving JSON {url: https://www.openml.org/api/v1/json/task/31}

    ## [1] "Task 31: credit-g (Supervised Classification)"

``` r
oml_task$nrow
```

    ## INFO  [20:55:23.122] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/qualities/31}

    ## [1] 1000

``` r
oml_task$ncol
```

    ## INFO  [20:55:23.204] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/features/31}
    ## INFO  [20:55:27.769] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/31}

    ## [1] 21

``` r
oml_task$task
```

    ## INFO  [20:55:27.841] Downloading ARFF {url: https://www.openml.org/data/v1/download/31/credit-g.arff}

    ## <TaskClassif:Task 31: credit-g (Supervised Classification)> (1000 x 21)
    ## * Target: class
    ## * Properties: twoclass
    ## * Features (20):
    ##   - fct (13): checking_status, credit_history, employment,
    ##     foreign_worker, housing, job, other_parties, other_payment_plans,
    ##     own_telephone, personal_status, property_magnitude, purpose,
    ##     savings_status
    ##   - int (7): age, credit_amount, duration, existing_credits,
    ##     installment_commitment, num_dependents, residence_since

``` r
oml_task$resampling
```

    ## INFO  [20:55:27.990] Downloading ARFF {url: https://www.openml.org/api_splits/get/31/Task_31_splits.arff}

    ## <ResamplingCustom> with 10 iterations
    ## * Instantiated: TRUE
    ## * Parameters: list()

``` r
# list oml data sets with 5 features and 50 - 200 instances
tab = list_oml_data_sets(number_features = 5, number_instances = c(50, 200))
```

    ## INFO  [20:55:28.113] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/list/number_instances/50..200/number_features/5/limit/1000}
    ## INFO  [20:55:28.463] Retrieving JSON {url: https://www.openml.org/api/v1/json/data/list/number_instances/50..200/number_features/5/limit/1000/offset/1000}

``` r
head(tab[, .(data_id, name)])
```

    ##    data_id               name
    ## 1:      61               iris
    ## 2:     199           fruitfly
    ## 3:     214           baskball
    ## 4:     329         hayes-roth
    ## 5:     346               aids
    ## 6:     668 witmer_census_1980

``` r
# list first 10 oml tasks
tab = list_oml_tasks(limit = 10)
```

    ## INFO  [20:55:28.708] Retrieving JSON {url: https://www.openml.org/api/v1/json/task/list/limit/10}

``` r
tab[, .(task_id, data_id, name)]
```

    ##     task_id data_id            name
    ##  1:       2       2          anneal
    ##  2:       3       3        kr-vs-kp
    ##  3:       4       4           labor
    ##  4:       5       5      arrhythmia
    ##  5:       6       6          letter
    ##  6:       7       7       audiology
    ##  7:       8       8 liver-disorders
    ##  8:       9       9           autos
    ##  9:      10      10           lymph
    ## 10:      11      11   balance-scale
