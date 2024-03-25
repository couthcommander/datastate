# Step 0: install package

```{r}
remotes::install_github('couthcommander/datastate', upgrade = 'never')
library(datastate)
```

# Step 1: have data

```{r}
Hmisc::getHdata(titanic3)
t3 <- titanic3
t3[[1]] <- c('first','second','third')[match(t3[[1]], c('1st','2nd','3rd'))]
```

# Step 2: create data dictionary/specification
```{r}
dd <- datadict(t3)
```

# Step 3: alter specification

```{r}
dd1 <- makeFactor(dd, t3, 'pclass')
dd1 <- addRecode(dd1, t3, vars = 'survived')
myfile <- paste0(tempfile(), '.yaml')
dd2yaml(dd1, file = myfile)
sprintf('make changes to %s', myfile)
# directly edit file to do something like this
#dd1$variables[['pclass']]$factor[[1]]$label_output <- '1st'
#dd1$variables[['pclass']]$factor[[2]]$label_output <- '2nd'
#dd1$variables[['pclass']]$factor[[3]]$label_output <- '3rd'
#dd1$variables[['survived']]$recode[[1]]$newvalue <- 'dead'
#dd1$variables[['survived']]$recode[[2]]$newvalue <- 'alive'
#dd1$variables[['cabin']]$factor <- NULL
#dd1$variables[['boat']]$factor <- NULL
```

# Step 4: transform data with specification

```{r}
upd <- yaml2dd(myfile)
out <- mod(upd)
```
