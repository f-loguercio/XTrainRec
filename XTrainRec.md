XTrainRec
================
Federico Loguercio
3/19/2019

Design of a recommendation engine for gym exercises. Collaborative filtering, association rule and popularity based recommenders are explored and combined into ensembles.

### Dataset creation

Generate simulated dataset

``` r
nRatings <- 40000
nItems <- 300
nUsers <- 4000

# Add external data for exercise names
ex_names <- read.csv('https://gist.githubusercontent.com/skorecky/7721583/raw/a6367c0f61a0a57561646942ba8fba315fcf9b6e/Exercises', sep = ",", header = F)

# Subset the exercise names to keep a random 300 of them
ex_names <- ex_names[sample(nrow(ex_names), 300), ]

# Binary feedback
# Create a two-column dataset with user and item IDs
gym_matrix <- data.frame(
  user=sample(nUsers, nRatings, replace=T),
  item=ex_names[sample(nItems, nRatings, replace = T), ] # sample from exercise names
)

# Delete unnecessary column
gym_matrix$item.V2 <- NULL

# Convert it to binaryRatingMatrix
gym_matrix <- as(gym_matrix, "binaryRatingMatrix")
```

##### Users dataset:

``` r
set.seed = 1
user_id <- 1:nUsers
df <- as.data.frame(user_id)

# Add simulated data for personal information
df$age <- sample(16:70, nUsers, replace = T)
df$weight <- sample(40:120, nUsers, replace = T)
df$gender <- sample.int(3, nUsers, replace = T) #male/female/other
df$strength <- sample(10:200, nUsers, replace = T)

hist(df$strength)
```

![](gym_recommender_files/figure-markdown_github/unnamed-chunk-2-1.png)

##### Item (exercise) dataset:

``` r
set.seed = 1
item_id <- 1:300
df_item <- as.data.frame(item_id)

# Randomly sample 300 of these exercises
ex_names <- ex_names[sample(nrow(ex_names), 300), ]

# Assign exercise names and bodypart names to the item dataframe
df_item$name <- data.frame(lapply(ex_names['V1'], as.character), stringsAsFactors=FALSE)
df_item$bodypart <- ex_names['V2']
```

### Create popularity-based recommendation engine

``` r
recc <- NA
# Leave out the last 5 users for testing
recc <- Recommender(gym_matrix[1:3995,], method="POPULAR")

# Define function to output recommendations using the above recommender
apply_popular <- function(data) {
  pre <- predict(recc, data, n = 5)
  return(recs <<- as(pre, "list"))
}

# Run prediction
apply_popular(gym_matrix[3996:4000,])
recs
```

    ## $`3996`
    ## [1] "atlas stone trainer"                     
    ## [2] "ankle on the knee"                       
    ## [3] "palms-up barbell wrist curl over a bench"
    ## [4] "bent over one-arm long bar row"          
    ## [5] "cable hammer curls - rope attachment"    
    ## 
    ## $`3997`
    ## [1] "atlas stone trainer"                     
    ## [2] "ankle on the knee"                       
    ## [3] "palms-up barbell wrist curl over a bench"
    ## [4] "bent over one-arm long bar row"          
    ## [5] "cable hammer curls - rope attachment"    
    ## 
    ## $`3998`
    ## [1] "atlas stone trainer"                     
    ## [2] "palms-up barbell wrist curl over a bench"
    ## [3] "bent over one-arm long bar row"          
    ## [4] "cable hammer curls - rope attachment"    
    ## [5] "dumbbell floor press"                    
    ## 
    ## $`3999`
    ## [1] "atlas stone trainer"                     
    ## [2] "ankle on the knee"                       
    ## [3] "palms-up barbell wrist curl over a bench"
    ## [4] "bent over one-arm long bar row"          
    ## [5] "cable hammer curls - rope attachment"    
    ## 
    ## $`4000`
    ## [1] "atlas stone trainer"                     
    ## [2] "ankle on the knee"                       
    ## [3] "palms-up barbell wrist curl over a bench"
    ## [4] "bent over one-arm long bar row"          
    ## [5] "cable hammer curls - rope attachment"

### Association Rules Based Recommender

I am unsure whether this is working correctly and lift is being recognised as the measure

``` r
ar_rec <- Recommender(gym_matrix[1:3995], method = "AR",
                              parameter=list(support = 0.001, conf = 0.1, sort_measure = 'lift', maxlen = 3))

apply_AR <- function(data) {
  pre <- predict(ar_rec, data, n = 5)
  return(recs_AR <<- as(pre, "list"))
}

apply_AR(gym_matrix[3996:4000])
recs_AR
```

    ## $`3996`
    ## [1] "seated leg tucks"
    ## 
    ## $`3997`
    ## [1] "kneeling hip flexor"       "cable tuck reverse crunch"
    ## [3] "ankle on the knee"        
    ## 
    ## $`3998`
    ## [1] "ankle on the knee"   "atlas stone trainer"
    ## 
    ## $`3999`
    ## [1] "front cable raise"   "atlas stone trainer"
    ## 
    ## $`4000`
    ## [1] "atlas stone trainer"

### User Based Collaborative Filtering

``` r
recUBCFCosine <- Recommender(gym_matrix[1:3995], method = "UBCF", 
                     param=list(method="Cosine", nn=20))

apply_UCBF <- function(data) {
  pre <- predict(recUBCFCosine, data, n = 5)
  return(recs_UCBF <<- as(pre, "list"))
}

apply_UCBF(gym_matrix[3996:4000])
recs_UCBF
```

    ## $`3996`
    ## [1] "standing inner-biceps curl"       "seated one-arm cable pulley rows"
    ## [3] "barbell shrug behind the back"    "single-leg high box squat"       
    ## [5] "front squat (clean grip)"        
    ## 
    ## $`3997`
    ## [1] "alternate incline dumbbell curl" "dips - triceps version"         
    ## [3] "muscle snatch"                   "one arm dumbbell bench press"   
    ## [5] "narrow stance squats"           
    ## 
    ## $`3998`
    ## [1] "goblet squat"             "bodyweight walking lunge"
    ## [3] "dumbbell floor press"     "single-cone sprint drill"
    ## [5] "incline cable flye"      
    ## 
    ## $`3999`
    ## [1] "close-grip standing barbell curl" "smith machine calf raise"        
    ## [3] "push-ups with feet elevated"      "wide-grip standing barbell curl" 
    ## [5] "butt-ups"                        
    ## 
    ## $`4000`
    ## [1] "weighted sit-ups - with bands"   "cuban press"                    
    ## [3] "lunge pass through"              "isometric neck exercise - sides"
    ## [5] "jm press"

### Item Based Collaborative Filtering

``` r
recIBCFJaccard <- Recommender(gym_matrix[1:3995], method = "IBCF", parameter = list(method = "Jaccard", k = 50))

apply_ICBF <- function(data) {
  pre <- predict(recIBCFJaccard, data, n = 5)
  return(recs_ICBF <<- as(pre, "list"))
}

apply_ICBF(gym_matrix[3996:4000])
recs_ICBF
```

    ## $`3996`
    ## [1] "full range-of-motion lat pulldown" "chin-up"                          
    ## [3] "vertical swing"                    "standing low-pulley deltoid raise"
    ## [5] "superman"                         
    ## 
    ## $`3997`
    ## [1] "leg press"                      "plyo kettlebell pushups"       
    ## [3] "standing pelvic tilt"           "one arm dumbbell preacher curl"
    ## [5] "push-ups with feet elevated"   
    ## 
    ## $`3998`
    ## [1] "close-grip standing barbell curl" "standing cable chest press"      
    ## [3] "standing dumbbell upright row"    "alternating cable shoulder press"
    ## [5] "bodyweight walking lunge"        
    ## 
    ## $`3999`
    ## [1] "heavy bag thrust"              "incline bench pull"           
    ## [3] "clean and press"               "front cable raise"            
    ## [5] "lying high bench barbell curl"
    ## 
    ## $`4000`
    ## [1] "cuban press"                        
    ## [2] "standing olympic plate hand squeeze"
    ## [3] "standing dumbbell calf raise"       
    ## [4] "external rotation with band"        
    ## [5] "push-ups with feet elevated"

### Create weighted ensemble recommender using UBCF and IBCF

``` r
recsys1 <- Recommender(gym_matrix[1:3995], method = "UBCF", 
                     param=list(method='Cosine', nn=20))
recsys2 <- Recommender(gym_matrix[1:3995], method = "IBCF", 
                     param=list(method='Jaccard', k = 50))
combined <- HybridRecommender(recsys1, recsys2, weights = c(0.60, 0.40))

apply_ensemble_w <- function(data) {
  pre <- predict(combined, data, n = 5)
  return(recs_ensemble_w <<- as(pre, "list"))
}

apply_ensemble_w(gym_matrix[3996:4000])
recs_ensemble_w
```

    ## $`3996`
    ## [1] "standing inner-biceps curl"       "seated one-arm cable pulley rows"
    ## [3] "barbell shrug behind the back"    "single-leg high box squat"       
    ## [5] "ankle on the knee"               
    ## 
    ## $`3997`
    ## [1] "alternate incline dumbbell curl" "dips - triceps version"         
    ## [3] "muscle snatch"                   "one arm dumbbell bench press"   
    ## [5] "narrow stance squats"           
    ## 
    ## $`3998`
    ## [1] "goblet squat"             "bodyweight walking lunge"
    ## [3] "dumbbell floor press"     "single-cone sprint drill"
    ## [5] "incline cable flye"      
    ## 
    ## $`3999`
    ## [1] "close-grip standing barbell curl" "smith machine calf raise"        
    ## [3] "push-ups with feet elevated"      "butt-ups"                        
    ## [5] "wide-grip standing barbell curl" 
    ## 
    ## $`4000`
    ## [1] "cuban press"                     "weighted sit-ups - with bands"  
    ## [3] "band hip adductions"             "lunge pass through"             
    ## [5] "isometric neck exercise - sides"

### Create ensemble over experience

The recommender will adapt recommendations based on strength of user

``` r
ensemble_predictor <- function(matrix, pred_range, user_data, var, cutoff){
  recs_ensemble = list() # initiate list for recommendations
  
  # For each user passed to the function, I check whether the user's strength exceeds
  # the cutoff. If it does not, I append a popularity-based recommendation to the
  # list, otherwise a UBCF one.
  for(i in pred_range){
    if(user_data[i, var] <= cutoff){
      recs_ensemble = append(recs_ensemble, as(predict(recc, matrix[i], n=5), 'list'))
    }else{
      recs_ensemble = append(recs_ensemble, as(predict(recUBCFCosine, matrix[i], type="topNList", n = 5), 'list'))
    }
  }
  return(recs_ensemble)
}
```

``` r
ensemble_predictor(gym_matrix, 3995:4000, df, 'strength', 40)
```

    ## $`3995`
    ## [1] "reverse plate curls"            "rope crunch"                   
    ## [3] "one-arm kettlebell swings"      "bear crawl sled drags"         
    ## [5] "standing dumbbell reverse curl"
    ## 
    ## $`3996`
    ## [1] "standing inner-biceps curl"       "seated one-arm cable pulley rows"
    ## [3] "barbell shrug behind the back"    "single-leg high box squat"       
    ## [5] "front squat (clean grip)"        
    ## 
    ## $`3997`
    ## [1] "alternate incline dumbbell curl" "dips - triceps version"         
    ## [3] "muscle snatch"                   "one arm dumbbell bench press"   
    ## [5] "narrow stance squats"           
    ## 
    ## $`3998`
    ## [1] "goblet squat"             "bodyweight walking lunge"
    ## [3] "dumbbell floor press"     "single-cone sprint drill"
    ## [5] "incline cable flye"      
    ## 
    ## $`3999`
    ## [1] "close-grip standing barbell curl" "smith machine calf raise"        
    ## [3] "push-ups with feet elevated"      "wide-grip standing barbell curl" 
    ## [5] "butt-ups"                        
    ## 
    ## $`4000`
    ## [1] "weighted sit-ups - with bands"   "cuban press"                    
    ## [3] "lunge pass through"              "isometric neck exercise - sides"
    ## [5] "jm press"
