# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\21 Machine Learning")

# Supervised Learning
# -> use tree, randomForest package to implement bagged decision trees and random forests
# -> calculate probabilities using a naive-Bayes model

# Unsupervised Learning
# use kmeans() function with specified number of clusters
# use kmeans() function with specified inital cluster centres
# calculate Euclidean distance
# Investigate number of clusters to use
# Perform PCA for dimensionality reduction

install.packages("MASS")
library(MASS)

# Creating a Training Dataset
# sample() function -> sample(vecor of values to sample from, number of values to sample, prob=vector of probs)

# We will use the Boston Dataset
set.seed(1)
training.rows = sample(1:nrow(Boston), nrow(Boston)/2) # sampling without replacement
# Here its easy to see we sample the row indices from Boston dataset -> 1:nrow(Boston) meaning all rows from the data set
# -> The number of indices to sample is nrow(Boston)/2 -> randomly sample these rows
# -> The prob argument is default equally likely -> equal chance of selecting a row index from Boston
# -> Sample function samples without replacement unless you specify replacement=TRUE
# -> As you can see, we sample from a vector c(1,2,3,..., nrow(Boston)) and the number from the sample we want is nrow(Boston)/2
# -> These will be the row indices we will use to train the tree

head(training.rows)
# Will produce the top of vector row indices
# -> with these indices, we can build the training set

training.data = Boston[training.rows, ]
head(training.data)

# We now consider various supervised learning approaches to predict medv -> based on other columns

# Constructing a Decision Tree
# A decision tree is a series of questions that we ask a data point about the input variables to form a prediction
# -> A decision tree using this data to predict medv from Boston -> see notes

# We can check the mean of medv values for suburbs in the training data meeting the conditions in the tree ->
mean(training.data$medv[training.data$rm < 6.9595 & training.data$lstat < 14.405 & training.data$rm >=6.543])

# To construct a decision tree, use tree package in R

install.packages("tree")
library(tree)

# Use the tree() function -> tree(output variable ~ (tilda) input variables, data = data set)
# Output variable needs to either be a numeric vector or a factor -> the function produces
# a regression or classification tree

# To include all the input vaiables, use a ~. (tilda full stop) in the function

boston_tree = tree(medv ~., data = training.data)
boston_tree

# deviance -> alternative to Gini impurity score
# yval -> the predicted output value for points at that node -> only interest in this at terminal nodes indicated with * at the end of the line
# -> Look at the output from boston_tree to see what going on

# Indenting of the nodes indicates the parent / child relationships -> nodes 4 and 5 are children of node 2

mean(training.data$medv[training.data$rm < 6.9595 & training.data$lstat < 14.405 & training.data$rm<6.543])
# Check this at node 4 split at rm < 6.543
# -> 21.37748

# Plotting a Decision Tree
plot(boston_tree) # -> this just plots the paths...the text() function adds the text
text(boston_tree, cex = 1.2, pos = 1, offset = 0.7)

# Retrieving the fitted values of the Training Data
# -> Use the output: frame ->
boston_tree$frame

# The node numbers are given as the row labels on LHS of the output

rownames(boston_tree$frame)[boston_tree$where[1]] # -> Will give us the first data point from the training set

boston_tree$frame$yval[boston_tree$where[1]] # -> Will give us the predicted value

# Use the predict() function to extract all predicted values
pred.vals = predict(boston_tree)
head(pred.vals)

# Alternatively, use boston_tree$frame$yval[boston_tree$where]
pred.vals2 = boston_tree$frame$yval[boston_tree$where]

# These numbers above are from the training.data set...what about predicting new data?

# Predicted values for new data ->
# We can use the predict() function to predict mdev for unseen data points
# Remember that we split the dataset into training and remaining...lets use the remaining for testing ->

testing.data = Boston[-training.rows, ]
head(testing.data)

test.preds = predict(boston_tree, newdata = testing.data) # -> be careful here...use newdata = ...
head(test.preds)

# We can predict the value of medv for any given input values...doesn't have to come from the Dataset
# -> Construct a data frame with input variables as columns, and the set of points we wish to predict medv
# -> Set the newdata argumnet in predict() to this data frame...seems simple enough

data = data.frame(crim = 0.03, zn = 0, indus = 5, chas = 0, nox = 0.47, rm = 6.5, age = 45.2, dis = 5.4, rad = 2, tax = 289, ptratio = 15.8, black = 395.21, lstat = 8.51)
# -> this is a row data input...see all the inputs here are what we used in the tree construction...we want to predict medv from ~.

predict(boston_tree, newdata = data) # -> 21.37748 

# For a classifcation tree -> default ouput of the predict() are the probabilities for each ouput classifcation
# -> For A CLASSFICATION TREE -> DEFAULT OUTPUT of the predict() are PROBABILITIES for each OUTPUT CLASSFICATION
# -> setting type = "class" in the predict()...outputs the class with highest probabiity (with ties split randomly) -> useful and will probably come up

# For comparing actual vs. predicted for a classication tree -> use table()
# -> table(actual values, predicted values)

plot(testing.data$medv, test.preds, main = "Predicted vs. observed median house value of owner-occupied homes ($000's) across Boston suburbs using a decision tree, with y = x comparison line",
ylab = "Predicted values", xlab = "Actual values")
abline(0,1,col="red")

# We can calculate the MSE for predictions ->
mean((testing.data$medv - test.preds)^2) # -> 35.28688
# Use this as a comparison of performance of different models on the test data set

# We can also compare the mean value of the points in each node with the predicted value to see how the tree performs on average
# use the type = "where" in the predict()

test.nodes = predict(boston_tree, newdata = testing.data, type = "where")

# Can also calculate the average medv value across each of these nodes using aggregate()
avg.vals = aggregate(medv ~ test.nodes, data = testing.data, FUN = mean)
avg.vals

terminal.preds = boston_tree$frame$yval[avg.vals$test.nodes]
terminal.preds

# Plot the predicted values against the average medv in the test data for each node ->
plot(avg.vals$medv, terminal.preds, main = "Predicted vs. average median house value of owner-occupied homes ($000's) in each terminal node with y = x comparison line",
ylab = "Terminal node predicted values", xlab = "Average value in terminal node")
abline(0,1,col="blue")

# Bagged Decision trees
# We can generally improve the performs by constructing several decison trees -> instead of just one
# -> Take the predicted value for each point as the average predicited value across the trees

# For a classification problem, we can take the predicted class to be the most predicted across the tress

# We use the method of bootstrapping -> repeated sampling 

# -> Randomly selct points to use from the training data WITH replacement
# -> For any tree, the same point can be used more than once in the data used to train it

# To create a Bootstrapped sample, we turn to the sample() function
# Remember how sameple() works -> sample(vector of prob, replacement = TRUE, size = size, prob = vector to sample from)

bag.rows = sample(1:nrow(training.data), replace=TRUE) # -> sample frm c(1,2,3,..., nrow(training.data)) and you sample with replacement! meaning the same row can appear in the sample
bag = training.data[bag.rows, ]
# We repeat this as many times as desired -> randomly sampling rows from the training.data

# For results to be reproducible -> set.seed()

# Instead of manually creatin the samples, use the randomForest()
install.packages("randomForest")
library(randomForest)

set.seed(1)
training.rows = sample(1:nrow(Boston), nrow(Boston)/2, replace=TRUE)
training.data = Boston[training.rows,]

# Use randomForest() ->
# randomForest(output ~ input, data = dataset, mtry = variables to try at each split, ntree = number of trees)

# mtry -> how many of the input variables the algo tries at each split point
# For constructing a set of purely bagged decision trees (rather than RF), we try all of the input variables -> ~. and mtry = 13
# -> why mtry = 13? There are 13 input variables -> 13 inputs, 1 output: medv
# ntree = number of trees -> default is 500

boston_bag = randomForest(medv ~., data = training.data, mtry = 13) # -> Important to remember how to construct these
head(boston_bag$predicted) # -> looking at the predicted component

# Alternatively, use predict()
testing.data = Boston[-training.rows,]
test.preds.bag = predict(boston_bag, newdata = testing.data) # -> we can use the testing data from before as we've not used it for training

plot(testing.data$medv, test.preds.bag, main = "Predicted vs. observed median house value of owner-occupied homes ($000's)
 across Boston suburbs using bagged decision trees, with y = x comparison line",ylab = "Predicted values", xlab = "Actual values") 
 # Plot above is the testing data versus the prediction of the test data -> test.preds.bag = predict(boston_bag, newdata = testing.data)
 abline(0, 1, col = "red")
 
 # There is a much better correspondence of predictions from the the bagged decison tree than on a single tree
 # Better correspondence between predicted and observed values when averaging over multiple predictions
 
 # We can also look a the MSE ->
 mean((test.pred.bag - testing.data$medv)^2)
 # -> You'll find that the MSE from bagged decision trees are much lower than MSE from a single Tree
 
 # Random forests
 # A better appraoch than bagged decision trees are considering random subsets of the variables at each potential split points ->
 # Setting mtry io somethin gless than total number of input variables
 # The default of mtry depends on whether we are constructing:
 # -> Regression for whch the regression tree is the integer part of d/3 -> d number of input variables -> for mtry
 # -> Classification for which the classification tree is the integer part of sqrt(d) -> mtry

# randomForest() performs random selections, get a different answer each time we run it -> set a seed

set.seed(1)
training.rows = sample(1:nrow(Boston), nrow(Boston)/2, replace=TRUE)
training.data = Boston[training.rows,]
boston_bag = randomForest(medv ~., data = training.data, mtry = 13)
boston_rf = randomForest(medv ~., data = training.data) # See -> we excluded the mtry argument for teh random forest

test.preds.rf = predict(boston_rf, newdata = testing.data)

plot(testing.data$medv, test.preds.rf, main = "Predicted vs. observed median house value of owner-occupied homes ($000's) 
 across Boston suburbs using random forest trees, with y = x comparison line",ylab = "Predicted values", xlab = "Actual values")
abline(0, 1, col = "green")

# The graph should look similar to the bagged decison predicted versus test data medv's...
# The noticbale feature of this graph - >looks even less spread out around the green line -> y = x
# -> Suggesting further improvement when using randomForest()

# MSE
mean((testing.data$medv - test.preds.rf)^2)
# Using randomForest() has further reduced the MSE

# Naive Bays Classifcation Theory
# This is an approach to classication problems ->
# Under naive Bayes classification, we can express the output variable y taking a value -> say 1 conditional on values
# of d inputs ->

# P(y=1 | x1,x2,x3,...,xd) -> P(x1|yi=1)*P(x2|yi=1)*P(x3|yi=1)...*P(xd|yi=1) / P(x1,x2,x3,...,xd)

# This is assuming that the values of the covariates are conditionally independent give the output variable
# It is generally easier to only calculate the numerator on RHS ->
# Write: P(y=1 |...) proporational to P(x1|yi=1)*P(x2|yi=1)*P(x3|yi=1)...*P(xd|yi=1) / P(x1,x2,x3,...,xd)

# For a given set for the inputs, we can work out the RHS of this statement for each value of the output variable
# We can use these to then back out the probabilities of the form P(y=1|x1,x2,...,xd) which is what we want

# Naive Bayes Classification Example

whisky = read.csv("whisky.csv")

table(whisky$whisky.name) # -> Check how many different whiskies there are
# From the table, tasters are providing their assessment on (Smoky, Fruity, Colour) to ID the whisky
# For a type of whisky, there are differing assessments to classify the type

# table() is quite handy...will use it again later on
# Identified that there are 4 different type of whisky

# Lets say we want to estimate the probabilities of each whisky type for a particular whisky
# -> lets also assume that each of the three characteristics scored a 2 -> Smoky = 2, Fruit = 2, Colour = 2
# In other words, we want to estimate:

# P(Dogvulin | S=2, F=2, C=2)
# P(Glenragh | S=2, F=2, C=2)
# P(Mactavish | S=2, F=2, C=2)
# P(Western Isle | S=2, F=2, C=2)

# Lets focus on P(Dogvulin | S=2, F=2, C=2)...will work this out and then apply to the rest

# Form naive Bayes -> P(Dogvulin | S=2, F=2, C=2) proportional to P(S=2|D)*P(F=2|D)*P(C=2|D) * P(Dogavulin) DONT FORGET THE UNCONDITIONAL PROBABILITY
# Note the conditional independence -> we're able to multiply the conditonal probabilities

d = whisky[whisky$whisky.name == "Dogavulin", ] # -> extract all Dogavulin from whisky
nrow(d)

# To estimate P(S=2|D) -> we just extracted all D -> now a matter of emperical frequency
# P(S=2|D) ->
nrow(d[d$smoky == 2,])/nrow(d)
# -> estimate P(S=2|D) = 0.09

# use table to calculate all smoky scores ->
p_s_d = table(d$smoky)/nrow(d) # -> very cool
# p_s_d -> Proability of S given Dogavulin -> For different scoring on Smoky, we know P(S=s|D) -> S={1,2,3} scores

# Do this for the rest of whisky types ->

p_f_d = table(d$fruity) / nrow(d)
p_c_d = table(d$colour) / nrow(d)

# So from the p_s_d, p_f_d, p_c_d for {s,f,c} = 2 ->
# P(S=2|D) = 0.09
# P(F=2|D) = 0.79
# P(C=2|D) = 0.755

# Finally we need the P(Dogavulin) ->
p_wh = table(whisky$whisky.name)/nrow(whisky) # Awesome function -> table()

# P(Dogavulin) = 0.2 

# Finally we can calculate -> P(S=2|D)*P(F=2|D)*P(C=2|D) * P(Dogavulin) which is proportional to P(Dogvulin | S=2, F=2, C=2)
# -> P(S=2|D)*P(F=2|D)*P(C=2|D) * P(Dogavulin)

p_s_d[2] * p_f_d[2] * p_c_d[2] * p_wh[1]
# or
p_s_d["2"] * p_f_d["2"] * p_c_d["2"] * p_wh["Dogavulin"] # -> Recommended to do it this way because knowledge of the index may not be always known

# P(Dogvulin | S=2, F=2, C=2) is not equal to what we just calculated, but rather PROPORTIONAL to it
# Before we can calculate the actual value, we must repeat the method for the other whisky types ->

g = whisky[whisky$whisky.name == "Glenragh", ]
p_s_g = table(g$smoky)/nrow(g)
p_f_g = table(g$fruity)/nrow(g)
p_c_g = table(g$colour)/nrow(g)

m = whisky[whisky$whisky.name == "Mactavish", ]
p_s_m = table(m$smoky)/nrow(m)
p_f_m = table(m$fruity)/nrow(m)
p_c_m = table(m$colour)/nrow(m)

w = whisky[whisky$whisky.name == "Western_Isle", ]
p_s_w = table(w$smoky)/nrow(w)
p_f_w = table(w$fruity)/nrow(w)
p_c_w = table(w$colour)/nrow(w)

vals = c(p_s_d["2"] * p_f_d["2"] * p_c_d["2"] * p_wh["Dogavulin"],
		 p_s_g["2"] * p_f_g["2"] * p_c_g["2"] * p_wh["Glenragh"],
		 p_s_m["2"] * p_f_m["2"] * p_c_m["2"] * p_wh["Mactavish"],
		 p_s_w["2"] * p_f_w["2"] * p_c_w["2"] * p_wh["Western_Isle"])

# vals carry the names of "2" -> change it to names of the whisky
names(vals) = c("Dogavulin", "Glenragh", "Mactavish", "Western Isle")

# Remember that vals are the numerator on the RHS of the naieve Bayes equation
# Turn vals into probabilities that sum to 1 ->

# We use the fact that:
# P(D|S=2, F=2, C=2) + 
# P(G|S=2, F=2, C=2) + 
# P(M|S=2, F=2, C=2) + 
# P(WI|S=2, F=2, C=2)  = 1

probs = vals/sum(vals)
probs

# We see that Dogavulin has the highest probability for S=2, F=2, C=2
# So assuming misclassification costs, across the whiskies, we would predict an unkown whisky with a score of 2 in each characteristic -> Dogavulin

# If we are only interested in the prediction, there was no need to scale (although good for correctness)
# Why no need to scale? -> the whisky with the largest value in vals will also have the largest PROBABILITY

# Naive Bayes Classification using Generic Functions -> 
# We can generalise what we did above in simpler steps

nb.prob = function(data, y_col, y_val, x_col, x_val){
	data.y = data[data[, y_col] == y_val, ] # -> Here is the given part i.e. given D (category value)
 	data.yx = data.y[data.y[, x_col] == x_val, ] # -> Here is where we calculate the probability 
	nrow(data.yx) / nrow(data.y)
	}
	
nb.prob(whisky, "whisky.name", "Dogavulin", "smoky", 2) # -> P(S=2|D) -> nrow(d[d$smoky == 2,])/nrow(d)

nb.probs = function(data, y_col, y_val, x_col){
	data.y = data[data[, y_col] == y_val, ] # -> Here is the given part i.e. given D (category value)
	table(data.y[, x_col]) / nrow(data.y) # -> Here we calculate the probabilities for all the input scores {1,2,3}
	}

nb.probs(whisky, "whisky.name", "Dogavulin", "smoky") # -> p_s_d

# Try other classification with different scores



#########################################################################################

# Unsupervised Learning - K Means 

# K-means with specified number of parameters
# K-means is a clustering algo -> takes a set of points and attempts to identify distinct clusters containing similar points

# Using the faithful dataset -> do a simple split to visualize

plot(faithful,xlab = "Eruption time (minutes)", ylab = "waiting time to next eruption (minutes)", main = "Graph of waiting time to next eruption against eruption time for Old Faithful", pch = 20)

# Perform K-means clustering using kmeans()
# But first we need to rescale the vairbales to ensure that the distance calculations are not dominated by the waiting times

faithful.stand = scale(faithful) # -> Remember we MUST STANDARDISE!
# -> this standardises the the columns in faithful -> (x-xbar)/sd(x) -> get columd with sd = 1
# scale() also centeres the variables around 0 by subtracting the mean

set.seed(272)
faithful.clusters = kmeans(faithful.stand, 2) # -> here we have 2 because we think there are two clusters

str(faithful.clusters)

as.numeric(faithful.clusters$centers)
faithful.clusters$centers[2]

# The algo has returned two clusters of sizes 98 and 174
# -> the cluster means (centroids of each cluster)
# -> the cluster vector -> allocated cluster for each data point
# -> the within cluster sum of squares -> intepret this on fit
# -> the ratio of the between-cluster sum of squares to the total sum of squares...meausre is simlar to R^2 in LR -> explains proportio of variance explained
# -> avaialable components extracted using $ notation

# Cluster centres
faithful.clusters$clusters

# However the clusters are based on scaled data -> need to UNDO the scaling
# A method to use is calc the means of each column of the orignial data for subsets based on the cluster -> colMeans()

# For cluster 1 -> this will give us the centre of the cluster
colMeans(faithful[faithful.clusters$cluster == 1, ]) # Dont forget about the ,] -> you need the rows from faithful assigned to cluster 1

# Similarly for cluster 2 -> this will give us the centre of the cluster
colMeans(faithful[faithful.clusters$cluster == 2, ]) # Dont forget about the ,] -> you need the rows from faithful assigned to cluster 1

# If we wanted both results ->
centers = data.frame(
	Cluster1 = colMeans(faithful[faithful.clusters$cluster == 1, ]),
	Cluster2 = colMeans(faithful[faithful.clusters$cluster == 2, ]))

centers

# In summary...how we got these numbers? We averaged using original data where the rows used come from the clusters identified in the kmeans()

# R has returned the cluster centres as columns
# If we wanted the centres as rows instead of columns -> transpose
t(centers) # -> this is much better to read off

# Plotting the clusters -> same plot from before BUT adding col = faithful.cluster
plot(faithful,xlab = "Eruption time (minutes)", ylab = "waiting time to next eruption (minutes)", main = "Graph of waiting time to next eruption against eruption time for Old Faithful",
	pch = 20, col = faithful.clusters$cluster)
legend("topleft", legend = c("Cluster 1", "Cluster 2"),col = c(1, 2),lwd = 2)
# The col argument is set to a vector 1 and vector 2 i.e. cluster 1 and cluster 2
points(t(centers), cex = 5, pch = 3, lwd = 4, col = "blue") # -> add the cluster centres using points() and t(centres) to match the axes
# Remember we undid the scaling to get the centres! Dont forget about it because you can't use it otherwise in analysis


# Describing the Clusters
# Output the centroids as a reminder of where the clusters are centred ->
faithful.clusters$centers # -> is extracted from $ notation -> you will see the bottom of the output what can be taken
# Dont forget, these are SCALED values

# Remember that we scaled / normalized the data -> each column has a mean = 0 and a sd = 1
# -> A Positive average value for one of the columns indicates this cluster represents points that are overall above average for that variable
# As the scaled columns all have unit variance, the maginitude of the numbers across the columns gives an indication of how far from the overall average the centroid is for that variable

# We'v identified two clusters...Cluster 1 and CLuster 2
# Cluster 1 has many fewer points than Cluster 2 (98 vs 174) -> roughly 1:2
# Appears to be 1/3 of the time much lower eruption time and waiting time on average

# Consider the centroids of the clusters -> here we look at centeres that we unscaled ->

# Gives an idea on the magnitude of the differences between clusters in original units
# We can see that the eruptions in Cluster 1 is 2.24 mins shorter than eruptions in Cluster 2...etc

# K-means with proposed initial centres
# Lets say it was suggested to rerun algo with initial cluster centres (2.2, 80) and (4.5, 55)
# Looking at the plots before...seems whoever suggested these points had the y values mixed up
# We can reun the algo based on these initial centres...see what happens

# First we will need to scale the eruptions and times in faithful to reflect these given centres -> standardise

erup.centres = (c(2.2, 4.5) - mean(faithful$eruptions))/sd(faithful$eruptions) # -> We standardise the initial centres...we need a SCALED starting point -> kmeans() takes scaled values
wait.centres = (c(80, 55) - mean(faithful$waiting))/sd(faithful$waiting) # -> We standardise the initial centres...we need a SCALED starting point -> kmeans() takes scaled values

init.centres = data.frame(erup = erup.centres, wait = wait.centres) # -> 2x2 data frame with one row per clusterm columns representing centroid co-ordinates

# We can now re-run kmeans() ->
faithful.clusters2 = kmeans(faithful.stand, init.centres) # -> here we specifiy the INITIAL STANDARDISED CENTRES and we still use our STANDARDISED faithful dataset
faithful.clusters2

faithful.clusters2$cluster

faithful.clusters$centers
faithful.clusters2$centers
# -> So even though the given initial centres didn't look sensisble, the algo converged on the same clusters as before...as expected

centres.2 = data.frame(
	Cluster1 = colMeans(faithful[faithful.clusters2$cluster == 1, ]),
	Cluster2 = colMeans(faithful[faithful.clusters2$cluster == 2, ]))

# -> We do this so that the rows represent the cluster, and the columns the coordinates of the cluster centroid

centres.2 = t(centres.2)
centres = t(centres)

# Check the unscaled centroids -> form initial centres, converged to the centres we had before
# So it does appear that are two distinct clusters in this data set

# Calculating Euclidean Distance
# We manually calculate the Euclidean distance from the ith point to the jth cluster centre

# sqrt((Wi - Wj_bar)^2  + (Ei - Ej_bar)^2)
# Where Wi is the waiting time at the ith point, Ei the eruption time of the ith point
# -> Wj_bar -> average waiting time of points in cluster j
# -> Ej_bar -> average waiting time of points in cluster j

euc.dist = function(x1,x2){
	sqrt(sum((x1-x2)^2))
	}
	
euc.dist(faithful[1, ], centres[, 1])

# Use apply(matrix, MARGIN, function) -> where we apply the function to rows of the matrix (if MARGIN = 1 -> rows)
# If MARGIN = 2 -> columns of the matrix

dists1 = apply(faithful, 1, function(row){euc.dist(row, centres[, 1])}) # -> careful with centres[, 1] -> this isn't transposed 
# -> so the cluster is the column name and rows have the (x,y) coordinates
head(dists1)

# Do the same thing for the second cluster
dists2 = apply(faithful, 1, function(row){euc.dist(row, centres[, 2])})# -> careful with centres[, 1] -> this isn't transposed 
# -> so the cluster is the column name and rows have the (x,y) coordinates
head(dists2)

# apply() is quite useful -> apply(data, MARGIN, function(){the function code})

# We can now create a vector that indicates which cluster centre each point is closest to -> use ifelse()
cluster = ifelse(dists1 < dists2, 1, 2)
head(cluster)
# -> We can see that the first data point in faithful -> the first row -> is closest to the second cluster centre

# Lets see if the output aligns with the cluster allocations from kmeans() 
which(cluster != faithful.clusters$cluster)

# IMPORTANT -> If we used the unscaled data, there is a chance that we get mismatches
# -> Rather best to SCALE the data, and run the DISTANCE measurments on SCALED data
# -> we see that there are 4 mismatches -> This happened becaue so far we have calculated distances to each cluster centre using UNSCALED data
# -> When kmeans() does it, it uses the scaled data _> IMPORTANT...kmeans() uses SCALED DATA

# We can recalculate our distances based on the SCALED data ->

dists1.scaled = apply(faithful.stand, 1, function(row){euc.dist(row, faithful.clusters$center[1, ])})
head(dists1.scaled)

dists2.scaled = apply(faithful.stand, 1, function(row){euc.dist(row, faithful.clusters$center[2, ])})
head(dists2.scaled)
 
# We can then create a vector indicating which cluster each is closest to -> check whether there are any differnces to the output by kmeans()
cluster.scaled = ifelse(dists1.scaled < dists2.scaled, 1, 2)
which(cluster.scaled != faithful.clusters$cluster) # -> named integer(0) -> indicates no mismathches

# Sum of Squares
# Recall the two outputs of the kmeans()
# -> WITHIN cluster sum of squares by cluster
# -> the ratio of the between-cluster sum of squares and the total sum of squares -> like an R^2

# The WITHIN cluster SS for a particular cluster gives the sum of squared distances from each point to cluster centre -> WITHIN GROUP SS -> sum of squared distances
# from each  point to cluster centre
# sum(i=1, nj)[(Wij - Wj_bar)^2 + (Eij - Ej_bar)^2] -> SS of all the points in Cluster j

# Where:
# Wij is the waiting time of the ith point in Cluster j
# Eij is the eruption time of the ith point in Cluster j
# Wj_bar is the average waiting time of points in Cluster j
# Ej_bar is the average eruption time points in Cluster j
# sqrt[(Wij - Wj_bar)^2 + (Eij-Ej_bar)^2)]
# and nj is the number of points in Cluster j

# sum(i=1, nj)sqrt([(Wij - Wj_bar)^2 + (Eij - Ej_bar)^2])^2
# -> sum(i=1, nj)[Euclidean Distances]^2

# This is a measure of within-cluster homogeneity -> the smaller these figures the more tightly packed each is

# The ratio of the between-cluster SS to the total SS is a measure of cluster heterogeneity -> the between-cluster SS ->
# sum(j=1, k)[nj * (sqrt((Wj_bar - W_bar)^2 +(Ej_bar -E_bar)^2))^2] -> sum (nj * [sqrt((Wj_bar - Wbar)^2 + (Ej_bar - E_bar)^2)^2] -> BEWTWEEN CLUSTER SS
# Where:
# -> W_bar is the average waiting time of ALL POINTS
# -> E_bar is the average eruption time of ALL POINTS
# -> Wj_bar, Ej_bar and nj are defined as above
# -> k is the number of clusters 

# The larger it is, the further apart the clusters are
# The TOTAL SS ->
# sum(j=1, k)[sum(i=1, nj)[(sqrt((Wij - W_bar)^2 + (Eij - E_bar)^2))^2] -> See here that we use the global W_bar, and E_bar
# This is quite cool -> the between SS as a ratio of the total SS is like an R^2 -> Total SS = Within SS + Between SS

# The total SS is the sum of the within-cluster SS and between-cluster SS
# The ratio of the between cluster SS to the total SS is a measure that describes how much of the total SS has been explained
# by different clusters...similar to R^2 -> REMEMBER -> The ratio of BETWEEN CLUSTER SS to TOTAL SS -> measure of how much total SS has been explained by different clusters
# IMPORTANT -> The closer the ratio is to 1, the greater the heterogeneity (BECAUSE BETWEEN CLUSTER SS is a MEASURE OF CLUSTER HETEROGENEITY between clusters, 
# and the greater the homogeneity within clusters
# We can change the ratio by considering different numbers of clusters -> hyperparameter
# We can force the ratio to equal 1 exactly by having each individual point as its own cluster of size 1 -> not a useful thing to do
# Similar to inflating R^2 in linear regression by continually adding more and more covariates without considering their significance

# Investigating different numbers of clusters
# A method to try is select an appropriate number of clusters -> identify a number after which there is no longer a significant improvement
# -> in total within-group SS IMPORTANT
# -> When adding a nother cluster, we look at the Within Cluster SS -> By improve -> does it get smaller at a quicker rate?
# Use a plot of total within SS -> kmean() -> we can extract this info after adding additonal cluster
# What does this mean...Remember we must consider the WITHIN_GROUP SS...adding another cluster...does the WITHIN_GROUP SS improve?

# We can calculate this for different numbers of clusters say 1 to 10...
# Plot the result ->

tot.withinss = numeric(10)

for (i in 1:10){
	set.seed(6)
	tot.withinss[i] = kmeans(scale(faithful), i)$tot.withinss # -> DONT FORGET TO SCALE THE DATA
	}
	
tot.withinss

plot(1:10, tot.withinss, main = "Plot of total within-group sum of squares against cluster count for k-means clustering on movie data",
 xlab = "cluster count",ylab = "total within-group sum of squares", type = "b")
 
# Here we see a sharp drop from 1 cluster to 2 cluster assignment -> thereafter (after 2 clusters) it dcreases but at much lower rate
# So 2 clusters seems sensibile
# # Think how this will be different with higher dimensional data sets

# Principle Component analysis
# Useful for pre-processing high dimensional data
# Reduces dimentionality -> retain only the first few principals
# can then perform K-means on the selected principal components

# Perform PCA using prcomp() ->
pca = prcomp(trees, scale.=TRUE) # -> PCA should be performed on data that is centred around 0 -> prcomp does this automatically i.e. SCALED DATA -> prcomp does it automatically
str(pca) # -> useful for extraction

# The $center and $scale components gives the means and standard deviations of each column
# Values of x_bar and sd are the vaues of $ center and $ scaled
# Usefule check ->

pca$center
colMeans(trees)
# Should give the same result

pca$scale
apply(trees, 2, sd) # -> note how this works: apply(data, MARGIN = 2 for columns, function())
# Should give the same result

# the command $x gives the the component of pca that contains teh corordinates of each points on each of the new principal components axes
# -> after rotating that is

head(pca$x)

# To go back to the original data, we rotate this matrix ($x) by the inverse of the rotation matrix -> to rotate back
# -> The INVERSE of the Rotation matrix is its TRANSPOSE

recons = pca$x%*%t(pca$rotation) # -> where t(pca$rotation) = inv(rotation) -> in R the inverse of a matrix: solve(matrix)
head(recons) # -> youll see it doesnt look like trees data -> because its centred and scaled! You then need to undo the normalization

head(recons)
head(scale(trees)) # -> should match the recons 

# We use PCA for dimensionality reduction -> if there are linear relationships between the variables in the original data, we should
# be able to represent (at least somewhat accurately) the original data using only a subset of the principal components
# -> lower dimensionality without losing too much overall information

# To decide on what principal components to keep is to consider their standard deviations and variances -> use summary()

summary(pca)
# -> First line gives us the standard deviation for each principal component
# -> The second line indicates the PROPORTION of the OVERALL VARIATION in the original data set that is CAPTURED by each PRINCIPAL component
# -> The first PC captures 80.34% of the variation in the data -> This will decrease as we look at additional PCs
# -> The third line is the cumulative proportion of the variation captured as we include more and more PCs
# -> Essentailly the cumsum of the Proportion of Variances

# Example: we may wish to capture at least 99% of the variation in the data set -> Therefore need to keep the first two PCs
# -> We can reconstruct the original (centred and scaled) data using the first <n> PCs as follows ->

n=2 # -> Reconstruct the original dataset using the first two PCs
pca$x[ ,1:n]%*%t(pca$rotation[, 1:n])

# Reconstrucint trees using first two PCs ->
recons.2 = pca$x[ ,1:n]%*%t(pca$rotation[, 1:n]) # -> Remember that these values are still SCALED

# Plot fo the original data
pairs(scale(trees), main = "Plot of centred and scaled diameter (labelled girth), height and volume of 31 black cherry trees")

# Plot fo the original data
pairs(recons.2, main = "Plot of reconstructed centred and scaled diameter (labelled girth), height and volume of 31 black cherry trees from 2 principal components")

# Unscale recons.2
means = colMeans(trees)
std.dev = apply(trees, 2, sd)

means = matrix(c(rep(means, nrow(recons.2))), nrow=nrow(recons.2), byrow=TRUE)
std.dev=matrix(c(rep(std.dev, nrow(recons.2))), nrow=nrow(recons.2), byrow=TRUE)

trees.2 = recons.2*std.dev + means

head(trees.2); head(trees)

#########################################################################################
# Question 1

set.seed(19)
training.rows = sample(1:nrow(iris), 0.6*nrow(iris))
training.data = iris[training.rows, ]
head(training.data, 10)

library(tree)

iris_tree = tree(Species ~., data = training.data)
iris_tree
# -> We're building a Classfication Tree -> Remember that we get probabaility outputs -> 

plot(iris_tree)
text(iris_tree, cex = 1.2, pos = 1, offset = 0.7)

pred.data = data.frame(Sepal.Length = 5, Sepal.Width = 3.7, Petal.Length = 2.5, Petal.Width = 0.3)
predict(iris_tree, newdata = pred.data, type="class")
# -> versicolor  with prob 0.8

test.data = iris[-training.rows, ]
head(test.data)

pred.test = predict(iris_tree, newdata = test.data, type="class") # -> IMPORATANT! Include type="class" for classfication tree
head(pred.test)

table(test.data$Species)
table(pred.test)
table(test.data$Species, pred.test)
# -> see one incorrect prediction -> Look independent tables or table( , )

set.seed(7233)
iris_rf = randomForest(Species ~., data = training.data, mtry = 2, ntree = 1000)
# -> If you wanted bagged  decision trees -> don't include mtry -> IMPORTANT

pred.data = data.frame(Sepal.Length = 5, Sepal.Width = 3.7, Petal.Length = 2.5, Petal.Width = 0.3)
predict(iris_rf, newdata = pred.data, type = "class")
# -> setosa  with prob 1 -> the decison tree and RF are giving different predictions!

# test.date remains the same -> test.data
pred.test.iris = predict(iris_rf, newdata = test.data, type = "class")
table(test.data$Species); table(pred.test.iris) # -> not useful so rather use table( , )
table(test.data$Species, pred.test.iris)
# -> see that RF misclassifying 2 flower types

# To calculate the accuracy  of predicting the test data set for each method ->
# Sum the diagonal elements of each table (this will give the total of correct predictions)
# -> Take the sum of the diagonal elements and DIVIDE it by the number of test data points ->

# For the single decison tree
sum(diag(table(test.data$Species, pred.test))) / nrow(test.data)
# -> 0.9833333 accurate reflecting one misclassification

# For the Random Forrest with mtry = 2
sum(diag(table(test.data$Species, pred.test.iris))) / nrow(test.data)
# -> 0.9666667 accurate reflecting two misclassifications

# Usually we would expec teh Random Forest to outperform the single decision tree
# -> Could jsut be the case that the single decision tree peforms well on this specific data set

#########################################################################################
# Question 2

whisky = read.csv("whisky.csv")

nb.prob = function(data, y_col, y_val, x_col){
	data.y = data[data[, y_col] == y_val, ] # -> Here is the given part i.e. given D (category value)
	table(data.y[, x_col]) / nrow(data.y) # -> Here we calculate the probabilities for all the input scores {1,2,3}
	}

set.seed(842231)
training.rows = sample(1:nrow(whisky), 0.6*nrow(whisky))
training.data = whisky[training.rows, ]
head(training.data)

nb.prob(training.data, "whisky.name", "Mactavish", "smoky")
# table -> need to convert to data frame

str(nb.prob(training.data, "whisky.name", "Mactavish", "smoky"))
# -> table

# use.as.data.frame()

smoky = as.data.frame(nb.prob(training.data, "whisky.name", "Mactavish", "smoky"))
colnames(smoky) = c("Value", "Mactavish")
# -> This sets up the first column in the data frame
# Now for other whisky names -> Dogavulin, Glenragh, Western_Isle
smoky$Dogavulin = nb.prob(training.data, "whisky.name", "Dogavulin", "smoky")
smoky$Glenragh = nb.prob(training.data, "whisky.name", "Glenragh", "smoky")
smoky$Western_Isle = nb.prob(training.data, "whisky.name", "Western_Isle", "smoky")

# Now for the other types -> fruity, and Colour

fruity = as.data.frame(nb.prob(training.data, "whisky.name", "Mactavish", "fruity"))
colnames(fruity) = c("Value", "Mactavish")
# -> This sets up the first column in the data frame
fruity$Dogavulin = nb.prob(training.data, "whisky.name", "Dogavulin", "fruity")
fruity$Glenragh = nb.prob(training.data, "whisky.name", "Glenragh", "fruity")
fruity$Western_Isle = nb.prob(training.data, "whisky.name", "Western_Isle", "fruity")

colour = as.data.frame(nb.prob(training.data, "whisky.name", "Mactavish", "colour"))
colnames(colour) = c("Value", "Mactavish")
# -> This sets up the first column in the data frame
colour$Dogavulin = nb.prob(training.data, "whisky.name", "Dogavulin", "colour")
colour$Glenragh = nb.prob(training.data, "whisky.name", "Glenragh", "colour")
colour$Western_Isle = nb.prob(training.data, "whisky.name", "Western_Isle", "colour")

prob.w = table(training.data$whisky.name) / length(training.data$whisky.name)
sum(prob.w) # -> Check that it sums to 1


prop.to = function(whisky_name, smoky_val, fruity_val, colour_val){
	prob.w[whisky_name] * smoky[smoky_val, whisky_name] * fruity[fruity_val, whisky_name] * colour[colour_val, whisky_name]
	}

prop.to("Mactavish", training.data$smoky, training.data$fruity, training.data$colour)

test.data = whisky[-training.rows, ]
head(test.data)

preds = data.frame(ID = 1:nrow(test.data))

for (y in names(prob.w)){
	preds[, y] = prop.to(y, test.data$smoky, test.data$fruity, test.data$colour)
	}

head(preds) # -> Rememeber this is PROPORTIONAL TO

# Take the max of the row -> get the corresponding whisky name ->

preds$prediction = names(preds)[-1][max.col(preds[, -1])] # -> use [-1] to exclude the first column -> the ID column
# -> see whats going on in here...the max.col gives a position in the colums excluding the first column ***
# -> then you want names(preds) -> exlcude the first colummn which is the ID column by [-1] and then use [max.col...] to give the position in names(preds)
# -> Exhausting :(

# Comparing actual versus predicted ->

PAT = table(test.data$whisky.name, preds$prediction) # -> we use the test.data classifications versus what was predicted as before

# To get the accuracy -> sum(diag(PAT)) / nrow(test.data)
sum(diag(PAT)) / nrow(test.data)
# 85.25% accurate


#########################################################################################
# Question 3

install.packages("rpart.plot")
library(rpart.plot); data(ptitanic)

head(ptitanic)

set.seed(10)
train.rows = sample(1:nrow(ptitanic), 0.5*nrow(ptitanic))
train.data = ptitanic[train.rows, ]
head(train.data,4)

set.seed(6047)
train.rf = randomForest(survived ~ pclass + sex + sibsp + parch, data = train.data, ntree=1500)

test.data = ptitanic[-train.rows,]
pred = predict(train.rf, newdata = test.data)

surv.status = table(pred)/length(pred) # -> Survival status

CM = table(test.data$survived, pred)
TP = CM[2,2]
FN = CM[2,1]
FP = CM[1,2]
TN = CM[1,1]

Prec = TP/(TP+FP)
Recall = TP/(TP+FN)
Acc = (TP+TN)/(TP + FN + FP + TN)
F1 = 2*Prec*Recall/(Prec + Recall)
FPRate = FP/(FP + TN)

Prec; Recall; F1; FPRate; Acc

# In R use -> CM[2:1, 2:1] to swap the rows and columns

nb.prob = function(data, y_col, y_val, x_col, x_val){
	data.y = data[data[, y_col] == y_val, ] # -> Here is the given part i.e. given D (category value)
 	data.yx = data.y[data.y[, x_col] == x_val, ] # -> Here is where we calculate the probability 
	nrow(data.yx) / nrow(data.y)
	}

nb.prob(train.data, "survived", "survived", "sibsp", 0)
nb.prob(train.data, "survived", "died", "sibsp", 0)

prob.S = table(train.data$survived) / nrow(train.data)

prop.to = function(data, s_status, sex_val, sibsp_val, parch_val, pclass_val){
		prob.S[s_status] *
		nb.prob(data, "survived", s_status, "sex", sex_val) * 
		nb.prob(data, "survived", s_status, "sibsp", sibsp_val) * 
		nb.prob(data, "survived", s_status, "parch", parch_val) * 
		nb.prob(data, "survived", s_status, "pclass", pclass_val)
		}

test.data = ptitanic[-train.rows,]

preds = data.frame(ID = 1:nrow(test.data))

preds$surv = apply(test.data, 1, function(row){prop.to(train.data, "survived", row["sex"], row["sibsp"], row["parch"], row["pclass"])})
preds$died = apply(test.data, 1, function(row){prop.to(train.data, "died", row["sex"], row["sibsp"], row["parch"], row["pclass"])})

head(preds)

preds$pred = ifelse(preds$surv < preds$died, "died", "survived")
head(preds)

nb.surv.status = table(preds$pred)
nb.CM = table(test.data$survived, preds$pred)
nb.CM = nb.CM[2:1, 2:1]

TP = nb.CM[1,1]
FN = nb.CM[1,2]
FP = nb.CM[2,1]
TN = nb.CM[2,2]

nb.Prec = TP/(TP+FP)
nb.Recall = TP/(TP+FN)
nb.Acc = (TP+TN)/(TP + FN + FP + TN)
nb.F1 = 2*Prec*Recall/(Prec + Recall)
nb.FPRate = FP/(FP + TN)

# Naive Bayes Predictions
nb.Prec; nb.Recall; nb.F1; nb.FPRate; nb.Acc

# Random Forest Predictions
Prec; Recall; F1; FPRate; Acc

# Naive Bayes model has a higher Recall -> out of those that survived, the NB model correclty predicted survival for of these passengers
# RF has a higher  precision comapred to NB -> out of those predicted to survive, the RF got a higher proportion correct
# Overall the F1 scores are Similar
# The RF has a lower FP rate -> RF does a better job at NOT CLASSIFYING passengers who died as surviving
# Accuracies are vey similar -> both models have an overall similar proportion of correct predictions

# Overall both models are very similar -> If these metris accuracy reflect their performance on out-of-sample data, then which
# may be more useful depend on the importance of the metrics

#########################################################################################
# Question 4

movie.data = read.csv("movie.data.csv")
head(movie.data)

centers = sapply(1:3, function(i){colMeans(movie.data[movie.data$Cluster == i, 1:5])}) 
# Quick and easy -> movie.data[movie.data$Cluster == 1,2,3, 1:5 columns to exclude the Cluster columns]
# We take the column means for each genre in each cluster

colnames(centers) = paste("centers.",1:3)

# To calculate the distances from each point to the center of the cluster -> 
# -> sqrt((H-H1)^2 + (R-R1)^2...+(F-F1)^2) 
# -> where H1, R1,.., F1 are the centers of cluster 1

euc.dist = function(x1,x2){
	sqrt(sum((x1-x2)^2))
	}
	
movie.data$dist1 = apply(movie.data, 1, function(row){euc.dist(row[1:5], centers[, "centers. 1"])})
movie.data$dist2 = apply(movie.data, 1, function(row){euc.dist(row[1:5], centers[, "centers. 2"])})
movie.data$dist3 = apply(movie.data, 1, function(row){euc.dist(row[1:5], centers[, "centers. 3"])})

head(movie.data)

# Incorrectly allocated point
# For each row, check if hte point is allocated to the cluster corresponding to the SMALLEST DISTANCE
# Create a new column that indicates the correct updated cluster
# Then use which.min() function -> we want to find the smallest distance for all row(points) in Dataset
# -> USe sapply() to apply the which.min() to each set of distances

movie.data$updated = apply(movie.data[ ,c("dist1", "dist2", "dist3")], 1, which.min) 
# -> The which.min() looks at the distances 1,2,3  and looks for the minimum of the three
# -> The kmeans() algo assigns a point to a cluster based on the mininum distance between that point and the cluster centre
# -> Here amongst the 3 distances, the minimum of the the 3 distances will indicate which cluster i.e. the index of position
# -> c("dist1", "dist2", "dist3"3) -> positional index {1,2,3}

# To identify the point misallocated ->
movie.data[movie.data$updated != movie.data$Cluster, ]
# -> We see it should be allocated to Cluster 1 but was allocated to Cluster 3
movie.data[264,6] = 1

new.centers = sapply(1:3, function(i){colMeans(movie.data[movie.data$Cluster == i, 1:5])}) 
colnames(new.centers) = paste("centers. ", 1:3, sep="")
new.centers

# To verify that the algo has concerged -> calculate the distance from each point again to the cluster center ->

movie.data$dist1_new = apply(movie.data, 1, function(row){euc.dist(row[1:5], new.centers[, "centers. 1"])})
movie.data$dist2_new = apply(movie.data, 1, function(row){euc.dist(row[1:5], new.centers[, "centers. 2"])})
movie.data$dist3_new = apply(movie.data, 1, function(row){euc.dist(row[1:5], new.centers[, "centers. 3"])})

# We can now calulate the 'new' clsuter for each point using the which.min()
movie.data$New.cluster = apply(movie.data[ ,c("dist1_new", "dist2_new", "dist3_new")], 1, which.min)
movie.data[movie.data$New.cluster != movie.data$updated, ]

# Analyst realises the that the data should be normalized before using kmeans()

movie.data = read.csv("movie.data.csv")
movie.data = movie.data[-6]
head(movie.data)

movie.data.scaled = scale(movie.data)

set.seed(6)
movie.cluster = kmeans(movie.data.scaled, 3)

unscaled.centers = data.frame(
	Cluster1 = colMeans(movie.data[movie.cluster$cluster == 1, ]),
	Cluster2 = colMeans(movie.data[movie.cluster$cluster == 2, ]),
	Cluster3 = colMeans(movie.data[movie.cluster$cluster == 3, ]))

# or
unscaled.centers = sapply(1:3, function(i){colMeans(movie.data[movie.cluster$cluster == i, 1:5])}) 

unscaled.centers; new.centers

# Cluster 1 points are the same in comparison -> Clusters 2,3 may have had some points switched around
# We don't know what distance metric the analyst used to minimise the distances between points and cluster centroids

# Cluster 1 prefers Horror and Action movies. Doesn't care much for Romcom or Comedies
# Cluster 2 prefers Comedey and Fantasy. Is undecided on whether they prefer action or Romcom. Definitely not a horror fan
# Cluster 3 prefers Action, Comedey and a bit of Horror. Not too keen on Fantasy and Romcoms

# Plotting the movie data set and showing the cluster centroids from K-means algo ->
plot(movie.data, col = movie.cluster$cluster, main = "Plot of average movie scores for five types of movies, coloured by k-means cluster")

# Want to consider other clusters  -> want to use total within SS metric to determine how many clusters to use

set.seed(6)
total.withinss = numeric(10)

for (j in 1:10){
	total.withinss[j] = kmeans(movie.data.scaled, j)$withinss
	}

plot(1:10, total.withinss, main = "Plot of total within-group sum of squares against cluster count for k-means clustering on movie data",
 xlab = "cluster count",ylab = "total within-group sum of squares", type = "b")
 
# Based on the plot, 3 clusters looks adequate. More than three clusters doesn't improve the total within SS very much ->
# It decrease but at a slow rate comapred to the decrease seen when 2 clusters are used. Adding a third cluster does show some
# -> improvement in total within SS but thereafter, the imrpovements are marginal for more than 3 clusters. Its not to say there isn't any improvement,
# -> jus that the improvement is smaller compared to the first three clusters
# -> Also the more clusters, more compuational time, more segmentation of the data -> may lose the heterogeneity among clusters


#########################################################################################

# Machine Learning 

# Supervised Learning ->
# Use tree package to construct a decision Tree
# Use randomForest pacakge to implement bagged decision trees and random forests
# Calculating probabities using a naive Bayes model

# Unsupervised Learning ->
# Using the kmeans() with specified number of clusters
# Using kmeans() with specified intial cluster centres
# Calcualting Euclidean distance
# Investigating the number of clusters to use
# Peform PCA

install.packages("MASS")
library(MASS)

#Boston data set
head(Boston)

# Create a training data set -> chose random rows from Boston as the index of what to sample for the training data set
# use sample() function -> sample(vector of values, number of values to sample, prob = vector of probabilities)

set.seed(1)
training.rows = sample(1:nrow(Boston), nrow(Boston)/2) # -> use half the Boston data set for Training
# Randomly select half of the rows in Boston 
# To recover the rest of Boston -> Boston[-training.rows, ]

head(training.rows) # -> just a bunch of numbers -> use as row indices from Boston when creating the taining dataset

# Creating the training data set ->
training.data = Boston[training.rows, ]
head(training.data)

# We will now consider various supervised learning approaches tto predict medv -> the median value based on other columns

# Constructing a Decision Tree
# A DT is a seroes of questions that we ask a data point about the input variables to form a predictio for the output
# Refer to diagram pg 6 in notes

# Given a point from the dataset i.e. say training.data[1, ] -> the first row...what will the tree predict?
# The DT in the diagram is a solved tree offering a prediction of the medv so follow the questions to work through the tree to get 27.73 based on the questions the tree asks

# The predicted medv value of 27.73 was determined in the following way -> Remember thaat
# The medv value of 27.73 is the MEAN of of medv values for suburbs in the training data meeting these conditions -> if we ask the series of questions to training.data there will be a 
# few rows that meet the conditions -> the best estimate of the prediction is the mean of the values! IMPORTANT!

mean(training.data$medv[training.data$rm < 6.9595 & training.data$lstat < 14.405 & training.data$rm >= 6.543]) # -> 27.72917

# To construct a DT -> use tree() function from tree package
install.packages("tree")
library(tree)

# Use tree() on training.data -> symbol ~. used here -> data = training.data -> we want to construct a predictor for medv from training.data but letting R know to exclude medv
boston_tree = tree(medv ~. , data = training.data)

# By default, a greedy approach with deviance is used as the loss function
# -> The loss function is the deviance (refer to pg 7 in notes on this)

boston_tree # ->
# The first line of the output dsecribes the information given on each subsequent line:
# -> node) ... the node index 1), 2), etc
# -> split ... how the node is constructed from the parent
# -> n ... the number of data points at that node
# -> deviance ... the loss function by which the tree has been constructed -> alternative to Gini impurity score
# -> yval ... the predicted output value for points ar that node -> only interested in this at the terminal nodes -> indicated with * at the end

# Indenting of nodes -> indicated the parent / child relationships
# You'll understand when you see an output
# Look at node 4 -> constains all the data point from node 2 when lstat < 14.405 ... 135 data points
# -> The deviance score is 1816 and the average value for the points at this node is 22.51

# We can confirm by calculating the mean ourselves based on the splits in the tree ->
mean(training.data$medv[training.data$rm < 6.9595 & training.data$lstat < 14.405]) # -> 22.50667

# Plotting a Decision Tree
plot(boston_tree) # -> not useful other than showing the split archietecture
# To add the text ->
plot(boston_tree)
text(boston_tree)
# text(boston_tree, cex = 1.2, pos =1, offset = 0.7)

# Fitted Values of Training Data
# Tree objects have a component called frame
# -> frame describes the structure of the tree similar to output above
# -> use frame to extract information 
str(boston_tree) # -> see $frame

boston_tree$frame # -> output
# -> The first row realted to the root node...the initial node containing all the data points
# -> It tells us that there are 253 points in that node and that the average value of mdev for all data points is 21.7856
# -> It also tells us how the child nodes are created from it -> variable split is on rm with left and right split criteria
# PLOT the tree with text -> much easier to understand what's going on!

# compare plot(boston_tree) with bston_tree$frame

# There where component of a tree object contains information relating to the terminal node for each point in training sample
# Ok...how it works ->
boston_tree$where[1] # -> tells us the first training row is the 505th row of the Boston Data set
# The output also shows 5 under 505 -> means that this row data point i.e. the first trainsing data point (which is the 505th row in Boston) is in the 5th row in boston_tree$frame :
# -> 9 <leaf> 24 256.4696 27.72917
# so the first row of the training data corresponds to node 9 in the tree! confusing
# To get the node information -> 
rownames(boston_tree$frame) # ->  "1"  "2"  "4"  "8"  "9"  "5"  "10" "20" "21" "11" "3"  "6"  "7"  ... ok these are the nodes -> use result of boston_tree$where[] to find node
rownames(boston_tree$frame)[boston_tree$where[1]] # -> "9"

# Then the predicted value of the first row of the training.data is ->
str(boston_tree$frame) # -> can see what else you can extract ...
boston_tree$frame$yval # -> these are the predicted node values -> check against tree plot output
# We want the the predicted value from the the first training.data row point ->
boston_tree$frame$yval[boston_tree$where[1]] # -> 27.72917

# To get general output info for the first row point ->
boston_tree$frame[boston_tree$where[1], ] # this gives the frame output of the 5th row ... because boston_tree$where[1] -> 5th row of frame (and 505th row of Boston dataset)

boston_tree$where # -> quite cool -> gives the index of Boston data set (training row) and where in frame it sit -> you'll see a long list of output but makes sense

# Extracting all the predicted values -> what are the values at the terminal nodes ->
pred.vals = boston_tree$frame$yval
# or...
pred.vals = boston_tree$frame$yval[boston_tree$where]

# easier to get the predicted values ->
pred.vals2 = predict(boston_tree) # -> will give the predicted values for all of the training set points
head(pred.vals2) # -> also gives the row in Boston where the training data came from

# Predicted values for NEW DATA
testing.data = Boston[-training.rows, ]
head(testing.data)

test.preds = predict(boston_tree, newdata = testing.data) # -> straight forward

# We can also predict the value of medv with data not seen before -> it doesn't have to only be in the training or test data sets
# For instance, we can create the following data point:

data = data.frame(crim = 0.03, zn = 0, indus = 5, chas = 0, nox = 0.47, rm = 6.5, age = 45.2, dis = 5.4, rad = 2, tax = 289, ptratio = 15.8, black = 395.21, lstat = 8.51)
# remember to match the column names properly
predict(boston_tree, newdata = data) # -> 21.37748

# For a classification tree, the fault output of the predict() function are probabilities for each possible output class
# -> Setting type = "class" in the predict function instead outputs the class with the highest probability (ties split randomly)

# To compare actual versus predicted for a classifcation tree -> use table()
# -> table(actual values, predicted values)
# Chapter questions discuss the use of classification trees

# For a numeric output variable -> can compare actual with predicted values using a plot -> Scatterplot! compare the actual versus predicted -> error in prediction -> MSE?
plot(testing.data$medv, test.preds, main = "Predicted vs. observed median house value of owner-occupied homes ($000's) across Boston suburbs using a decision tree, with y = x comparison line",
ylab = "Predicted values", xlab = "Actual values")
# Plot looks confusing but makes sense -> the terminal nodes in the tree are averages of the median values from the data points that are assinged there -> 
abline(0, 1, col = "red")

# We can calculate the MSE for these predictions as follows ->
mean((testing.data$medv - test.preds)^2) # -> MSE for the TESTING DATA! Use testing medv values (actual) compared to predicted (test.preds) # -> 35.28688

# We can use the MSE as a comparison of performance of different models on the test data set

# We can compar the mean value of the points in each node with the predicted value to see how the tree performs on Average
# Firstly identify the terminal node for each test point  by using 'where' ->
test.nodes = predict(boston_tree, newdata = testing.data, type = "where")
head(test.nodes) # -> 2  3  4  5  6  7 
				 #    4 12 12 12  4  4 

# we know the row now in $frame where the test rows are
# What we can do is average the medv values for these rows 
# -> calculate the average medv value across each of these nodes using aggregate()

avg.vals = aggregate(medv ~ test.nodes, data = testing.data, FUN  = mean) # -> ok so all testing row is assinged a location where -> group wheres to get the Average
# Aggregate function is working on test.nodes -> nodes are the terminal nodes of the prediction -> find average of the medvs that are assigned there
avg.vals

# We can extract what the predicted values are at the terminal nodes of prediction ->
# You just need to know what the terminal nodes are -> the predicted values are learnt from training.data and can't change -> if we send through new data -> make its way to a node and then predicton
# -> using the boston_tree object which is trained -> find the test nodes predicted values
terminal.preds = boston_tree$fram$yval[avg.vals$test.nodes] # -> we use the terminal nodes that the testing.data arrived at -> conveniently grouped in avg.vals
# -> 21.37748 27.72917 18.08667 14.42903 10.31538 33.42500 45.38000
 
# Compare this with avg.vals ->
plot(avg.vals$medv, terminal.preds)
abline(0, 1, col = "red")
 
# with labels
plot(avg.vals$medv, terminal.preds, main = "Predicted vs. averagemedian house value of owner-occupied homes ($000's) in each terminal node with y = x comparison line", ylab = "Terminal node predicted values",
xlab = "Average value in terminal node")
abline(0, 1, col = "red")

# So it appears that on average, the predictions are fairly close from the plot but...the prediction may be quite far from the true value for any particular point
# The graphs that has horizontals -> previous graph -> you just averaged the values on the horizontals and compared with predicted value at terminal node! 
# Very Cool :*) !

# Bagged Decision Trees ->
# Improve perfomance of general DT by constructing several DT instead of just one and taking the predicted value for each point as the average predicted value across the trees
# This is the idea behind Bagged DTs

# For a classification problem, we can take the predicted class to be the most predicted class across the trees

# Running tree() function over and over doesn't make a new tree unless the input data changes each time -> randomly select points to use from the training set WITH REPLACEMENT
# To create a bootstrapped sample from the training data -> use sample() with replacement = TRUE

bag.rows = sample(1:nrow(training.data), replace = TRUE) # Notice here we don't speficy a size ... R samples randomly for whole size of training.data with replacement
bag = training.data[bag.rows, ]

# We can repeat this above many times to get sub-samples of the training data for the bootstrapped samples
# -> this is randomly selecting rows from the training data -> get different output each time its run

# Ok we can write code to do that -> genrate sub-samples, built tree, get predictions, and Average

# Rather than manually doing that -> we can use randomForest() in the randomForest package
install.packages("randomForest")
library(randomForest)

set.seed(1)
training.rows = sample(1:nrow(Boston), nrow(Boston)/2, replace = TRUE)
training.data = Boston[training.rows, ]

# randomForest function -> randomForest(output variable ~ input variable, data = dataset, mtry = variable to try at each split, ntree = number of trees)
# For bagged DT -> we want to subsample from training.data but try all the variables at each split -> mtry = all vraibles to try

boston_bag = randomForest(medv ~. , data = training.data, mtry = 13) # ntree is set to 500 by default but you can change It
# -> mtry = 13 because we want to try all the variables at each split
head(boston_bag$predicted) # -> this is done using the sub-trees to predict the full training set using sub-samples and then averaging the result

predict(boston_tree) # -> show predicted values of the training data using one DT on full training.data
predict(boston_bag) # -> show predicted values from 500 DT on sub-sampled training.data

# We can use the predict() to get predictions on testing.data
testing.data = Boston[-training.rows, ]

test.preds.bag = predict(boston_bag, newdata = testing.data)

plot(testing.data$medv, test.preds.bag, 
	main = "Predicted vs. observed median house value of owner-occupied homes ($000's) across Boston suburbs using bagged decision trees, with y = x comparison line",ylab = "Predicted values", xlab = "Actual values")
abline(0, 1, col = "red")

# Comparing this graph wit the graph of predictions from a single tree -> better correspondence between predicted and oberved values when avergaing over a multiple of predictons
# plot(boston_bag) -> can see the error change as ntree increases

# We can calculated the MSE ->
mean((test.preds.bag - testing.data$medv)^2) # -> 23.33324
# This is much lower than the MSE form the single decision tree -> predictions are much closer to teh observations overall

# Random Forests 
# A better approach to bagged DT is to only consider a random subset of the VARIABLES at each potential split point -> setting mtry = something less than total number of input VARIABLES
# Default is to use:
# mtry = d/3 for regression (integer part)
# mtry = sqrt(d) for classifcation (integer part)

# Let's go ->
set.seed(1)
training.rows = sample(1:nrow(Boston), nrow(Boston)/2)
training.data = Boston[training.rows, ]
testing.data = Boston[-training.rows, ]

# Note -> dont get confused ... when using bagged DT or randomForest -> the training.data will be subsampled inside of randomForest for each tree

# Now create the DTs -> bagged and rf
boston_bag = randomForest(medv ~. , data = training.data, mtry = 13) # default ntree = 500
boston_rf = randomForest(medv ~. , data = training.data) # default ntree = 500 and mtry is default set to either d/3 or sqrt(d) depending on the data that R detects -> DONT SET mtry for RF except if BAGGED

test.preds.bag = predict(boston_bag, newdata = testing.data)
test.preds.rf = predict(boston_rf, newdata = testing.data)

par(mfrow = c(2, 1))

plot(testing.data$medv, test.preds.rf, main = "Predicted vs. observed median house value of owner-occupied homes ($000's)
across Boston suburbs using a random forest DT, with y = x comparison line", ylab = "Predicted values", xlab = "Actual values")
abline(0, 1, col = "red")

plot(testing.data$medv, test.preds.bag, main = "Predicted vs. observed median house value of owner-occupied homes ($000's)
across Boston suburbs using bagged DT, with y = x comparison line", ylab = "Predicted values", xlab = "Actual values")
abline(0, 1, col = "red")

par(mfrow = c(1, 1))

MSE.DT = mean((testing.data$medv - test.preds)^2) # -> 35.28688
MSE.bag = mean((testing.data$medv - test.preds.bag)^2) # -> 23.33324
MSE.rf = mean((testing.data$medv - test.preds.rf)^2) # -> 18.32787


#########################################################################################

# Naive Bayes Classifcation

# Approach to classifcation problems ->
# Refer to pg 18 for formula -> you know already the proportional relationships
# Important to critique Naive Bayes -> CONDITIONAL INDEPENDENCE

# We can express the probability of the output variable y taking a value ... say y = 1 conditional on values of d inputs xj -> notes
# We rely on the proportional to part of the equation -> we then rescale
# The denominator is a joint distribution probability which is impossible to find otherwise -> you understand what needs to be done

# Naive Bayes Classifcation Example ->

whisky = read.csv("whisky.csv")
head(whisky)

# Summarise the number of whiskies there are - table(whisky$name) # -> think ... there are list of names -> you want the unique names and count...
table(whisky$whisky.name) # -> 

# table() is a handy tool -> will use it frequently

# Let say we want to estimate the probabilities of each whisky type for a particular whisky that has been scored 2 in each category -> that is estimate:

# P(Dogvulin | S=2, F=2, C=2)
# P(Glenragh | S=2, F=2, C=2)
# P(Mactavish | S=2, F=2, C=2)
# P(Western Isle | S=2, F=2, C=2)

# Focuse on one of these to show the method ->
# Using the assumption of the Naive Bayes we have for Dogavulin ->

# P(D | S = 2, F = 2, C = 2) proportional to P(D) * P(S = 2 | D) * P(F = 2 | D) * P(C = 2 | D)
# We can estiamte the RHS probabilities using the data ->

# As the RHS probabilties are related to Dogavulin -> create a data set that only contains this whisky: Dogavulin

d = whisky[whisky$whisky.name == "Dogavulin", ]

# to estimate the P(S = 2 | D) ->
nrow(d[d$smoky == 2, ]) / nrow(d) # -> 0.09 for S = 2

# What about the smoky = 1 and 3?
p_s_d = table(d$smoky) / nrow(d) # -> this will sumamries the number of times smoky 1, 2, 3 and divide by length of d ->
p_s_d # ->     1     2     3 
			# 0.085 0.090 0.825

# Interpret p_s_d -> prob S = {1,2,3} given D

# We can do the same for the fuity and colour -> given the whisky is Dogavulin

p_f_d = table(d$fruity) / nrow(d)
p_c_d = table(d$colour) / nrow(d)

# We also need the Probability of Dogavulin -> P(D)
# Do the same thing as above ->
p_wh = table(whisky$whisky.name) / nrow(whisky)

# Have everything now to calculate -> P(D) * P(S = 2 | D) * P(F = 2 | D) * P(C = 2 | D)
p_wh["Dogavulin"] * p_s_d["2"] * p_f_d["2"] * p_c_d["2"] # -> 0.0107361

# But this is not the final probability -> remember LHS is proportional to RHS ... we've just got the RHS i.e. the proportional to 
# Before we can calculate the actual value we need to do the same as above for the other whiskies ->

d = whisky[whisky$whisky.name == "Dogavulin", ]
p_s_d = table(d$smoky) / nrow(d)
p_f_d = table(d$fruity) / nrow(d)
p_c_d = table(d$colour) / nrow(d)

g = whisky[whisky$whisky.name == "Glenragh", ]
p_s_g = table(g$smoky) / nrow(g)
p_f_g = table(g$fruity) / nrow(g)
p_c_g = table(g$colour) / nrow(g)

m = whisky[whisky$whisky.name == "Mactavish", ]
p_s_m = table(m$smoky) / nrow(m)
p_f_m = table(m$fruity) / nrow(m)
p_c_m = table(m$colour) / nrow(m)

wi = whisky[whisky$whisky.name == "Western_Isle", ]
p_s_wi = table(wi$smoky) / nrow(wi)
p_f_wi = table(wi$fruity) / nrow(wi)
p_c_wi = table(wi$colour) / nrow(wi)

# Collect all -> P(D) * P(S = 2 | D) * P(F = 2 | D) * P(C = 2 | D) type

vals = c(p_wh["Dogavulin"] * p_s_d["2"] * p_f_d["2"] * p_c_d["2"],
		 p_wh["Glenragh"] * p_s_g["2"] * p_f_g["2"] * p_c_g["2"],
		 p_wh["Mactavish"] * p_s_m["2"] * p_f_m["2"] * p_c_m["2"],
		 p_wh["Western_Isle"] * p_s_wi["2"] * p_f_wi["2"] * p_c_wi["2"])

probs = vals/sum(vals)

#    Dogavulin     Glenragh    Mactavish    Western_Isle 
#    0.51773192   0.36503232   0.01924568   0.09799008 

# We can see that Dogavulin has the highest probability -> Assuming equal misclassifcation costs across the whiskies, we would predict an unknown whisky with characterists:
# -> S=2, C=2, F=2 to be Dogavulin

# Naive Bayes Classifcation - Generic Functions
# In the previous example we manually acalcualted each of the various probabilities (or groups of probabitlities)
# -> General approach would be to have a set of custom functions that can calculate these PROBABILITIES

# Example -> write a function that calculate P(X = x | Y = y)
np.prob = function(data, y_col, y_val, x_col, x_val){
	data.y = data[data[, y_col] == y_val,] # example whisky.name = "Dogavulin" (given Y = y = Dogavulin)
	data.yx = data.y[data.y[, x_col] == x_val,] # Given Dogavulin, smoky and smoky = 2 (X = Smoky... x = 2)
	nrow(data.yx) / nrow(data.y)
	}
	
# Using this function ->
np.prob(whisky, "whisky.name", "Dogavulin", "smoky", 2) # -> 0.09

# Another Generic function that is useful -> returns the probabilties of the form: P(X = x | Y = y) for a given y ACROSS ALL POSSIBLE VALUES OF x -> Given Y = y = Dogavulin, get the distribution X -> 
# -> Distribution of X:smoky when smoky = {1,2,3}

nb.probs = function(data, y_col, y_val, x_col){
	data.y = data[data[, y_col] == y_val, ] # -> given the whisky name
	table(data.y[, x_col]) / nrow(data.y) # use table to aummarise x_col -> summarise the characteristics -> i.e. smoky ={1, 2, 3}
	}
 
nb.probs(whisky, "whisky.name", "Dogavulin", "smoky")	# ->     1     2     3 
#																0.085 0.090 0.825 

# This is p_s_d as we did above

# To summarise these generic functions:
# -> P(X = x1 | Y = y), P(X = x2 | Y = y), ... P(X = xn | Y = y) can be evaluted using nb.probs function where:
nb.probs = function(data, y_col, y_val, x_col){
	data.y = data[data[, y_col] == y_val, ] # -> given the whisky name
	table(data.y[, x_col]) / nrow(data.y) # use table to aummarise x_col -> summarise the characteristics -> i.e. smoky = {1, 2, 3} using table
	}
# This effectively gives the distribtion of X | Y = y 

# Create a training data set by randomly selecting 60% of the data ->
set.seed(842231)
train.rows = sample(1:nrow(whisky), 0.6 * nrow(whisky))
whisky.train = whisky[train.rows, ]
head(whisky.train)

nb.probs(whisky.train,"whisky.name", "Mactavish", "smoky")
# ->          1          2          3 
#		0.80519481 0.08658009 0.10822511 

# nb.probs() function outputs a table -> we need a data frame ->

# Start with smoky for all of the whiskies ->

smoky = as.data.frame(nb.probs(whisky.train, "whisky.name", "Mactavish", "smoky"))
smoky # ->
colnames(smoky) = c("Value", "Mactavish")

# We can see the other types of whiskies by using table(whisky.train$whisky.name)
# -> Dogavulin     Glenragh    Mactavish   Western_Isle 

# So repeating by adding other solums to smoky for the different whiskies ->

smoky$Dogavulin = nb.probs(whisky.train, "whisky.name", "Dogavulin", "smoky")
smoky$Glenragh = nb.probs(whisky.train, "whisky.name", "Glenragh", "smoky")
smoky$Western_Isle = nb.probs(whisky.train, "whisky.name", "Western_Isle", "smoky")

smoky # ->
# Lucky because there was alwaya a score for smoky i.e. 1, 2, or 3 for each of the whiskies -> but it can be the where it isn't

# Now do the same for Fruity
fruity = as.data.frame(nb.probs(whisky.train, "whisky.name", "Mactavish", "fruity"))
fruity # ->
colnames(fruity) = c("Value", "Mactavish")

fruity$Dogavulin = nb.probs(whisky.train, "whisky.name", "Dogavulin", "fruity")
fruity$Glenragh = nb.probs(whisky.train, "whisky.name", "Glenragh", "fruity")
fruity$Western_Isle = nb.probs(whisky.train, "whisky.name", "Western_Isle", "fruity")

# Now do the same for Colour
colour = as.data.frame(nb.probs(whisky.train, "whisky.name", "Mactavish", "colour"))
colour # ->
colnames(colour) = c("Value", "Mactavish")

colour$Dogavulin = nb.probs(whisky.train, "whisky.name", "Dogavulin", "colour")
colour$Glenragh = nb.probs(whisky.train, "whisky.name", "Glenragh", "colour")
colour$Western_Isle = nb.probs(whisky.train, "whisky.name", "Western_Isle", "colour")

y_probs = table(whisky.train$whisky.name) / nrow(whisky.train)

# Now apply the conditonal independence and construct a proportional to function ->
# Construct a prop.to function based on training data estimates:
# -> P(W = w) * P(S = s | W = w) * P(F = f | W = w) * P(C = c | W = w)
# Remember what we are doing -> need to determine the probability of P(W = w | S, F, C)
# -> The prop.to function must then have values for S, F, C as an input
# -> If we have those values, we know their distribtion given the whiskey name
# -> So we can specify a whisky name to find its probability given S, F, and C 

prop.to = function(whisky_name, smoky_val, fruit_val, colour_val){ 
	y_probs[whisky_name] *
		smoky[smoky_val, whisky_name] *
		fruity[fruit_val, whisky_name] * 
		colour[colour_val, whisky_name]
		}

# This function will work on vector of inputs -> smokey, fruity, and colour have vector inputs given the training data

# Example -> calculate the value of P(W = w) * P(S = s | W = w) * P(F = f | W = w) * P(C = c | W = w) for all rows of training data set for Mactavish whisky
prop.to("Mactavish", whisky.train$smoky, whisky.train$fruity, whisky.train$colour)
# This is just an example -> there are 27 combinations of S x F x C (1:3)^3

# Test Predictions
# Before we start predictions - Recap
# We created our training set from the original data
# Then constructed the objective -> determine the prop.to 
# Determined the distribution of S, F, C given the whiskey name -> nb.probs using the TRAINING data
# Created the dataframe that contains the distribution of S, F, C for all the whiskey names -> smoky, fruity, colour
# Use this with UNCONDITIONAL distribution of whisky names (p_wh or y_probs) to determine the prop.to
# Remember what the goal is here -> Use NAIVE BAYES FOR CLASSFICATION

# Now -> Test Predictions -> USE whisky.test
whisky.test = whisky[-train.rows, ]

# Create a data frame to store the predictions ->
# Initialise ->
preds = data.frame(ID = 1:nrow(whisky.test))

# loop over each of the whiskies using the names of the whiskies in y_probs table and input the whiky name and characteristic columns into prop.to()\
# ->

for (y in names(y_probs)){
	preds[ , y] = prop.to(y, whisky.test$smoky, whisky.test$fruity, whisky.test$colour)
	}

head(preds)

# This is the prop.to() for each of the whiskies
# -> now make the prediction by finding max in each row and corresponsinding column ->
preds$prediction = names(preds)[-1][max.col(preds[, -1])]
# -> see whats going on in here...the max.col gives a position in the columns excluding the first column ***
# -> then you want names(preds) -> exlcude the first colummn which is the ID column by [-1] and then use [max.col...] to give the position in names(preds)
# -> Exhausting :(
# In R, the max.col() function is used to find the column index of the maximum value in each row of a matrix or data frame

# Table of combinations ->
# Use table() 

mat = table(whisky.test$whisky.name, preds$prediction) 
# -> the LEFT TO RIGHT -> OBSERVED
# -> the TOP DOWN -> PREDICTED

# Accuracy -> look at main diagonal -> sum and divide by total test points ->
sum(diag(mat)) / nrow(whisky.test) # -> 0.8525

#########################################################################################

# Unsupervised Learning 

# K-Means with specified number of clusters
# K-Means takes a set of points and attempts to identify distinct cluster containing similair points

# Built in dataset -> faithful -> volcanic eruption data
# plot this dataset ->
plot(faithful, xlab = "Eruption time (minutes)", ylab = "waiting time to next eruption (minutes)", main = "Graph of waiting time to next eruption against eruption time for Old Faithful", pch = 20)

# Before applying K-Means (which we see we can based on the plot) -> what are we trying to achieve?
# Rescale the data -> THIS IS AN IMPORANT STEP
# We dont wan the distance calculations to be dominated by the waiting times
# We can use scale() which calculates z-scores

faithful.stand = scale(faithful) 
# standardise the values in faithful to zero mean and std deviation of 1 -> z-score
# stand -> standardised

# Can now perform the k-means clustering ->
set.seed(272)
faithful.clusters = kmeans(faithful.stand, 2) # -> the 2 here means number of centres -> we identify 2 in the plot
str(faithful.clusters)

# Algo has returned -> two clusters of sizes 98 and 174
# - cluster means i.e. centroids of each cluster
# - clustering vector which is the allocated cluster for each data point -> in this case there are two vectors [1, 2]
# Within cluster SS by clsuter
# Ratio of the between cluster SS to total SS
# Avaialable components that can be extracted using $ notation

# Cluster Centers ->
# Extract the centers using $centers
faithful.clusters$centers 
# ->    eruptions    waiting
#    1 -1.2577669 -1.1993566
#    2  0.7083975  0.6754997

# These clusters are based on SCALED DATA
# Have to unscale to get the values correct to base ->

# So for the points in cluster 1 ->
faithful.clusters$centers[1, 1] * sd(faithful$eruptions) + mean(faithful$eruptions)
# -> 2.052204 which is the average eruption time

# Quicker to calculate the means of each columnm of the original data for subsets based on cluster
# -> use colMeans() to return the average of each column

# Because we know the which cluster the data is assinged to, we can calculate the mean vector from there ->
# We can use $cluster to know which cluster the data point belongs to i.e. which cluster (1 or 2) it was assigned to

# For cluster 1 -> this will give us the centre of the cluster
colMeans(faithful[faithful.clusters$cluster == 1, ]) # Dont forget about the ,] -> you need the rows from faithful assigned to cluster 1

# Similarly for cluster 2 -> this will give us the centre of the cluster
colMeans(faithful[faithful.clusters$cluster == 2, ]) # Dont forget about the ,] -> you need the rows from faithful assigned to cluster 1

# If we wanted both results -> This is a lot easier to understand
centers = data.frame(
	Cluster1 = colMeans(faithful[faithful.clusters$cluster == 1, ]),
	Cluster2 = colMeans(faithful[faithful.clusters$cluster == 2, ]))

centers

# R has returned the cluster centres as columns
# If we wanted the centres as rows instead of columns -> transpose
t(centers) # -> this is much better to read off

# Plotting the clusters -> same plot from before BUT adding col = faithful.cluster
plot(faithful, xlab = "Eruption time (minutes)", ylab = "waiting time to next eruption (minutes)", main = "Graph of waiting time to next eruption against eruption time for Old Faithful",
	pch = 20, col = faithful.clusters$cluster) # see the col assingment here -> col = faithful.clusters$cluster
legend("topleft", legend = c("Cluster 1", "Cluster 2"), col = c(1, 2), lwd = 2)
# The col argument is set to a vector 1 and vector 2 i.e. cluster 1 and cluster 2
points(t(centers), cex = 5, pch = 3, lwd = 4, col = "blue") # -> add the cluster centres using points() and t(centres) to match the axes
# Remember we undid the scaling to get the centres! Dont forget about it because you can't use it otherwise in analysis
# The centers used here are the UNSCALED CENTERS

# Describing the Clusters
# Output the centroids as a reminder of where the clusters are centred ->
faithful.clusters$centers # -> is extracted from $ notation -> you will see the bottom of the output what can be taken
# Dont forget, these are SCALED values

# Remember that we scaled / normalized the data -> each column has a mean = 0 and a sd = 1
# -> A Positive average value for one of the columns indicates this cluster represents points that are overall above average for that variable
# As the scaled columns all have unit variance, the maginitude of the numbers across the columns gives an indication of how far from the overall average the centroid is for that variable

# We've identified two clusters...Cluster 1 and Cluster 2
# Cluster 1 has many fewer points than Cluster 2 (98 vs 174) -> roughly 1:2
# Appears to be 1/3 of the time much lower eruption time and waiting time on average

# Consider the centroids of the clusters -> here we look at centeres that we unscaled -> centers

# Gives an idea on the magnitude of the differences between clusters in original units
# We can see that the eruptions in Cluster 1 is 2.24 mins shorter than eruptions in Cluster 2...etc

# K-means with proposed initial centres
# Lets say it was suggested to rerun algo with initial cluster centres (2.2, 80) and (4.5, 55)
# Looking at the plots before...seems whoever suggested these points had the y values mixed up
# We can reun the algo based on these initial centres...see what happens

# First we will need to scale the eruptions and times in faithful to reflect these given centres -> standardise

erup.centres = (c(2.2, 4.5) - mean(faithful$eruptions))/sd(faithful$eruptions) # -> We standardise the initial centres...we need a SCALED starting point -> kmeans() takes scaled values
wait.centres = (c(80, 55) - mean(faithful$waiting))/sd(faithful$waiting) # -> We standardise the initial centres...we need a SCALED starting point -> kmeans() takes scaled values

init.centres = data.frame(erup = erup.centres, wait = wait.centres) # -> 2x2 data frame with one row per clusterm columns representing centroid co-ordinates

# We can now re-run kmeans() ->
faithful.clusters2 = kmeans(faithful.stand, init.centres) # -> here we specifiy the INITIAL STANDARDISED CENTRES and we still use our STANDARDISED faithful dataset
faithful.clusters2

faithful.clusters2$cluster

faithful.clusters$centers
faithful.clusters2$centers
# -> So even though the given initial centres didn't look sensisble, the algo converged on the same clusters as before...as expected

centers.2 = data.frame(
	Cluster1 = colMeans(faithful[faithful.clusters2$cluster == 1, ]),
	Cluster2 = colMeans(faithful[faithful.clusters2$cluster == 2, ]))

# -> We do this so that the rows represent the cluster, and the columns the coordinates of the cluster centroid

centers.2 = t(centers.2)
centers = t(centers)

# Check the unscaled centroids -> form initial centers, converged to the centers we had before
# So it does appear that are two distinct clusters in this data set

# Calculating Euclidean Distance
# We manually calculate the Euclidean distance from the ith point to the jth cluster centre

# sqrt((Wi - Wj_bar)^2  + (Ei - Ej_bar)^2)
# Where Wi is the waiting time at the ith point, Ei the eruption time of the ith point
# -> Wj_bar -> average waiting time of points in cluster j
# -> Ej_bar -> average waiting time of points in cluster j

euc.dist = function(x1,x2){
	sqrt(sum((x1-x2)^2))
	}
	
euc.dist(faithful[1, ], centers[, 1])

# Use apply(matrix, MARGIN, function) -> where we apply the function to rows of the matrix (if MARGIN = 1 -> rows)
# If MARGIN = 2 -> columns of the matrix

dists1 = apply(faithful, 1, function(row){euc.dist(row, centers[, 1])}) # -> careful with centers[, 1] -> this isn't transposed 
# -> so the cluster is the column name and rows have the (x,y) coordinates
head(dists1)

# Do the same thing for the second cluster
dists2 = apply(faithful, 1, function(row){euc.dist(row, centers[, 2])})# -> careful with centers[, 1] -> this isn't transposed 
# -> so the cluster is the column name and rows have the (x,y) coordinates
head(dists2)

# apply() is quite useful -> apply(data, MARGIN, function(){the function code})

# We can now create a vector that indicates which cluster centre each point is closest to -> use ifelse()
cluster = ifelse(dists1 < dists2, 1, 2)
head(cluster)
# -> We can see that the first data point in faithful -> the first row -> is closest to the second cluster centre

# Lets see if the output aligns with the cluster allocations from kmeans() 
which(cluster != faithful.clusters$cluster)

# IMPORTANT -> If we used the unscaled data, there is a chance that we get mismatches
# -> Rather best to SCALE the data, and run the DISTANCE measurments on SCALED data
# -> we see that there are 4 mismatches -> This happened becaue so far we have calculated distances to each cluster centre using UNSCALED data
# -> When kmeans() does it, it uses the scaled data _> IMPORTANT...kmeans() uses SCALED DATA

# We can recalculate our distances based on the SCALED data ->

dists1.scaled = apply(faithful.stand, 1, function(row){euc.dist(row, faithful.clusters$center[1, ])})
head(dists1.scaled)

dists2.scaled = apply(faithful.stand, 1, function(row){euc.dist(row, faithful.clusters$center[2, ])})
head(dists2.scaled)
 
# We can then create a vector indicating which cluster each is closest to -> check whether there are any differnces to the output by kmeans()
cluster.scaled = ifelse(dists1.scaled < dists2.scaled, 1, 2)
which(cluster.scaled != faithful.clusters$cluster) # -> named integer(0) -> indicates no mismathches

# Sum of Squares
# Recall the two outputs of the kmeans()
# -> WITHIN cluster sum of squares by cluster
# -> the ratio of the between-cluster sum of squares and the total sum of squares -> like an R^2

# The WITHIN cluster SS for a particular cluster gives the sum of squared distances from each point to cluster centre -> WITHIN GROUP SS -> sum of squared distances
# from each  point to cluster centre
# sum(i=1, nj)[(Wij - Wj_bar)^2 + (Eij - Ej_bar)^2] -> SS of all the points in Cluster j

# Where:
# Wij is the waiting time of the ith point in Cluster j
# Eij is the eruption time of the ith point in Cluster j
# Wj_bar is the average waiting time of points in Cluster j
# Ej_bar is the average eruption time points in Cluster j
# sqrt[(Wij - Wj_bar)^2 + (Eij-Ej_bar)^2)]
# and nj is the number of points in Cluster j

# sum(i=1, nj)sqrt([(Wij - Wj_bar)^2 + (Eij - Ej_bar)^2])^2
# -> sum(i=1, nj)[Euclidean Distances]^2

# This is a measure of within-cluster homogeneity -> the smaller these figures the more tightly packed each is

# The ratio of the between-cluster SS to the total SS is a measure of cluster heterogeneity -> the between-cluster SS ->
# sum(j=1, k)[nj * (sqrt((Wj_bar - W_bar)^2 +(Ej_bar -E_bar)^2))^2] -> sum (nj * [sqrt((Wj_bar - Wbar)^2 + (Ej_bar - E_bar)^2)^2] -> BEWTWEEN CLUSTER SS
# Where:
# -> W_bar is the average waiting time of ALL POINTS
# -> E_bar is the average eruption time of ALL POINTS
# -> Wj_bar, Ej_bar and nj are defined as above
# -> k is the number of clusters 

# The larger it is, the further apart the clusters are
# The TOTAL SS ->
# sum(j=1, k)[sum(i=1, nj)[(sqrt((Wij - W_bar)^2 + (Eij - E_bar)^2))^2] -> See here that we use the global W_bar, and E_bar
# This is quite cool -> the between SS as a ratio of the total SS is like an R^2 -> Total SS = Within SS + Between SS

# The total SS is the sum of the within-cluster SS and between-cluster SS
# The ratio of the between cluster SS to the total SS is a measure that describes how much of the total SS has been explained
# by different clusters...similar to R^2 -> REMEMBER -> The ratio of BETWEEN CLUSTER SS to TOTAL SS -> measure of how much total SS has been explained by different clusters
# IMPORTANT -> The closer the ratio is to 1, the greater the heterogeneity (BECAUSE BETWEEN CLUSTER SS is a MEASURE OF CLUSTER HETEROGENEITY between clusters, 
# and the greater the homogeneity within clusters
# We can change the ratio by considering different numbers of clusters -> hyperparameter
# We can force the ratio to equal 1 exactly by having each individual point as its own cluster of size 1 -> not a useful thing to do
# Similar to inflating R^2 in linear regression by continually adding more and more covariates without considering their significance

# Investigating different numbers of clusters
# A method to try is select an appropriate number of clusters -> identify a number after which there is no longer a significant improvement
# -> in total within-group SS IMPORTANT
# -> When adding another cluster, we look at the Within Cluster SS -> By improve -> does it get smaller at a quicker rate?
# Use a plot of total within SS -> kmean() -> we can extract this info after adding additonal cluster
# What does this mean...Remember we must consider the WITHIN_GROUP SS...adding another cluster...does the WITHIN_GROUP SS improve?

# We can calculate this for different numbers of clusters say 1 to 10...
# Plot the result ->

	tot.withinss = numeric(10)

	for (i in 1:10){
		set.seed(6)
		tot.withinss[i] = kmeans(scale(faithful), i)$tot.withinss # -> DONT FORGET TO SCALE THE DATA
		}
		
	tot.withinss

	plot(1:10, tot.withinss, main = "Plot of total within-group sum of squares against cluster count for k-means clustering on movie data",
	 xlab = "cluster count",ylab = "total within-group sum of squares", type = "b")
 
# Here we see a sharp drop from 1 cluster to 2 cluster assignment -> thereafter (after 2 clusters) it dcreases but at much lower rate
# So 2 clusters seems sensibile
# # Think how this will be different with higher dimensional data sets

# Principle Component analysis
# Useful for pre-processing high dimensional data
# Reduces dimentionality -> retain only the first few principals
# can then perform K-means on the selected principal components

# Perform PCA using prcomp() ->
pca = prcomp(trees, scale.=TRUE) # -> PCA should be performed on data that is centred around 0 -> prcomp does this automatically i.e. SCALED DATA -> prcomp does it automatically
str(pca) # -> useful for extraction

# The $center and $scale components gives the means and standard deviations of each column
# Values of x_bar and sd are the vaues of $ center and $ scaled
# Usefule check ->

pca$center
colMeans(trees)
# Should give the same result

pca$scale
apply(trees, 2, sd) # -> note how this works: apply(data, MARGIN = 2 for columns, function()) -> MARGIN = 2 for the columns
# Should give the same result

# the command $x gives the the component of pca that contains the corordinates of each points on each of the new principal components axes
# -> after rotating that is

head(pca$x)

# To go back to the original data, we rotate this matrix ($x) by the inverse of the rotation matrix -> to rotate back
# -> The INVERSE of the Rotation matrix is its TRANSPOSE

recons = pca$x%*%t(pca$rotation) # -> where t(pca$rotation) = inv(rotation) -> in R the inverse of a matrix: solve(matrix)
head(recons) # -> youll see it doesnt look like trees data -> because its centred and scaled! You then need to undo the normalization

head(recons)
head(scale(trees)) # -> should match the recons 

# We use PCA for dimensionality reduction -> if there are linear relationships between the variables in the original data, we should
# be able to represent (at least somewhat accurately) the original data using only a subset of the principal components
# -> lower dimensionality without losing too much overall information

# To decide on what principal components to keep is to consider their standard deviations and variances -> use summary()

summary(pca)
# -> First line gives us the standard deviation for each principal component
# -> The second line indicates the PROPORTION of the OVERALL VARIATION in the original data set that is CAPTURED by each PRINCIPAL component
# -> The first PC captures 80.34% of the variation in the data -> This will decrease as we look at additional PCs
# -> The third line is the cumulative proportion of the variation captured as we include more and more PCs
# -> Essentailly the cumsum of the Proportion of Variances

# Example: we may wish to capture at least 99% of the variation in the data set -> Therefore need to keep the first two PCs
# -> We can reconstruct the original (centred and scaled) data using the first <n> PCs as follows ->

n=2 # -> Reconstruct the original dataset using the first two PCs
pca$x[ ,1:n]%*%t(pca$rotation[, 1:n])

# IMPORANT -> apply the pca on dataset
# -> get the pca$x
# -> use summary(pca) to determine what PCs explain the VARIATION
# -> then rotate pca$x by the inverse of its rotation -> tranpose of its rotation! SEE BELOW
# -> REMEMBER the reconstruction using the PCs AS BELOW IS SCALED
# -> UNSCALE USING CODE BELOW

# Reconstrucint trees using first two PCs ->
recons.2 = pca$x[ ,1:n]%*%t(pca$rotation[, 1:n]) # -> Remember that these values are still SCALED

n=2# -> Reconstruct the original dataset using first tw0 PCs
recons.3 = pca$x[ ,1:n]%*%t(pca$rotation[, 1:n])

scale = pca$scale
scale = matrix(rep(scale, nrow(recons.3)), nrow = nrow(recons.3), ncol = length(scale), byrow=TRUE)

center = pca$center
center = matrix(rep(center, nrow(recons.3)), nrow = nrow(recons.3), ncol = length(center), byrow=TRUE)

w = recons.3 * scale + center # UNSCALE recons.3
trees

# Plot of the original data
pairs(scale(trees), main = "Plot of centred and scaled diameter (labelled girth), height and volume of 31 black cherry trees")

# Plot of the original data
pairs(recons.2, main = "Plot of reconstructed centred and scaled diameter (labelled girth), height and volume of 31 black cherry trees from 2 principal components")

# Unscale recons.2
means = colMeans(trees)
std.dev = apply(trees, 2, sd)

means = matrix(c(rep(means, nrow(recons.2))), nrow=nrow(recons.2), byrow=TRUE)
std.dev=matrix(c(rep(std.dev, nrow(recons.2))), nrow=nrow(recons.2), byrow=TRUE)

trees.2 = recons.2*std.dev + means

head(trees.2); head(trees)


#########################################################################################
# Question 1

set.seed(19)
train.row = sample(1:nrow(iris), 0.6 * nrow(iris))
train.data = iris[train.row, ]

head(train.data, 10)

iris_tree = tree(Species ~. , data = train.data)

plot(iris_tree)
text(iris_tree)

case = data.frame(Sepal.Length = 5,  Sepal.Width = 3.7,  Petal.Length = 2.5, Petal.Width = 0.3)
predict(iris_tree, newdata = case, type = "class") # -> versicolor

test.data = iris[-train.rows, ]
iris.preds = predict(iris_tree, newdata = test.data, type = "class")

mat = table(test.data$Species, iris.preds)
# -> one misclassifcation

set.seed(7223)
iris.rf = randomForest(Species ~. , data = train.data, mtry = 2, ntree = 1000) # -> remember this isn't bagged...using mtry = 2 so subsampling explanatories

predict(iris.rf, newdata = case, type = "class") # -> setosa 

iris.preds.rf = predict(iris.rf, newdata = test.data, type = "class")
mat.rf = table(test.data$Species, iris.preds.rf)

# With Classfication problems -> we can using the table() function and determine accuracy of the Predictions

# Accuracy with DT ->
sum(diag(mat)) / nrow(test.data) # -> 0.9824561

# Accuracy with RF->
sum(diag(mat.rf)) / nrow(test.data) # -> 1

# Sometimes the DT can just perform better than RF ... maybe spcific to the data here 


#########################################################################################
# Question 3

install.packages("rpart.plot")
library(rpart.plot)
data(ptitanic)

head(ptitanic)

set.seed(10)
train.rows = sample(1:nrow(ptitanic), 0.5 * nrow(ptitanic))

train.data =  ptitanic[train.rows, ]
head(train.data, 4)

set.seed(6047)
ptitanic.rf = randomForest(survived ~ pclass + sex + sibsp + parch, data = train.data, ntree = 1500)

test.data = ptitanic[-train.rows, ]
ptitanic.preds.rf = predict(ptitanic.rf, newdata = test.data, type = "clas")

con.mat = table(test.data$survived, ptitanic.preds.rf) # -> Confusion matrix
con.mat = con.mat[2:1, 2:1] # -> flips it around such that survival is postive outcome

TP = con.mat[1, 1]
TN = con.mat[2, 2]
FP = con.mat[2, 1]
FN = con.mat[1, 2]

A = (TP + TN) / sum(con.mat)
R = TP / (TP + FN)
P = TP / (TP + FP)
FPR = FP / (FP + TN)
F1 = 2 * P * R / (P + R)

A # -> 0.7770992
R # -> 0.6096654
P # -> 0.8
FPR # -> 0.1062176
F1 # -> 0.6919831

# Naive Bayes ...
# -> write a function that calculate P(X = x | Y = y)
np.prob = function(data, y_col, y_val, x_col, x_val){
	data.y = data[data[, y_col] == y_val,] # example whisky.name = "Dogavulin" (given Y = y = Dogavulin)
	data.yx = data.y[data.y[, x_col] == x_val,] # Given Dogavulin, smoky and smoky = 2 (X = Smoky... x = 2)
	nrow(data.yx) / nrow(data.y)
	}
	
# Prob of zero siblings or spouse given passenger survived
np.prob(train.data, "survived", "survived", "sibsp", 0) # ->  0.6103896

# Prob of zero siblings or spouse given passenger died
np.prob(train.data, "survived", "died", "sibsp", 0) # -> 0.6997636

# Prob of survival (unconditional)
length(train.data$survived[train.data$survived == "survived"]) / length(train.data$survived) # -> 0.353211

# -> P(X = x1 | Y = y), P(X = x2 | Y = y), ... P(X = xn | Y = y) can be evaluted using nb.probs function where:
nb.probs = function(data, y_col, y_val, x_col){
	data.y = data[data[, y_col] == y_val, ] # -> given the whisky name
	table(data.y[, x_col]) / nrow(data.y) # use table to aummarise x_col -> summarise the characteristics -> i.e. smoky = {1, 2, 3} using table
	}
# This effectively gives the distribtion of X | Y = y 

y_probs = table(train.data$survived) / length(train.data$survived)

# Write prop.to for survival status...
prop.to = function(surv_status, sex, sibsp, parch, pclass){ 
	y_probs[surv_status] *
		np.prob(train.data, "survived", surv_status, "sex", sex) * 
		np.prob(train.data, "survived", surv_status, "sibsp", sibsp) * 
		np.prob(train.data, "survived", surv_status, "parch", parch) * 
		np.prob(train.data, "survived", surv_status, "pclass", pclass)
		}

pass.preds = data.frame(ID = 1:nrow(test.data))

pass.preds$survived = apply(test.data, MARGIN = 1, function(row){prop.to("survived", row["sex"], row["sibsp"], row["parch"], row["pclass"])})
pass.preds$died = apply(test.data, MARGIN = 1, function(row){prop.to("died", row["sex"], row["sibsp"], row["parch"], row["pclass"])})

head(pass.preds)

# Now we can form predictions based on whether surv or died is the max of the row ->

pass.preds$pred = names(pass.preds)[-1][max.col(pass.preds[-1])]

con.mat = table(test.data$survived, pass.preds$pred)
con.mat = con.mat[2:1, 2:1]

TP = con.mat[1, 1]
TN = con.mat[2, 2]
FP = con.mat[2, 1]
FN = con.mat[1, 2]

A = (TP + TN) / sum(con.mat)
R = TP / (TP + FN)
P = TP / (TP + FP)
FPR = FP / (FP + TN)
F1 = 2 * P * R / (P + R)

A # -> 0.7694656
R # -> 0.6394052
P # -> 0.7610619
FPR # -> 0.1398964
F1 # -> 0.6949495

# The random forest has a lower false positive rate of 10.62% compared to 13.99% for the naïve Bayes model. So, the random forest does a better job at not misclassifying passengers who
# died as surviving

# If these metrics accuracy reflect their performance on out-of-sample data, then which may be more useful would depend on the importance of the
# metrics. If a low false positive rate is desired, for example, then the random forest may be preferred even though it scores worse on the recall and overall F1 score

#########################################################################################

# Can we do the same analysis on whisky?

# Naive Bayes ...
# -> write a function that calculate P(X = x | Y = y)
np.prob = function(data, y_col, y_val, x_col, x_val){
	data.y = data[data[, y_col] == y_val,] # example whisky.name = "Dogavulin" (given Y = y = Dogavulin)
	data.yx = data.y[data.y[, x_col] == x_val,] # Given Dogavulin, smoky and smoky = 2 (X = Smoky... x = 2)
	nrow(data.yx) / nrow(data.y)
	}

set.seed(842231)
train.rows = sample(1:nrow(whisky), 0.6*nrow(whisky))
train.data = whisky[train.rows, ]
test.data = whisky[-train.rows, ]

y_probs = table(train.data$whisky.name) / length(train.data$whisky.name)

# Write prop.to for survival status...
prop.to = function(whisky.name, smoky, fruity, colour){ 
	y_probs[whisky.name] *
		np.prob(train.data, "whisky.name", whisky.name, "smoky", smoky) * 
		np.prob(train.data, "whisky.name", whisky.name, "fruity", fruity) * 
		np.prob(train.data, "whisky.name", whisky.name, "colour", colour)
		}

pass.preds = data.frame(ID = 1:nrow(test.data))

pass.preds$Dogavulin = apply(test.data, MARGIN = 1, function(row){prop.to("Dogavulin", row["smoky"], row["fruity"], row["colour"])})
pass.preds$Mactavish = apply(test.data, MARGIN = 1, function(row){prop.to("Mactavish", row["smoky"], row["fruity"], row["colour"])})
pass.preds$Glenragh = apply(test.data, MARGIN = 1, function(row){prop.to("Glenragh", row["smoky"], row["fruity"], row["colour"])})
pass.preds$Western_Isle = apply(test.data, MARGIN = 1, function(row){prop.to("Western_Isle", row["smoky"], row["fruity"], row["colour"])})

head(pass.preds)

# Now we can form predictions based on whether surv or died is the max of the row ->

pass.preds$pred = names(pass.preds)[-1][max.col(pass.preds[-1])]

con.mat = table(test.data$whisky.name, pass.preds$pred)

Acc = sum(diag(con.mat)) / sum(con.mat) # -> 0.8525

#########################################################################################
# Question 4

movie = read.csv("movie.data.csv")

cluster.centers = data.frame(
					colMeans(movie[movie$Cluster == 1, ]),
					colMeans(movie[movie$Cluster == 2, ]),
					colMeans(movie[movie$Cluster == 3, ]))

colnames(cluster.centers ) = paste("centers", 1:3)
cluster.centers = cluster.centers[-nrow(cluster.centers), ]

euc.dist = function(x1, x2){
	sqrt(sum((x1 - x2)^2))
	}

# Use apply(matrix, MARGIN, function) -> where we apply the function to rows of the matrix (if MARGIN = 1 -> rows)
# If MARGIN = 2 -> columns of the matrix

movie$dists1 = apply(movie, 1, function(row){euc.dist(row[1:5], cluster.centers[, "centers 1"])}) # Be careful to index row from 1:5 so that only the input variables (and not the cluster column) are being used in the calculation
movie$dists2 = apply(movie, 1, function(row){euc.dist(row[1:5], cluster.centers[, "centers 2"])}) # Be careful to index row from 1:5 so that only the input variables (and not the cluster column) are being used in the calculation
movie$dists3 = apply(movie, 1, function(row){euc.dist(row[1:5], cluster.centers[, "centers 3"])}) # Be careful to index row from 1:5 so that only the input variables (and not the cluster column) are being used in the calculation

dists = data.frame(dists1, dists2, dists3)
dists$cluster = c(1, 2,3)[max.col(-dists)]

movie$Cluster.ass = dists$cluster
which(movie$Cluster != movie$Cluster.ass) # row index 264

movie[which(movie$Cluster != movie$Cluster.ass), ] # -> 
#    Horror Romcom Action Comedy Fantasy Cluster   dists1  dists2   dists3 Cluster.ass
#     62.8     18   65.7   50.5    54.5       3  30.71538 62.3233 32.92628           1

movie$Cluster[which(movie$Cluster != movie$Cluster.ass)] = 1

# Redo calcs above ...###################################

movie = read.csv("movie.data.csv")
movie$Cluster[264] = 1

cluster.centers = data.frame(
					colMeans(movie[movie$Cluster == 1, ]),
					colMeans(movie[movie$Cluster == 2, ]),
					colMeans(movie[movie$Cluster == 3, ]))

colnames(cluster.centers ) = paste("centers", 1:3)
cluster.centers = cluster.centers[-nrow(cluster.centers), ]

euc.dist = function(x1, x2){
	sqrt(sum((x1 - x2)^2))
	}

# Use apply(matrix, MARGIN, function) -> where we apply the function to rows of the matrix (if MARGIN = 1 -> rows)
# If MARGIN = 2 -> columns of the matrix

movie$dists1 = apply(movie, 1, function(row){euc.dist(row[1:5], cluster.centers[, "centers 1"])}) # Be careful to index row from 1:5 so that only the input variables (and not the cluster column) are being used in the calculation
movie$dists2 = apply(movie, 1, function(row){euc.dist(row[1:5], cluster.centers[, "centers 2"])}) # Be careful to index row from 1:5 so that only the input variables (and not the cluster column) are being used in the calculation
movie$dists3 = apply(movie, 1, function(row){euc.dist(row[1:5], cluster.centers[, "centers 3"])}) # Be careful to index row from 1:5 so that only the input variables (and not the cluster column) are being used in the calculation

dists = data.frame(dists1, dists2, dists3)
dists$cluster = c(1, 2,3)[max.col(-dists)]

movie$Cluster.ass = dists$cluster
which(movie$Cluster != movie$Cluster.ass) # Integer 0

# ... ###################################

# Running kmeans()

movie = movie[, 1:6]
x = movie[, 1:5]
x = scale(x)
head(x)

set.seed(6)
movie.clusters = kmeans(x, 3)
# movie.cluster$cluster -> we can use this to calculate the unscaled cluster centers -> using code from earlier but just assigning the cluster number to move.cluster$cluster

kmeans.cluster.centers = data.frame(
					colMeans(movie[movie.clusters$cluster == 1, ]),
					colMeans(movie[movie.clusters$cluster == 2, ]),
					colMeans(movie[movie.clusters$cluster == 3, ]))

colnames(kmeans.cluster.centers ) = paste("centers", 1:3)
kmeans.cluster.centers = kmeans.cluster.centers[-nrow(kmeans.cluster.centers), ]

# Cluster 1 points are the same in comparison -> Clusters 2,3 may have had some points switched around
# We don't know what distance metric the analyst used to minimise the distances between points and cluster centroids

# Cluster 1 prefers Horror and Action movies. Doesn't care much for Romcom or Comedies
# Cluster 2 prefers Comedey and Fantasy. Is undecided on whether they prefer action or Romcom. Definitely not a horror fan
# Cluster 3 prefers Action, Comedey and a bit of Horror. Not too keen on Fantasy and Romcoms

plot(movie[, 1:5], col = movie.clusters$cluster, main = "Plot of average movie scores for five types of movies, coloured by k-means cluster")

total.within.ss = numeric(10)

for (j in 1:length(total.within.ss)){
	
	set.seed(6)
	total.within.ss[j] = kmeans(scale(movie[, 1:5]), j)$tot.withinss
	
	}

plot(seq(1, 10, 1), total.within.ss, xlab = "number of clusters considered", ylab = "total within SS", main = "plot of total within SS for different number of clusters", 
	col = "red", type = "b")

total.within.ss # -> diff(total.witih.ss)
# The marginal differences in total.within.ss starts to diminish quite quickly after 4 clusters -> 3 looks appropriate for this model