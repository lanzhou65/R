library('readxl')
library(caret)
set.seed(123)

dowret=na.omit(readRDS('dowret.RDS'))
#View(dowret[5,])
#pca_data = as.data.frame(data[,-27])

inTrain <- createDataPartition(dowret$AAPL, p = .8,list=F) 

traindata=dowret[inTrain,]
testdata=dowret[-inTrain,]
nrow(traindata)
nrow(testdata)

pca.out = prcomp(dowret,scale=TRUE) #scale is to standardize 
pca.out

summary(pca.out)


pca.out$x %*% t(pca.out$rotation)
#View(scale(pca_data)) 

#install.packages('factoextra')
library(factoextra)
eig.val <- get_eigenvalue(pca.out)
eig.val

#An alternative method to determine the number of principal components is to look at a Scree Plot,
fviz_eig(pca.out, addlabels = TRUE, ylim = c(0, 50))

#Correlation Circle
#Color by cos2 values: quality on the factor map
fviz_pca_var(pca.out, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Quality of Representation
library("corrplot")
var <- get_pca_var(pca.out)
var
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)    #contributions of the variables" 


#DNA of Assets
#denomalized loading=loading^t*principal component sdev
s=t(pca.out$rotation)*pca.out$sdev
#barplot(s[c(1,2),],beside=TRUE,col=c("blue","red"),legend = rownames(s[c(1,2),]))
barplot(s[c(1),],beside=TRUE,col=c("red"),legend = rownames(s[c(1),]))
#s[c(1,2),]
pca1 = s[c(1),] #loadings of each variabces - 
rownames(s[c(1,2),])
dim(s)
p = data.frame(sort(pca1))
p

#k-means
set.seed(123)
wcss <- vector()
for (i in 1:10){ wcss[i] <- sum(kmeans(t(s), i)$withinss) }
#can't tell which is the optimal clusters 
plot(1:10, wcss, type = "b", main = paste('Clusters of factors '), xlab= "Number of Clusters", ylab= "WCSS")
  
#Partition over PCA loadings
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
res.km <- kmeans(t(s), centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
grp

library(cluster)
clusplot(t(s),
         res.km$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste ('Clusters of asset variables'),
         xlab = 'xxx',
         ylab = 'yyy')

# Color variables by groups
fviz_pca_var(pca.out, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

#Calculate a distance matrix using the complete linkage method
#calculate Ecledean distance of the asset classes to the matrix 
d=dist(t(s))
###commodity and us bond has 1.4, far, but us bond and corp. bond 0.33, close 
d
#Generate the dendrogram
#looks for the closest points, and repeat and combine them into one cluster 
#find DNA distance 
clusters <- hclust(d)
plot(clusters)
clusters

#cut off the tree at the desired number of clusters using cutree.
#to divide the tree to only 3 classes 
clusterCut <- cutree(clusters, 3)
clusterCut

#Use the mean linkage method, change the method to average 
#change the destination of the distance 
#different dendrogram - most accurate case 
#
clusters <- hclust(d, method = 'average')
plot(clusters)


#Question 3

#Two-Tier "Risk Parity"
GetRiskParityWeighting <- function (pDate)
{
  FactorSigma=(diag(GetFactorCov(pDate)))^0.5  #diag(covariance matrix) gives variances of each factor
  
  W_Mtum=1/FactorSigma['Mtum']
  W_Vol=1/FactorSigma['Vol']
  W_Fundamental=1/7/FactorSigma[c('Profit','Value','CapUse','FunStab','Growth','Lev','Size')]
  
  W=c(W_Fundamental,W_Mtum,W_Vol)/sum(W_Mtum,W_Vol,W_Fundamental)#weights of each factor
  return(W)
}

# Factor Portfolio
x=GetRiskParityWeighting('2019-01-17')
x
#    Profit      Value     CapUse    FunStab     Growth        Lev       Size       Mtum        Vol 
#0.07298757 0.04589602 0.10875659 0.08035508 0.06341989 0.07537733 0.05481002 0.31585239 0.18254510 

# Replicate the "factor" portfolio using long/short stock baskets
#factor loading for S&P 500
FactorLoading=readRDS('FactorLoading.RDS') #dim(FactorLoading) = 2701533 * 13
mat1<-subset(FactorLoading,Date=='2019-01-17')  #dim(mat1)= 2488 * 13

# Quadrative Programming
# Solve:
# min 0.5*x'*H*x + f'*x  min variance/risk  sol$value, solve for x
# subject to:  A*x <= b   
# Aeq*x = beq  
# LB <= x <= UB 

#number of variables  dim(mat1) = 2488 * 13 
n=nrow(mat1[,-c(1,2,3,4)]) #n = 2488 mat1[,-c(1,2,3,4)] : keep all the rows, skip first 4 columns

library('quadprog')
H <- diag(n)  #diagonal identity matrix with 1 on the diagonal
f <- rep(0,n) #repeat 0 n times 

# equalities
A.eq <- as.matrix(mat1[,-c(1,2,3,4)])           #factor loading matrix, keep all the rows, skip first 4 columns
b.eq <- x/0.2277644                   #weightings of factor change
length(x)
sum(x)

# lower-bounds 
A.lbs <- diag(n)
b.lbs <- rep(-0.01,n) #repeat -0.01 n times

# upper-bounds on variables
A.ubs <- -diag(n)
b.ubs <- -rep(0.01,n)

# solve
sol <- solve.QP(Dmat = H, #matrix to be minimized
                dvec = f, #vector to be minimized 
                Amat = cbind(A.eq, A.lbs, A.ubs), #matrix defining the constraints under which we want to minimize the quadratic function.
                bvec = c(b.eq, b.lbs, b.ubs),
                meq = length(b.eq)) # this argument says the first "meq" rows of Amat are equalities

#these two have the same weights 
t(A.eq) %*% sol$solution  #transpose of factor loading %*% solution
x/0.2277644  #same as the above

names(sol$solution)<-mat1[,2]
sol$solution[1:50]
sum(sol$solution[sol$solution>0]) #0.2277644
sum(sol$solution[sol$solution<0]) #-0.2065938
plot(sol$solution)

#> class(sol$solution)
#[1] "numeric"
#> length(sol$solution)
#[1] 2488


#Simulation  -need to use covariance matrix 
strategy =FactorCov[,1]*0  #dim(strategy) = 512 *1 
names(strategy) <- 'simulationreturn'

for (i in 1:nrow(FactorCov)){
  x=GetRiskParityWeighting(time(FactorCov[i,]))/0.2277644 #to match the weights  #time creates the vector of times at which a time series was sampled.
  
  if (nrow((FactorRet[time(FactorCov[i,])]))>0) {
    strategy[i]=FactorRet[time(FactorCov[i,])] %*% x   #factorRet[time()] = 1*9; x =  9 *1 => gives a number
  }
}
chart.CumReturns(strategy,legend.loc='bottomright')

# Compute the annualized mean
Return.annualized(strategy) #0.01309051 vs.0.05782604



# Compute the annualized standard deviation
StdDev.annualized(strategy)  # 0.01088339 vs. 0.04778354


# Compute the annualized Sharpe ratio: ann_sharpe
ann_sharpe <- Return.annualized(strategy) / StdDev.annualized(strategy)
ann_sharpe #1.202797  vs. 1.210166

# Compute all of the above at once using table.AnnualizedReturns()
table.AnnualizedReturns(strategy)

# Table of drawdowns
table.Drawdowns(strategy) 
# Plot of drawdowns
chart.Drawdown(strategy)  #0 - (-0.008) vs. -0.01 - (-0.03) 



