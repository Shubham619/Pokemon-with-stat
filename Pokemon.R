View(Pokemon)
names(Pokemon)
Pokemon_data <- as.matrix(Pokemon[,c(6:11)])

##Peform PCA 
colMeans(Pokemon_data)

apply(Pokemon_data,2,sd)

pok.pr <- prcomp(Pokemon_data,scale=TRUE)

summary(pok.pr)

# Interpreting PCA results with scale=TRUE
biplot(pok.pr)
#Interpreting PCA results with scale=FALSE
pok.pr.ns <- prcomp(Pokemon_data,scale=FALSE)
biplot(pok.pr.ns)

##Calculating variability of each component
pr.var <- pok.pr$sdev ^2

#Variance explained by each PC
pve <- pr.var/sum(pr.var)

# Plotting variance explained by each component
par(mfrow=c(1,2))
plot(pve,xlab="Principal Component",ylab="Proportion of variance explained",ylim=c(0,1),type="b")

plot(cumsum(pve),xlab="Principal Component" ,ylab="Cumulative proportion of variance explained",ylim=c(0,1),type="b")

#Performing hierarchical clustering using euclidean distance
scaled_data <- scale(Pokemon_data)

Pokemon_hclust<- hclust(dist(scaled_data),method="complete")

plot(Pokemon_hclust)

# selecting number of clusters 
Pokemon_hclust_clusters <- cutree(Pokemon_hclust,k=4)


#intialize wss 
wss <- 0
for(i in 1:15){
  
  km.out<- kmeans(Pokemon_data,centers = i,nstart = 20,iter.max=50)
  wss[i] <- km.out$tot.withinss
}
plot(1:15,wss,type="b",xlab="Number of Clusters",ylab="Within group sum of squares")



k <- 4

# Build model with k clusters: km.out
km.out <- kmeans(Pokemon_data, centers = k, nstart = 20, iter.max = 50)
km.out

# Plot of Defense vs. Speed by cluster membership
plot(Pokemon_data[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")

# Plot of Defense vs. Sp. Atk by cluster membership
plot(Pokemon_data[, c("Defense", "Sp. Atk")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")

table(km.out$cluster,Pokemon_hclust_clusters)


# Create a hierarchical clustering model: wisc.pr.hclust
pokemon.pr.hclust <- hclust(dist(pok.pr$x[, 1:6]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
pokemon.pr.hclust.clusters <- cutree(pokemon.pr.hclust, k = 4)

