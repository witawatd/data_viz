coupon_redempt <- read.csv("~/Desktop/465/data/coupon_redempt.csv")
campaign_table <- read.csv("~/Desktop/465/data/campaign_table.csv")
coupon <- read.csv("~/Desktop/465/data/coupon.csv")
hh_demo <- read.csv("~/Desktop/465/data/hh_demographic.csv")
product <- read.csv("~/Desktop/465/data/product.csv")
trans <- read.csv("~/Desktop/465/data/transaction_data.csv")

###join

df_age<- hh_demo[,c("AGE_DESC","household_key")]

###coupon
Age_coup<-merge(df_age, coupon_redempt, by = "household_key")
Age_coup$CAMPAIGN<- as.factor(Age_coup$CAMPAIGN)
summary(Age_coup$CAMPAIGN)
summary(Age_coup$CAMPAIGN,maxsum=6)
hist(Age_coup$CAMPAIGN)

Campaign18 <- subset(Age_coup, CAMPAIGN=="18", select=c(household_key, AGE_DESC))
Campaign13 <- subset(Age_coup, CAMPAIGN=="13", select=c(pollen, colors))
Campaign8 <- subset(Age_coup, CAMPAIGN=="8", select=c(pollen, colors))
Campaign26 <- subset(Age_coup, CAMPAIGN=="26", select=c(pollen, colors))

vioplot(blue$pollen, green$pollen, purple$pollen, red$pollen, names=c("blue", "green", "purple", "red"), col="yellow")
camp_freq<-count(Age_coup$CAMPAIGN)
camp_freq$campaign<- camp_freq$x
require(lattice)

panel.violin(camp_freq$campaign,camp_freq$freq)

require(ggplot2)

agecou<- ggplot(camp_freq, aes(campaign, freq))
agecou+geom_violin()

library(vioplot)
vioplot(camp_freq$freq)

library(plyr)
count(Age_coup,"household_key")
count(Age_coup$CAMPAIGN)

##select_AGE_TRANS_Pro
#extract product
pro_desc<- product[,c("PRODUCT_ID", "COMMODITY_DESC")]

#AGE with trans
transAge<-merge(df_age, trans, by = "household_key")
####MERGE product_
trans_produc <- merge(transAge, pro_desc, by = "PRODUCT_ID")
trans_produc$prod_type<-trans_produc$COMMODITY_DESC

require(ggplot2)



Age_coup$CAMPAIGN<- as.factor(Age_coup$CAMPAIGN)

summary(Age_coup$CAMPAIGN)

summary(cus_4554$prod_type)













fit <- princomp(trans_produc, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)


install.packages("tm")
install.packages("arules")
library(arules)
library(tm)
head(trans_produc)

df_trans<- trans_produc[,c("household_key","BASKET_ID","AGE_DESC","prod_type")]
df_trans$BASKET_ID <- as.factor(df_trans$BASKET_ID)
df_trans$household_key <- as.factor(df_trans$household_key)

rules <- apriori(df_trans, parameter=list(support=0.01, confidence=0.01))
inspect(rules)

trans1<- df_trans[,c("BASKET_ID","prod_type")]
rules <- apriori(trans1, parameter=list(support=0.0001, confidence=0.01))
inspect(rules)

rules <- apriori(newtrans, parameter=list(support=0.001, confidence=0.001))
inspect(rules)




##NETWORK Plot
install.packages("igraph")
require(igraph)

raw <- trans_produc

head(raw)
#select customers

#topten = data.frame(raw,raw$prod_type == c("SOFT DRINKS"))  
topten <- raw[ which(raw$prod_type==c("BEERS/ALES",
                                      "BABY FOODS","FITNESS&DIET" ,"PHARMACY","FILM AND CAMERA PRODUCTS"  ,
                                      "VITAMINS","COFFEE"  )),]

topten<- na.omit(topten) 

edges = data.frame(p0=rep(0, 10000), p1=rep(0, 10000))

nEdges = 0
for (household in unique(topten$household_key))
{
  hPurchases = topten[topten$household_key == household, ]
  for (i in 1:nrow(hPurchases))
  {
    row = hPurchases[i, ]
    row
    hPurchases$prod_type[i]
    prodID = hPurchases$prod_type[i]
    for (j in i:nrow(hPurchases))
    {
      prodID2 = hPurchases$prod_type[j]
      
      if (prodID != prodID2)
      {
        nEdges = nEdges + 1
        edges$p0[nEdges] = as.character(prodID)
        edges$p1[nEdges] = as.character(prodID2)
      }
    }
  }
}
edges = edges[1:nEdges, ]

edge_test1<-cbind(edges,1)
colnames(edge_test1)[3]<-"num_edge"
require(plyr)
edge_test2<-ddply(edge_test1,.(p0,p1),numcolwise(sum))


write.csv(edge_test2, file="product.csv")

#####################

#g = graph.data.frame(edges, directed=F)
#g = simplify(g, remove.multiple = T)
#plot(g, layout=layout.fruchterman.reingold, vertex.label=NA, vertex.size=5)
#######
newedge<- na.omit(edges)
g = graph.data.frame(newedge, directed=F)
#g = simplify(g, remove.multiple = T)
E(g)$weight <- edge_betweenness(g)
E(g)$weight <- as.numeric(E(g)$weight)
#######
plot(g,layout=layout.circle,	# the layout method. see the igraph documentation for details
     main='Product Relationship',	#specifies the title
     vertex.label.dist=0.6,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.size = 10,
     edge.width=edge_betweenness(g),
     vertex.label.cex=0.95
     ,edge.curved=TRUE)



plot(g,layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Product Relationship',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.size = 10,
     edge.width=edge.betweenness(g),
     vertex.label.cex=1)

plot(g,layout=layout.reingold.tilford(g,circular=T,root=2),	# the layout method. see the igraph documentation for details
     main='Product Relationship',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.size = 10,
     edge.width=edge.betweenness(g),
     vertex.label.cex=1)





g = graph.data.frame(newedge, directed=F)
#g = simplify(g, remove.multiple = T)
E(g)$weight <- edge.betweenness(g)

plot(g,layout=layout.circle,	# the layout method. see the igraph documentation for details
     main='Top-ten Most Frequent Product',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     edge.width=edge.betweenness(g),
     vertex.label.cex=1)
unique(topten$household_key)
unique(topten$BASKET_ID) 

