library(ggplot2)
library(ggpubr)

############################################################
##############################################################
#############################################################

data <- read.csv("2020-XTern-DS.csv",sep=",")

cuisines = c()
list = c()
col <- as.character(data$Cuisines)

for(j in 1:nrow(data)){
	temp = col[j]
	temp = strsplit(temp,", ")
	cuisines <- append(cuisines,temp)
	list <- c(list,cuisines[[j]])
}

cuisines <- unique(list)
times <- c()

for(i in 1:length(cuisines)){
	times <- c(times, length(which(list == cuisines[i])))
}

d <- data.frame(
  name=cuisines ,  
  value=times
  )


ggplot(d, aes(x=name,value, y=value,fill=rownames(d),label=value)) +
  xlab("Cuisine") + ylab("Number of Orders") +
  geom_bar(stat = "identity") +
  guides(fill=FALSE) + 
  coord_flip() +
  theme(axis.text.y = element_text(color = "grey20", size = 5,hjust = .5, face = "plain"))

##############################################################
##########################################################
####################################################################

data$Rating[which(data$Rating=="NEW")]<- NA
data$Rating[which(data$Rating=="Opening Soon")]<- NA
data$Rating[which(data$Rating=="-")]<- NA
data$Rating <- as.numeric(as.character(data$Rating))
cook = c()
list2 = c()
col2 <- as.character(data$Cook_Time)

for(j in 1:nrow(data)){
	temp = col2[j]
	temp = strsplit(temp," ")
	cook <- append(cook,temp)
	list2 <- c(list2,cook[[j]][1])
}

list2 <- as.numeric(list2)

res <- kruskal.test(data$Rating~list2)
res



boxplot(data$Rating~list2,main="",xlab="Cooking Time (minutes)", 
ylab = "Ratings (out of 5)",pch=16)

######################################################
#########################################################

col3 <- as.character(data$Average_Cost)
list3 <- c()

for(j in 1:nrow(data)){
	temp = col3[j]
	temp = substring(temp,2)
	list3 <- append(list3,temp)
}

list3 <- as.numeric(list3)

res <- kruskal.test(data$Rating~list3)
res

boxplot(data$Rating~list3,main="",xlab="Average Cost (dollars)", 
ylab = "Ratings (out of 5)",pch=16)

res2 <- cor.test(data$Rating,list3, method="pearson", use="complete.obs")
res2

df <- data.frame(
  cost=list3,
  rating=data$Rating
)

ggscatter(df, x = "cost", y = "rating",add = "reg.line",conf.int = TRUE,add.params = list(color = "blue",fill = "lightgray"))+
  stat_cor(method = "pearson", label.x = 3, label.y = 5)


##################################################################
##################################################################
##################################################################

col4 <- as.character(data$Cuisines)
classList <- c()

for(j in 1:nrow(data)){
	temp = col4[j]
	temp = strsplit(temp,", ")
	if("Chinese" %in% temp | "Fast Food" %in% temp | "North Indian" %in% temp){
		classList <- append(classList, "Popular")
	}
	else{
		classList <- append(classList, "Not Popular")
	}
}

res <- kruskal.test(data$Rating~classList)
res

boxplot(data$Rating~classList,main="",xlab="What Cuisines were Ordered?", 
ylab = "Ratings (out of 5)",pch=16)


