posts <- read.csv("/Statistics/SciComm/sc25.csv", header = TRUE)
head(posts)

topics <- read.csv("/Statistics/SciComm/topics.csv", header = TRUE)
head(topics)

views <- read.csv("/Statistics/SciComm/viewByDate.csv", header = TRUE)
head(views)

ctry <- read.csv("/Statistics/SciComm/viewByCtry.csv", header = TRUE)
head(ctry)

refer <- read.csv("/Statistics/SciComm/viewByReferer.csv", header = TRUE)
head(refer)

f7 <- read.csv("/Statistics/SciComm/f7ByReferer.csv", header = TRUE)
head(f7)

f100 <- read.csv("/Statistics/SciComm/f7ByReferer.csv", header = TRUE)
head(f100)

allsite <- read.csv("/Statistics/SciComm/allSiteByDate.csv", header = TRUE)
head(allsite)
		
library(ggplot2)
setwd("/Statistics/SciComm/Graphs")

allsite$dt <- as.Date(allsite$ViewDate, format = "%m/%d/%y")
allsite <- allsite[order(allsite$dt),]
ggplot(data=allsite,aes(x=dt, y=ViewCount))+
		geom_line() +
		xlab("") +
		scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

for(i in 1:length(posts[,1]))
{
	row=posts[i, ]
	postId=row[1, 1]
	
	png(paste0("post_",postId,".png"),width=900,height=600)
	
	subData = views[views$PostId==postId,]
	subData$dt <- as.Date(subData$ViewDate, format = "%m/%d/%y")
	
	viewChart <- ggplot(data=subData,aes(x=dt, y=ViewCount))+
		geom_line() +
		scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

	print(viewChart)
	
	dev.off()
}


#for(i in 1:length(posts[,1]))
#{
#	row=posts[i, ]
#	postId=row[1, 1]
#	
#	png(paste0("post_",postId,".png"),width=300,height=100)
#	
#	subData = views[views$PostId==postId,]
#	subData$dt <- as.Date(subData$ViewDate, format = "%m/%d/%y")
#	
#	viewChart <- ggplot(data=subData,aes(x=dt, y=ViewCount))+
#		geom_line() +
#		xlab("") +
#		ylab("") +
#		scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
#
#	print(viewChart)
#	
#	dev.off()
#}

library(reshape2)

dfm <- melt(topics[,c('Id','PostCount','ViewCount')],id.vars = 1)

ggplot(dfm,aes(x = Id,y = value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge") 
    
labs = c("","Life","New Research","SciComm","Amazing Science","Collaboration")
ggplot(topics,aes(x = Id,y = ViewCount)) + 
    geom_bar(stat = "identity", fill = "#56B4E9") +
    xlab("") +
    scale_x_discrete(limits=labs)

ggplot(topics,aes(x = Id,y = PostCount)) + 
    geom_bar(stat = "identity", fill = "#E69F00") +
    xlab("") +
    scale_x_discrete(limits=labs)


subData = ctry[ctry$PostId==2135,]


labs = c("Social Network","Search Engine","Internal","www.sshpa.com","Others")
for(i in 1:length(posts[,1]))
{
	row=posts[i, ]
	postId=row[1, 1]
	print(postId)
	
	png(paste0("post_ref_all_",postId,".png"),width=900,height=600)
	
	subData = refer[refer$PostId==postId,]
	
	viewChart <- ggplot(subData,aes(x = Referer,y = ViewCount)) + 
    geom_bar(stat = "identity", fill = "#56B4E9") +
    xlab("") +
    scale_x_discrete(limits=labs)

	print(viewChart)
	
	dev.off()
}

for(i in 1:length(posts[,1]))
{
	row=posts[i, ]
	postId=row[1, 1]
	print(postId)
	
	png(paste0("post_ref_f7_",postId,".png"),width=900,height=600)
	
	subData = f7[f7$PostId==postId,]
	
	viewChart <- ggplot(subData,aes(x = Referer,y = ViewCount)) + 
    geom_bar(stat = "identity", fill = "#56B4E9") +
    xlab("") +
    scale_x_discrete(limits=labs)

	print(viewChart)
	
	dev.off()
}


for(i in 1:length(posts[,1]))
{
	row=posts[i, ]
	postId=row[1, 1]
	print(postId)
	
	png(paste0("post_ref_f100_",postId,".png"),width=900,height=600)
	
	subData = f100[f100$PostId==postId,]
	
	viewChart <- ggplot(subData,aes(x = Referer,y = ViewCount)) + 
    geom_bar(stat = "identity", fill = "#56B4E9") +
    xlab("") +
    scale_x_discrete(limits=labs)

	print(viewChart)
	
	dev.off()
}


