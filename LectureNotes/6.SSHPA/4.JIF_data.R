
##############
dataJif <- read.csv("/Statistics/sshpa/jif_field.csv", header = TRUE)
head(dataJif)
  
jif_labs = c("=0", "<=1","<=2","<=3","<=4","<=5","<=6","<=7","<=8",">8")

ggplot(data=dataJif,
       aes(x=jif, y=article, fill=field)) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Articles") +
       xlab("JIF") +
       scale_fill_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=seq(0, 9, by = 1), labels=jif_labs) +
       geom_text(aes(label=article), position=position_dodge(width=0.9), vjust=-0.25)

#ggplot(data=dataJif,
#       aes(x=jif, y=article, fill=field)) +
#       geom_bar(stat="identity") +
#       scale_fill_discrete(name = "Fields", labels = c("Economics", "Education", "Others")) +
#       scale_x_discrete(limits=seq(0, 9, by = 1), labels=jif_labs) +
#       geom_text(aes(label=article), position=position_dodge(width=0.9), vjust=-0.25)

#boxplot(jif~article, data=dataJif[dataJif$jif>0 & dataJif$field=='eco',], main="JIF ~ Number of Article", 
#   xlab="Article #", ylab="JIF")

#boxplot(jif~article, data=dataJif[dataJif$jif>0 & dataJif$field=='edu',], main="JIF ~ Number of Article", 
#   xlab="Article #", ylab="JIF")

#boxplot(jif~article, data=dataJif[dataJif$jif>0 & dataJif$field=='other',], main="JIF ~ Number of Article", 
#   xlab="Article #", ylab="JIF")


dataJif <- read.csv("/Statistics/sshpa/jif_year.csv", header = TRUE)
head(dataJif)

boxplot(jif~pubyear, data=dataJif[dataJif$jif>0 & dataJif$field=='eco',], main="JIF ~ Year", 
   xlab="Year", ylab="JIF")

abline(h=mean(dataJif[dataJif$jif>0 & dataJif$field=='eco',]$jif), col="red", lwd=3, lty=2)

text(0.3, y = mean(dataJif[dataJif$jif>0 & dataJif$field=='eco',]$jif) + 0.3, labels = round(mean(dataJif[dataJif$jif>0 & dataJif$field=='eco',]$jif), 2), col="red")


boxplot(jif~pubyear, data=dataJif[dataJif$jif>0 & dataJif$field=='edu',], main="JIF ~ Year", 
   xlab="Year", ylab="JIF")

abline(h=mean(dataJif[dataJif$jif>0 & dataJif$field=='edu',]$jif), col="red", lwd=3, lty=2)

text(0.5, y = mean(dataJif[dataJif$jif>0 & dataJif$field=='edu',]$jif) + 0.3, labels = round(mean(dataJif[dataJif$jif>0 & dataJif$field=='edu',]$jif), 2), col="red")


boxplot(jif~pubyear, data=dataJif[dataJif$jif>0 & dataJif$field=='med',], main="JIF ~ Year", 
   xlab="Year", ylab="JIF")

abline(h=mean(dataJif[dataJif$jif>0 & dataJif$field=='edu',]$jif), col="red", lwd=3, lty=2)

text(0.5, y = mean(dataJif[dataJif$jif>0 & dataJif$field=='med',]$jif) + 0.3, labels = round(mean(dataJif[dataJif$jif>0 & dataJif$field=='med',]$jif), 2), col="red")


boxplot(jif~pubyear, data=dataJif[dataJif$jif>0 & dataJif$field=='other',], main="JIF ~ Year", 
   xlab="Year", ylab="JIF")

abline(h=mean(dataJif[dataJif$jif>0 & dataJif$field=='other',]$jif), col="red", lwd=3, lty=2)

text(0.5, y = mean(dataJif[dataJif$jif>0 & dataJif$field=='other',]$jif) + 0.3, labels = round(mean(dataJif[dataJif$jif>0 & dataJif$field=='other',]$jif), 2), col="red")


ggplot(data=dataJif[dataJif$jif>0 & dataJif$field=='eco',],
       aes(x=as.factor(pubyear), y=jif)) +
       geom_boxplot()

dataJif <- read.csv("/Statistics/sshpa/jif_age.csv", header = TRUE)
head(dataJif)
  
jif_labs = c("=0", "<=1","<=2","<=3","<=4","<=5","<=6","<=7","<=8",">8")

ggplot(data=dataJif,
       aes(x=jif, y=avgage, fill=field)) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("Age") +
       xlab("JIF") +
       scale_fill_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=seq(0, 9, by = 1), labels=jif_labs) +
       geom_text(aes(label=avgage), position=position_dodge(width=0.9), vjust=-0.25)

dataJif <- read.csv("/Statistics/sshpa/jif_grouped_leader.csv", header = TRUE)
head(dataJif)
  
jif_labs = c("=0", "<=1","<=2","<=3","<=4","<=5","<=6","<=7","<=8",">8")

ggplot(data=dataJif,
       aes(x=jif, y=article, fill=field)) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Articles") +
       xlab("JIF") +
       scale_fill_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=seq(0, 9, by = 1), labels=jif_labs) +
       geom_text(aes(label=article), position=position_dodge(width=0.9), vjust=-0.25)
