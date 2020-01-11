dataLeader <- read.csv("/Statistics/sshpa/leader_year.csv", header = TRUE)
head(dataLeader)

boxplot(authors~pubyear, data=dataLeader, main="Year ~ Number of New Leaders", 
   xlab="Year", ylab="Leader #")
   
dataLeader <- read.csv("/Statistics/sshpa/leader_year.csv", header = TRUE)
head(dataLeader)

ggplot(data=dataLeader,
       aes(x=pubyear, y=authors, colour=field)) +
       geom_line() +
       ylab("New Leader #") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataAge$pubyear), by = 1))

dataLeader <- read.csv("/Statistics/sshpa/leader_age.csv", header = TRUE)
head(dataLeader)

ggplot(data=dataLeader,
       aes(x=pubyear, y=avgage, color=field)) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataAge$pubyear), by = 1))

dataLeader <- read.csv("/Statistics/sshpa/leader_sex.csv", header = TRUE)
head(dataLeader)

ggplot(data=dataLeader[dataLeader$field=='eco',],
       aes(x=pubyear, y=authors, fill=as.factor(sex))) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Leaders") +
       scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))

ggplot(data=dataLeader[dataLeader$field=='edu',],
       aes(x=pubyear, y=authors, fill=as.factor(sex))) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Leaders") +
       scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))

ggplot(data=dataLeader[dataLeader$field=='med',],
       aes(x=pubyear, y=authors, fill=as.factor(sex))) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Leaders") +
       scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))

ggplot(data=dataLeader[dataLeader$field=='other',],
       aes(x=pubyear, y=authors, fill=as.factor(sex))) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Leaders") +
       scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))
