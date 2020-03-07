dataSex <- read.csv("/Statistics/sshpa/sex_author_articles.csv", header = TRUE)
head(dataSex)

dataSex$fieldnum <- as.numeric(dataSex$field)
dataSex$field_sex <- factor(paste0(dataSex$fieldnum,"_",dataSex$sex))
dataSex$sexnum <- as.numeric(dataSex$field_sex)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "article", "norm")
model <- bvl_addNode(model, "sexnum", "cat")
model <- bvl_addNode(model, "fieldnum", "cat")

model <- bvl_addArc(model, "fieldnum",  "sexnum", "varint")
model <- bvl_addArc(model, "sexnum", "article", "varint")

model <- bvl_modelFix(model, dataSex)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

#dataList <- bvl_modelData(model, dataSex)
#dataList <- bvl_modelFix(model, data)

# Fit the model
model <- bvl_modelFit(model, dataSex, warmup = 2000, iter = 5000, chains = 4, cores = 4)

summary(model)

bvl_plotTrace(model)

bvl_plotIntervals(model)

stan_diag(model@stanfit)

#lookup_subj = 1
sex_labs = c("Male", "Female")
sexnum2fieldnum <- unique(dataSex[c("sexnum","fieldnum")])
sexnum2sex <- unique(dataSex[c("sexnum","sex")])
for(lookup_subj in 1:4)
{
	#png(paste0(modelName,"age_of_",lookup_subj,".png"),width=900,height=600)
	
	pars_subj = c()
	for(i in 1:length(sexnum2fieldnum[,"sexnum"]))
	{
		if (lookup_subj == sexnum2fieldnum[,"fieldnum"][i])
			pars_subj <- c(pars_subj, paste0("a_sexnum[",sexnum2fieldnum[,"sexnum"][i],"]"))
	}
	postSubj <- rstan::extract(model@stanfit, pars = pars_subj)
	ref_subj <- melt(postSubj)
	colnames(ref_subj)[2:3] <- c("a_sexnum","colors")
	
	for(i in 1:length(sexnum2sex[,"sexnum"]))
	{
		ref_subj[ref_subj==paste0("a_sexnum[",sexnum2sex[,"sexnum"][i],"]")]<-sexnum2sex[,"sex"][i]
		print(sex_labs[sexnum2sex[,"sex"][i]])
		print(sexnum2sex[,"sexnum"][i])
	}
	plotSchool <- ggplot(data=ref_subj,aes(x=a_sexnum, color=colors))+
		geom_density(size=1) +
		scale_color_discrete(name = "Fields", labels = sex_labs)
	print(plotSchool)
	
	#dev.off()
}



bvl_plotDensity2d(model, "a_sexnum[4]", "a_sexnum[13]", color_scheme = "blue", c("Eco (male)","Edu (male)"))

ggplot2::ggplot(model@posterior, aes(x=model@posterior[["a_sexnum[1]"]], y=model@posterior[["a_sexnum[3]"]]))+
	geom_point(alpha = 0.3, color = "blue")+
	geom_density2d(color = "gray30")+
	viridis::scale_color_viridis(option = "C")+ 
	geom_abline(intercept=0,slope=1) +
	labs(x = "Eco (male)", y = "Edu (male)")

ggplot2::ggplot(model@posterior, aes(x=model@posterior[["a_sexnum[2]"]], y=model@posterior[["a_sexnum[4]"]]))+
	geom_point(alpha = 0.3, color = "purple")+
	geom_density2d(color = "gray30")+
	viridis::scale_color_viridis(option = "C")+ 
	geom_abline(intercept=0,slope=1) +
	labs(x = "Eco (female)", y = "Edu (female)")

