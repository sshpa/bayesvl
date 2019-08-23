dataAge <- read.csv("/Statistics/sshpa/age_author_articles.csv", header = TRUE)
head(dataAge)

dataAge$fieldnum <- as.numeric(dataAge$field)
dataAge$field_age <- factor(paste0(dataAge$fieldnum,"_",dataAge$age))
dataAge$agenum <- as.numeric(dataAge$field_age)
#dataAge$agenum2fieldnum <- unique(dataAge[c("agenum","fieldnum")])[,"fieldnum"]

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "article", "norm")
model <- bvl_addNode(model, "agenum", "cat")
model <- bvl_addNode(model, "fieldnum", "cat")

model <- bvl_addArc(model, "fieldnum",  "agenum", "varint")
model <- bvl_addArc(model, "agenum", "article", "varint")

model <- bvl_modelFix(model, dataAge)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

#dataList <- bvl_modelData(model, dataAge)
#dataList <- bvl_modelFix(model, data)

# Fit the model
model <- bvl_modelFit(model, dataAge, warmup = 2000, iter = 5000, chains = 4, cores = 4)

summary(model)

bvl_plotTrace(model)

bvl_plotIntervals(model)

#lookup_subj = 1
age_labs = c("<25","25-29","30-34","35-39","40-44","45-49","50-54","55-59",">=60")
agenum2fieldnum <- unique(dataAge[c("agenum","fieldnum")])
agenum2age <- unique(dataAge[c("agenum","age")])
for(lookup_subj in 1:4)
{
	#png(paste0(modelName,"age_of_",lookup_subj,".png"),width=900,height=600)
	
	pars_subj = c()
	for(i in 1:length(agenum2fieldnum[,"agenum"]))
	{
		if (lookup_subj == agenum2fieldnum[,"fieldnum"][i])
			pars_subj <- c(pars_subj, paste0("a_agenum[",agenum2fieldnum[,"agenum"][i],"]"))
	}
	postSubj <- rstan::extract(model@stanfit, pars = pars_subj)
	ref_subj <- melt(postSubj)
	colnames(ref_subj)[2:3] <- c("a_agenum","colors")
	
	for(i in 1:length(agenum2age[,"agenum"]))
	{
		ref_subj[ref_subj==paste0("a_agenum[",agenum2age[,"agenum"][i],"]")]<-agenum2age[,"age"][i]
		print(age_labs[agenum2age[,"age"][i]])
		print(agenum2age[,"agenum"][i])
	}
	plotSchool <- ggplot(data=ref_subj,aes(x=a_agenum, color=colors))+
		geom_density(size=1) +
		scale_color_discrete(name = "Fields", labels = age_labs)
	print(plotSchool)
	
	#dev.off()
}


age_labs = c("<25","25-29","30-34","35-39","40-44","45-49","50-54","55-59",">=60")
for(lookup_subj in 1:4)
{
	#png(paste0(modelName,"age_of_",lookup_subj,".png"),width=900,height=600)
	
	pars_subj = c()
	for(i in 1:length(agenum2fieldnum[,"agenum"]))
	{
		if (lookup_subj == agenum2fieldnum[,"fieldnum"][i])
			pars_subj <- c(pars_subj, paste0("a_agenum[",agenum2fieldnum[,"agenum"][i],"]"))
	}
	postSubj <- rstan::extract(model@stanfit, pars = pars_subj)
	ref_subj <- melt(postSubj)
	colnames(ref_subj)[2:3] <- c("a_agenum","colors")

	age_mean = c()
	age_sd = c()
	age_min = c()
	age_max = c()
	age_20 = c()
	age_80 = c()
	
	for(a in 1:length(age_labs))
	{
		for(i in 1:length(agenum2fieldnum[,"agenum"]))
		{
			ageid = agenum2fieldnum[,"agenum"][i]
			if (lookup_subj == agenum2fieldnum[,"fieldnum"][i] && agenum2age[,"age"][agenum2age$agenum==ageid] == a)
			{
				print(ageid)
				sub <- ref_subj$a_agenum[ref_subj$colors==paste0("a_agenum[",ageid,"]")]
				age_mean = c(age_mean, mean(sub))
				age_sd = c(age_sd, sd(sub))
				
				a_quant = quantile(sub,c(0.025, 0.2, 0.50, 0.8, 0.975))
				a_quant <- data.frame(t(a_quant))
				names(a_quant) <- c("Q5",  "Q20", "Q50", "Q80", "Q95")
				
				age_min = c(age_min, a_quant$Q5)
				age_max = c(age_max, a_quant$Q95)
				age_20 = c(age_20, a_quant$Q20)
				age_80 = c(age_80, a_quant$Q80)
		
				ref_subj[ref_subj==paste0("a_agenum[",ageid,"]")]<-age_labs[agenum2age[,"age"][agenum2age$agenum==ageid]]
				print(age_labs[agenum2age[,"age"][agenum2age$agenum==ageid]])
			}
		}
	}
	
	a_df <- data.frame(age_mean, age_max, age_min, age_20, age_80, age_sd, age_labs)
	#round(head(a_df), 2)
	
	a_df <- a_df[order(a_df$age_mean), ]
	a_df$age_rank <- c(1 : dim(a_df)[1])
	
	ggplot(data = a_df, 
	       aes(x = age_labs, 
	           y = age_mean)) +
	  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	  geom_linerange(aes(ymin=age_20,ymax=age_80),size=2) +
	  geom_pointrange(aes(ymin = age_min, 
	                      ymax = age_max)) + 
	  geom_hline(yintercept = mean(a_df$age_mean), 
	             size = 0.5, 
	             col = "red") +
	  ylab("alpha_age") +
	  scale_x_discrete(name="Age", limits=a_df$age_labs)
	
	#dev.off()
}


bvl_plotDensity2d(model, "a_agenum[4]", "a_agenum[13]", color_scheme = "blue", c("Eco (35-39)","Edu (35-39)"))

ggplot2::ggplot(model@posterior, aes(x=model@posterior[["a_agenum[4]"]], y=model@posterior[["a_agenum[13]"]]))+
	geom_point(alpha = 0.3, color = "blue")+
	geom_density2d(color = "gray30")+
	viridis::scale_color_viridis(option = "C")+ 
	geom_abline(intercept=0,slope=1) +
	labs(x = "Eco (35-39)", y = "Edu (35-39)")

ggplot2::ggplot(model@posterior, aes(x=model@posterior[["a_agenum[4]"]], y=model@posterior[["a_agenum[22]"]]))+
	geom_point(alpha = 0.3, color = "orange")+
	geom_density2d(color = "gray30")+
	viridis::scale_color_viridis(option = "C")+ 
	geom_abline(intercept=0,slope=1) +
	labs(x = "Eco (35-39)", y = "Med (35-39)")

ggplot2::ggplot(model@posterior, aes(x=model@posterior[["a_agenum[4]"]], y=model@posterior[["a_agenum[24]"]]))+
	geom_point(alpha = 0.3, color = "purple")+
	geom_density2d(color = "gray30")+
	viridis::scale_color_viridis(option = "C")+ 
	geom_abline(intercept=0,slope=1) +
	labs(x = "Eco (35-39)", y = "Med (45-49)")
