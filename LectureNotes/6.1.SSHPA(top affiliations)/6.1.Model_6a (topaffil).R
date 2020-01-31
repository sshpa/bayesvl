data1 <- read.csv("/Users/Shared/Relocated Items/Security/Statistics/network/bayesvl/LectureNotes/6.1.SSHPA(top affiliations)/Data/data-bayes.csv", header = TRUE)
head(data1)

###############################

data1$affilid <- as.numeric(data1$affil)

data1$sexaffil <- factor(paste0(data1$sexid,"_",data1$affilid))

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "article", "norm")
model <- bvl_addNode(model, "sexaffil", "cat")
model <- bvl_addNode(model, "sexid", "cat")
model <- bvl_addNode(model, "age", "norm")

model <- bvl_addArc(model, "age",  "article", "slope")
model <- bvl_addArc(model, "sexaffil", "article", "varint")

model <- bvl_addArc(model, "sexid", "sexaffil", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

# review the model's diagram, that is a network of model components as declared above
bvl_bnPlot(model)

options(mc.cores = parallel::detectCores())

#start_time <- Sys.time()

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

#end_time <- Sys.time()

#end_time - start_time

bvl_trace(model)

bvl_plotDensity2d(model, "a_sexid[1]", "a_sexid[2]", labels = c("a(Male)", "a(Female)"))

bvl_plotIntervals(model, c("a0_sexid", "a_sexid[1]", "a_sexid[2]"), labels = c("a0_Sex", "a(Male)", "a(Female)"))

bvl_plotDensity(model, c("a0_sexid", "a_sexid[1]", "a_sexid[2]"), labels = c("a0_Sex", "a(Male)", "a(Female)"))

#compare school
school_names =unique(data1$affil)
school_mean = c()
school_sd = c()
school_min = c()
school_max = c()
school_25 = c()
school_97 = c()
for(schoolid in 1:8)
{
	#print(schoolid)
	#png(paste0("STEM_female_grade_",school_names[schoolid],".png"),width=1200,height=750)

	school_mean = c(school_mean, mean(fit_ss$a_sexaffil[,schoolid]))
	school_min = c(school_min, min(fit_ss$a_sexaffil[,schoolid]))
	school_max = c(school_max, max(fit_ss$a_sexaffil[,schoolid]))	
	school_sd = c(school_sd, sd(fit_ss$a_sexaffil[,schoolid]))
	
	a_quant = quantile(fit_ss$a_sexaffil[,schoolid],c(0.025, 0.50, 0.975))
	a_quant <- data.frame(t(a_quant))
	names(a_quant) <- c("Q2.5", "Q50", "Q97.5")
	
	school_25 = c(school_25, a_quant$Q2.5)
	school_97 = c(school_97, a_quant$Q97.5)
	
	#dev.off()
}  
a_df <- data.frame(school_mean, school_max, school_min, school_25, school_97, school_sd, school_names)
#round(head(a_df), 2)

a_df <- a_df[order(a_df$school_mean), ]
a_df$school_rank <- c(1 : dim(a_df)[1])

ggplot(data = a_df, 
       aes(x = school_names, 
           y = school_mean)) +
  geom_pointrange(aes(ymin = school_min, 
                      ymax = school_max),
                  position = position_dodge(width = 0.1)) +
  geom_pointrange(aes(ymin = school_25, 
                      ymax = school_97),
                  position = position_dodge(width = 1), size=1) +
  geom_hline(yintercept = mean(a_df$school_mean), 
             size = 0.5, 
             col = "red") +
  ylab("article") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
