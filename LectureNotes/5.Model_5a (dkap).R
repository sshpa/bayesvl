data1 <- read.csv("/Statistics/DKAP/DKAP1061.csv", header = TRUE)
head(data1)

data1$ict <- as.numeric(data1$a1) + as.numeric(data1$a2) + as.numeric(data1$a3) + as.numeric(data1$a4) + as.numeric(data1$a5) + as.numeric(data1$a6) + as.numeric(data1$a7) + as.numeric(data1$a8) + as.numeric(data1$a9) - 9
data1$ecostt <- ifelse(as.numeric(data1$h4_1) + as.numeric(data1$h4_2) + as.numeric(data1$h4_3) > 3, 2, 1)
data1$schoolid <- as.numeric(data1$schid)
data1$edumot <- as.numeric(data1$h2)
data1$edufat <- as.numeric(data1$h3)
data1$sex <- as.numeric(data1$f1)

data1$dr <- as.numeric(data1$b12) + as.numeric(data1$b13) + as.numeric(data1$b14) - 3

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "ict", "norm")
model <- bvl_addNode(model, "sex", "cat",test=c(1,2)) #Sex
model <- bvl_addNode(model, "ecostt", "cat")
model <- bvl_addNode(model, "edumot", "cat") #edumot
model <- bvl_addNode(model, "edufat", "cat") #edufat
model <- bvl_addNode(model, "schoolid", "cat") #school

model <- bvl_addArc(model, "sex",  "ict", "slope")
model <- bvl_addArc(model, "ecostt",  "ict", "slope")
model <- bvl_addArc(model, "edumot",  "ict", "slope")
model <- bvl_addArc(model, "edufat",  "ict", "slope")

model <- bvl_addArc(model, "schoolid", "ict", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_plotTrace(model)


school_names = c("Dai Cuong, Ha Noi","Hop Thanh, Ha Noi","Thang Long, Ha Noi","Nguyen Trai, Ha Noi","Lao Cai city no1, Lao Cai","Bao Thang, Lao Cai","Bac Ha no1, Lao Cai","Si Ma Cai no1, Lao Cai","Ong Ich Khiem, Da Nang","Tran Phu, Da Nang","Ngu Hanh Son, Da Nang","Thai Phien, Da Nang","Tran Phu, Lam Dong","Don Duong, Lam Dong","Duc Trong, Lam Dong","Lang Biang, Lam Dong","Tran Dai Nghia, Can Tho","Nguyen Viet Hong, Can Tho","Luu Huu Phuoc, Can Tho","Thuan Hung, Can Tho")
fit_ss <- extract(model@stanfit, permuted = TRUE) # fit_ss is a list 

school_mean = c()
school_sd = c()
school_min = c()
school_max = c()
school_20 = c()
school_80 = c()
for(schoolid in 1:length(school_names))
{
	#print(schoolid)
	school_mean = c(school_mean, mean(fit_ss$a_schoolid[,schoolid]))
	school_sd = c(school_sd, sd(fit_ss$a_schoolid[,schoolid]))
	
	a_quant = quantile(fit_ss$a_schoolid[,schoolid],c(0.025, 0.2, 0.50, 0.8, 0.975))
	a_quant <- data.frame(t(a_quant))
	names(a_quant) <- c("Q5",  "Q20", "Q50", "Q80", "Q95")
	
	school_min = c(school_min, a_quant$Q5)
	school_max = c(school_max, a_quant$Q95)
	school_20 = c(school_20, a_quant$Q20)
	school_80 = c(school_80, a_quant$Q80)
}  
a_df <- data.frame(school_mean, school_max, school_min, school_20, school_80, school_sd, school_names)
#round(head(a_df), 2)

a_df <- a_df[order(a_df$school_mean), ]
a_df$school_rank <- c(1 : dim(a_df)[1])

ggplot(data = a_df, 
       aes(x = school_names, 
           y = school_mean)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_linerange(aes(ymin=school_20,ymax=school_80),size=2) +
  geom_pointrange(aes(ymin = school_min, 
                      ymax = school_max)) + 
  geom_hline(yintercept = mean(a_df$school_mean), 
             size = 0.5, 
             col = "red") +
  ylab("alpha_school") +
  scale_x_discrete(limits=a_df$school_names)

require(gridExtra)
p1 <- bvl_plotDensity2d(model, "b_edumot_ict", "b_edufat_ict", color_scheme = "purple")
p2 <- bvl_plotDensity2d(model, "b_edufat_ict", "b_ecostt_ict", color_scheme = "orange")
grid.arrange(p1, p2, ncol=2)


######################################
# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "dr", "norm")
model <- bvl_addNode(model, "sex", "cat",test=c(1,2)) #Sex
model <- bvl_addNode(model, "ict", "cat")
model <- bvl_addNode(model, "schoolid", "cat") #school

model <- bvl_addArc(model, "sex",  "dr", "slope")
model <- bvl_addArc(model, "ict",  "dr", "slope")

model <- bvl_addArc(model, "schoolid", "dr", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)
