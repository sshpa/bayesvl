data1<-read.csv("/Statistics/1042/1042data/1042data.csv", header = TRUE)
head(data1)
data1$SES <- ordered(data1$SES, levels = c("Lo", "Med", "Hi"))
data1$SatIns <- ordered(data1$SatIns, levels = c("Unsat", "Aver", "Satis"))
data1<-na.omit(data1)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "SatIns", "norm")
model <- bvl_addNode(model, "Res", "cat")
model <- bvl_addNode(model, "Insured", "cat")
model <- bvl_addNode(model, "SES", "cat")
model <- bvl_addNode(model, "End", "cat")

model <- bvl_addArc(model, "Res", "SatIns", "slope")
model <- bvl_addArc(model, "Insured", "SatIns", "slope")
model <- bvl_addArc(model, "SES", "SatIns", "slope")
model <- bvl_addArc(model, "End", "SatIns", "varint")


bvl_bnStrength(model, data1[c("Res","SatIns","Insured","SES","End")])
bvl_bnBayes(model, data1[c("Res","SatIns","Insured","SES","End")])
bvl_bnBarchart(model, data1[c("Res","SatIns","Insured","SES","End")])

model_string <- bvl_model2Stan(model)
cat(model_string)

#dat1042 <- with(data1,
#            list(Nobs         = length(Res),
#                 NEnd         = length(unique(End)),
#                 End          = as.numeric(End),
#                 NSES         = length(unique(SES)),
#                 SES          = as.numeric(SES),
#                 NRes         = length(unique(Res)),
#                 Res          = as.numeric(Res),
#                 NInsured     = length(unique(Insured)),
#                 Insured      = as.numeric(Insured),
#                 SatIns       = as.numeric(SatIns)))

options(mc.cores = parallel::detectCores())

# Fit the model
fit <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_trace(fit)

summary(fit)


