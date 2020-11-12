library(lmerTest)
library(magrittr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(Hmisc) 
library(mediation) 

image_ratings <- read.table("image_ratings.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
image_stats <- read.table("image_stats.csv", sep=",", header=TRUE)
image_stats$gvHue <- sin(image_stats$hue)
image_stats$rcHue <- cos(image_stats$hue)

all.equal(image_ratings$picture.ID,image_stats$picture.no) # checking that images in image_ratings & image_stats match
means_data <- cbind(image_ratings, image_stats)

## Aim 1
#correlation matrix
ntr_llvps <- dplyr::select(means_data,fascination,coherence,scope,being_away,gvHue,rcHue,sat,SDsat,bright,SDbright,ED,PE)
ntr_llvps <- ntr_llvps[!is.na(ntr_llvps$sat),]

rcorr(as.matrix(ntr_llvps),type=c("spearman"))

mydata.cor <- cor(ntr_llvps, method=c("spearman"))
sig.test <- cor.mtest(ntr_llvps, conf.level=0.95)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mydata.cor, method="color", col=col(200), type="upper", 
         number.cex=.7, addCoef.col = "black", tl.col="black", 
         tl.srt=90, p.mat=sig.test$p, sig.level=0.05, insig="blank")

#regressions
fsc <- lm(fascination ~ gvHue+rcHue+sat+SDsat+bright+SDbright+ED+PE, data=means_data); summary(fsc)
chr <- lm(coherence ~ gvHue+rcHue+sat+SDsat+bright+SDbright+ED+PE, data=means_data); summary(chr)
scp <- lm(scope ~ gvHue+rcHue+sat+SDsat+bright+SDbright+ED+PE, data=means_data); summary(scp)
bng <- lm(being_away ~ gvHue+rcHue+sat+SDsat+bright+SDbright+ED+PE, data=means_data); summary(bng)

## Aim 2
f_n <- lm(fascination ~ naturalness, data=means_data); summary(f_n)
c_n <- lm(coherence ~ naturalness, data=means_data); summary(c_n)
s_n <- lm(scope ~ naturalness, data=means_data); summary(s_n)
b_n <- lm(being_away ~ naturalness, data=means_data); summary(b_n)

f_nC <- lm(fascination ~ naturalness + coherence + scope + being_away, data=means_data); summary(f_nC)
c_nC <- lm(coherence ~ naturalness + fascination + scope + being_away, data=means_data); summary(c_nC)
s_nC <- lm(scope ~ naturalness + fascination + coherence + being_away, data=means_data); summary(s_nC)
b_nC <- lm(being_away ~ naturalness + coherence + scope + fascination, data=means_data); summary(b_nC)

f_plot <- ggplot(means_data, aes(x=fascination, y=naturalness)) + geom_point(size=0.8) + geom_smooth(method=lm) + xlab("Fascination") + ylab("Naturalness")
c_plot <- ggplot(means_data, aes(x=coherence, y=naturalness)) + geom_point(size=0.8) + geom_smooth(method=lm) + xlab("Coherence") + ylab("Naturalness")
s_plot <- ggplot(means_data, aes(x=scope, y=naturalness)) + geom_point(size=0.8) + geom_smooth(method=lm) + xlab("Scope") + ylab("Naturalness")
b_plot <- ggplot(means_data, aes(x=being_away, y=naturalness)) + geom_point(size=0.8) + geom_smooth(method=lm) + xlab("Being-away") + ylab("Naturalness")
grid.arrange(f_plot, c_plot, s_plot, b_plot, nrow=2)

## Aim 3
# fascination
model.M <- lm(gvHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + gvHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="gvHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(rcHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + rcHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="rcHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(sat ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + sat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="sat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDsat ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + SDsat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDsat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(bright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + bright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="bright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDbright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + SDbright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDbright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(ED ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + ED, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="ED", boot=TRUE, sims=10000); summary(results)

model.M <- lm(PE ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(fascination ~ naturalness + PE, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="PE", boot=TRUE, sims=10000); summary(results)

# coherence 
model.M <- lm(gvHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + gvHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="gvHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(rcHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + rcHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="rcHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(sat ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + sat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="sat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDsat ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + SDsat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDsat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(bright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + bright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="bright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDbright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + SDbright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDbright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(ED ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + ED, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="ED", boot=TRUE, sims=10000); summary(results)

model.M <- lm(PE ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(coherence ~ naturalness + PE, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="PE", boot=TRUE, sims=10000); summary(results)

# scope
model.M <- lm(gvHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + gvHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="gvHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(rcHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + rcHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="rcHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(sat ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + sat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="sat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDsat ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + SDsat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDsat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(bright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + bright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="bright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDbright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + SDbright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDbright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(ED ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + ED, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="ED", boot=TRUE, sims=10000); summary(results)

model.M <- lm(PE ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(scope ~ naturalness + PE, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="PE", boot=TRUE, sims=10000); summary(results)

# being-away
model.M <- lm(gvHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(being_away ~ naturalness + gvHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="gvHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(rcHue ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(being_away ~ naturalness + rcHue, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="rcHue", boot=TRUE, sims=10000); summary(results)

model.M <- lm(sat ~ naturalness, data=means_data); summaset.sry(model.M)
model.Y <- lm(being_away ~ naturalness + sat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="sat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDsat ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(being_away ~ naturalness + SDsat, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDsat", boot=TRUE, sims=10000); summary(results)

model.M <- lm(bright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(being_away ~ naturalness + bright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="bright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(SDbright ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(being_away ~ naturalness + SDbright, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="SDbright", boot=TRUE, sims=10000); summary(results)

model.M <- lm(ED ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(being_away ~ naturalness + ED, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="ED", boot=TRUE, sims=10000); summary(results)

model.M <- lm(PE ~ naturalness, data=means_data); summary(model.M)
model.Y <- lm(being_away ~ naturalness + PE, data=means_data); summary(model.Y)
results <- mediate(model.M, model.Y, treat="naturalness", mediator="PE", boot=TRUE, sims=10000); summary(results)