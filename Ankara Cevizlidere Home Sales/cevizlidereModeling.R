# library(readxl)
# install.packages('xlsx')
# library(xlsx)
# install.packages('XLConnect')
# library(XLConnect)
library("nnet")
install.packages('mlbench')
library(mlbench)
install.packages('gtools')
library(gtools)
install.packages('caret') 
library(caret)
install.packages('readxl')
library(readxl)
install.packages(car)
library('car')
install.packages('earth')
library(earth)

# Read the excel files
cevizlidere<-read_excel("denemegayrimenkul_cevizlidere.xlsx")
fiyat_alan<-read_excel("cevizlidere_alan_fiyat.xlsx")
test_cevizlidere<-read_excel("test_cevizlidere.xlsx")
alanlar<-fiyat_alan[,1]
fiyatlar<-fiyat_alan[,2]
train_alan<-alanlar[1:41,1]
test_alan<-alanlar[42:56,1]
train_fiyat<-fiyatlar[1:41,1]
test_fiyat<-fiyatlar[42:56,1]
alan_n3<-(alanlar-min(alanlar))/(max(alanlar)-min(alanlar))
fiyat_n4<-(fiyatlar-min(fiyatlar))/(max(fiyatlar)-min(fiyatlar))
alan_n3_train<-alan_n3[1:41,1]
alan_n3_test<-alan_n3[42:56,1]
fiyat_n4_train<-fiyat_n4[1:41,1]
fiyat_n4_test<-fiyat_n4[42:56,1]
cevizlidere[1]=alan_n3_train
cevizlidere[2]=fiyat_n4_train
test_cevizlidere[1]=alan_n3_test
test_cevizlidere[2]=fiyat_n4_test
# s1<-scale(cevizlidere$Fiyat)
# s2<-scale(cevizlidere$Alan)
# cevizlidere[1]= abs(s2)
# cevizlidere[2]= abs(s1)
trainset <- cevizlidere[1:41,]

plot(cevizlidere[,2], type="l")

lm.fit<-lm(Fiyat~.,data = cevizlidere)
lm.fit2<-lm(Fiyat~ Alan+I(Alan^2)+Konum+BinaYasi+Cephe+Ulasilabilirlik+BulunduguKat+Yapikalitesiankastre+TicariYogunluk
                       +KonutTürü+SosyalDonatilaraYakinlik+Prestij+TeknikDonanim+Otopark, data = cevizlidere)
install.packages("corrplot")
library(corrplot)
corrplot(cor(cevizlidere), method="number", tl.cex=0.5)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt)
}
pairs(cevizlidere,lower.panel=panel.cor,pch=18)
#lm.fit <- lm(Fiyat~Alan+Konum+BinaYasi+Cephe+Ulasilabilirlik+BulunduguKat+Yapikalitesiankastre+TicariYogunluk
#            +KonutTürü+SosyalDonatilaraYakinlik+Prestij+TeknikDonanim+Otopark, data=cevizlidere)
lm.fit
coef(lm.fit)
predict(lm.fit2,data.frame(Alan=90, Konum=0.4, BinaYasi=0.4, Cephe=1, Ulasilabilirlik=0.7,
                          BulunduguKat=1, Yapikalitesiankastre=0.8, TicariYogunluk=1,
                          KonutTürü=0.55, SosyalDonatilaraYakinlik=1, Prestij=0, TeknikDonanim=0.5, Otopark=1),
        interval="confidence")

residualPlots(lm.fit)

layout(matrix(1:4,2,2)) 
plot(lm.fit2)
# The plot in the upper left shows the residual errors plotted versus their fitted values. The
# residuals should be randomly distributed around the horizontal line representing a
# residual error of zero; that is, there should not be a distinct trend in the distribution of
# points. The plot in the lower left is a standard Q-Q plot, which should suggest that the
# residual errors are normally distributed. The scale-location plot in the upper right shows
# the square root of the standardized residuals (sort of a square root of relative error) as a
# function of the fitted values. Again, there should be no obvious trend in this plot.
# Finally, the plot in the lower right shows each points leverage, which is a measure of its
# importance in determining the regression result. Superimposed on the plot are contour
# lines for the Cook’s distance, which is another measure of the importance of each
# observation to the regression. Smaller distances means that removing the observation has
# little affect on the regression results. Distances larger than 1 are suspicious and suggest
# the presence of a possible outlier or a poor model. 

# id.n – id most influential observation
# id.cex – font size for id.
# Graphs outcome vs predictor variables holding the rest constant (also called partial-regression plots)
# Help identify the effect (or influence) of an observation on the regression coefficient of the predictor variable
avPlots(lm.fit, id.n=2, id.cex=0.7)
qqPlot(lm.fit, id.n=3)
outlierTest(lm.fit)
influenceIndexPlot(lm.fit, id.n=3)
# Cook's distance measures how much an observation influences the overall model or predicted values
# Studentizided residuals are the residuals divided by their estimated standard deviation as a way to standardized
# Bonferroni test to identify outliers
# Hat-points identify influential observations (have a high impact on the predictor variables)
influencePlot(lm.fit, id.n=3)
# Creates a bubble-plot combining the display of Studentized residuals, hat-values, and Cook's
# distance (represented in the circles).
ncvTest(lm.fit)
# testing for heteroskedasticity
# Breush/Pagan and Cook/Weisberg score test for non-constant error variance. Null is constant variance
# See also residualPlots(lm.fit).
vif(lm.fit)
# testing for multicolinearity
# A gvif> 4 suggests collinearity.
# “When there are strong linear relationships among the predictors in a regression analysis, the
# precision of the estimated regression coefficients in linear models declines compared to what it
# would have been were the predictors uncorrelated with each other” (Fox:359)


lmfit <- train(Fiyat~., trainset, method="lm") 
lmfit

#glm.fit<-glm(Fiyat~., data=cevizlidere, family=poisson())

# MARS Algorithm
earth.mod<-earth(Fiyat~.,data=cevizlidere)
plotmo(earth.mod)
summary(earth.mod) #digits=2)#,style = "pmax"
# m =matrix(c(170,110,140,147,135,140,200,120,120,208,150,157,165,145,145,340000,230000,350000,265000,315000,340000,
#             335000,265000,350000,410000,450000,310000,350000,370000,285000),nrow = 15,ncol = 2)
# n3<-(m[,1]-min(m[,1]))/(max(m[,1])-min(m[,1]))
# n4<-(m[,2]-min(m[,2]))/(max(m[,2])-min(m[,2]))
# bm<-s3*attr(s3,'scaled:scale')+attr(s3,'scaled:center')
pred<-predict(earth.mod,data.frame(Alan=90, Konum=0.4, BinaYasi=0.2, Cephe=0.1, Ulasilabilirlik=0.5,
                          BulunduguKat=1, Yapikalitesiankastre=0.8, TicariYogunluk=1,
                          KonutTürü=0.55, SosyalDonatilaraYakinlik=1, Prestij=0, TeknikDonanim=0.5, Otopark=1))
pred
tahmin_fiyat <- pred*(max(fiyatlar)-min(fiyatlar))+min(fiyatlar)
tahmin_fiyat
head(resid(earth.mod, type="earth"))

# Random Forest Algorithm
install.packages("randomForest")
library(randomForest)
forestFit <- randomForest(Fiyat~.,data=cevizlidere)
forestPredict <- predict(forestFit,data.frame(Alan=105, Konum=0.4, BinaYasi=0.4, Cephe=0.1, Ulasilabilirlik=0.5,
                          BulunduguKat=1, Yapikalitesiankastre=0.8, TicariYogunluk=1,
                          KonutTürü=0.55, SosyalDonatilaraYakinlik=1, Prestij=0, TeknikDonanim=0.5, Otopark=1))
forestPredict
diff <- forestPredict - housingTesting$MDEV

# SVM
install.packages('e1071')
library(e1071)
sv<-svm(Fiyat~.,data=cevizlidere)
pred.svm<-predict(sv,data.frame(Alan=110, Konum=1, BinaYasi=0.7, Cephe=0.3, Ulasilabilirlik=0.7,
                        BulunduguKat=0.7, Yapikalitesiankastre=0.8, TicariYogunluk=1,
                        KonutTürü=0.55, SosyalDonatilaraYakinlik=1, Prestij=0.75, TeknikDonanim=0.8, Otopark=1))
pred.svm

# Neural Network Training
library(nnet)
library(caret)

x<-set.seed(0)
a<-nnet(Fiyat~I(Alan^2)+Konum+I(BinaYasi^2)+Cephe+Ulasilabilirlik+BulunduguKat+Yapikalitesiankastre+TicariYogunluk
  +KonutTürü+SosyalDonatilaraYakinlik+Prestij+TeknikDonanim+Otopark,data=cevizlidere
  ,size = 1, rang = 0.1, decay = 5e-4, maxit = 2000)#decay=0.001,maxit=5000)
b<-predict(a,test_cevizlidere)
b
# sonrasında bulunan index*(max fiyat-min fiyat)+min fiyat, olması gereken fiyatı verecektir
# [,1]
# 1  0.443988173 (326000)
# 2  0.024204866 (235280)
# 3  0.001109445
# 4  0.087828237 (249000)
# 5  0.091279784
# 6  0.001109445
# 7  0.798315009
# 8  0.005124518
# 9  0.000692856
# 10 0.897034459
# 11 0.264283322
# 12 0.010823702
# 13 0.620385447
# 14 0.001656639
# 15 0.076311552
# Wts initial parameter vector. If missing chosen at random.

### neuralnet package ###
install.packages('neuralnet')
library(neuralnet)
dim(cevizlidere)
nn<-neuralnet(Fiyat~Alan+Konum+BinaYasi+Cephe+Ulasilabilirlik+BulunduguKat+Yapikalitesiankastre+TicariYogunluk
        +KonutTürü+SosyalDonatilaraYakinlik+Prestij+TeknikDonanim+Otopark,data=cevizlidere,hidden=7,
        err.fct="sse",linear.output=FALSE)
nn
plot(nn)
nn$net.result
nn$weights
nn$result.matrix

nn$covariate
cevizlidere$Fiyat
nn$net.result[[1]]
### For Classified Outputs ###
# for classify nn1=ifelse(nn$net.result[[1]]>0.5,1,0)
# nn1
# misClassificationError=mean(cevizlidere$Fiyat!=nn1)
# misClassificationError
# OutPutVsPred=cbind(cevizlidere$Fiyat,nn1)
# OutPutVsPred
### Classic Backpropagation ###
nn.bp=neuralnet(Fiyat~Alan+Konum+BinaYasi+Cephe+Ulasilabilirlik+BulunduguKat+Yapikalitesiankastre+TicariYogunluk
                +KonutTürü+SosyalDonatilaraYakinlik+Prestij+TeknikDonanim+Otopark,data=cevizlidere,hidden=7,
                learningrate=0.01, algorithm="backprop", err.fct="ce",linear.output=FALSE)
nn #nn.bp has better result if we consider error
plot(nn.bp)
nn.bp
nn.bp$net.result #a list containing the overall result of the neural network for every repetition.(predictions)
nn.bp$weights
nn.bp$result.matrix

nn.bp$covariate
cevizlidere$Fiyat
nn.bp$net.result[[1]]
OutPutVsPred=cbind(cevizlidere$Fiyat,nn.bp$net.result[[1]])
OutPutVsPred
### Predictions ###
test.a<-matrix(c(test_cevizlidere[,1],test_cevizlidere[,3],test_cevizlidere[,4],test_cevizlidere[,5],
            test_cevizlidere[,6],test_cevizlidere[,7],test_cevizlidere[,8],test_cevizlidere[,9],
            test_cevizlidere[,10],test_cevizlidere[,11],test_cevizlidere[,12],test_cevizlidere[,13],
            test_cevizlidere[,14]),nrow=15,ncol=13)
test.a2<-test_cevizlidere[,-2] # deletye column 2
##nn.bp[[i]] = as.matrix(nn.bp[[i]])
new.output=compute(nn.bp,covariate=test.a2)
result=cbind(test_cevizlidere$Fiyat,new.output$net.result)
result
#new.result=result*(550000-21000)+210000
#new.result
a<-lapply(lapply(test_cevizlidere$Fiyat,"*",max(fiyatlar)-min(fiyatlar)),"+",min(fiyatlar))
b<-lapply(lapply(new.output$net.result,"*",max(fiyatlar)-min(fiyatlar)),"+",min(fiyatlar))
res=cbind(a,b)
res
myList<-list();
myList[[1]]<-matrix(a,15,1)
myList[[2]]<-matrix(b,15,1)
v<-as.numeric(myList[[1]])
w<-as.numeric(myList[[2]])
deviation=(v-w)/v
deviation
positif.sapmalar<-abs(deviation)
ortalama.sapma<-mean(positif.sapmalar)
ortalama.sapma

length(deviation[abs(deviation)<=0.1])

### Visualize the Results ###
par(mfrow=c(4,4))
gwplot(nn.bp,selected.covariate = "Alan",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "Konum",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "BinaYasi",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "Cephe",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "Ulasilabilirlik",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "BulunduguKat",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "Yapikalitesiankastre",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "TicariYogunluk",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "KonutTürü",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "SosyalDonatilaraYakinlik",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "Prestij",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "TeknikDonanim",min=-5,max=15)
gwplot(nn.bp,selected.covariate = "Otopark",min=-5,max=15)

#glm<-glm(Fiyat~.,data=test_cevizlidere)
#b<-prediction(a,glm)
#b
