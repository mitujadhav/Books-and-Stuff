#--------------------------------------------------------------------
# Used mtcars dataset for predicting type of car automatic or manual
# variable "mtcars$am" as dependent variable
# variables mtcars$wd and mtcars$hp as independent variables
#--------------------------------------------------------------------

 y<-mtcars$am
 x<-mtcars$hp
 z<-mtcars$wt
 newdata = data.frame(hp=120, wt=2.8)
 
 #------------------------------------------------------
 # Basic Formula of LM function
 # y = beta0 + beta1*x
 #------------------------------------------------------

 # coefficients of lm function :
 fit<-lm(am~hp,data=mtcars)
 coef(fit)

 # Maths behind lm function : 
 # beta1 (here hp)
 beta1<-cor(y,x)*sd(y)/sd(x)
 beta1
 
 # Intercept i.e. beta0 :
 intercept<-mean(y)-beta1*mean(x)
 intercept
 
 beta2<-cor(y,z)*sd(y)/sd(z)
 beta2

 intercept2<-mean(y)-beta2*mean(z)
 intercept2
 
 #------------------------------------------------------
 
 #predicted value by predict function : 
 predict(fit, newdata, type="response")
 
 # Formula behind predict function:
 intercept + (beta1*newdata$hp)

 # Multiple independent variables
 z1<-lm(am~hp+wt,data = mtcars) 
 plot(mtcars$am,mtcars$wt+mtcars$hp)
 abline(z1) # equivalent to abline(reg = z) or
 abline(coef = coef(z))
 
 lm(am~hp,data = mtcars) 
 lm(am~wt,data = mtcars)
 
 #------------------------------------------------------
 # Basic Formula of GLM function
 # bx = beta0 + beta1*x + ....
 # p = 1/(1+exp(-bx))
 # Note : GLM Uses log of odds fuction rather than probability.
 #------------------------------------------------------
 
 am.glm = glm(formula=am ~ hp + wt,data=mtcars,family=binomial)
 coef(am.glm)
 # Prediction by using predict() function :
 predict(am.glm, newdata, type="response")
 
 # Maths behind GLM predict function :
 bx<-coef(am.glm)[1] + (coef(am.glm)[2]*newdata$hp) + (coef(am.glm)[3]*newdata$wt)
 predictedval2<-1/(1+exp(-bx))
 predictedval2
 
 #-------------------------------
 
 x<-c(1,2,3,4,5)
 y<-c(2,4,5,4,5)
 cor(y,x)
 cor(x,y)

 correlation<-(6/4 )/(sd(x)*sd(y))
 correlation

 z <- lm(dist ~ speed, data = cars)
 plot(cars)
 abline(z) # equivalent to abline(reg = z) or
 abline(coef = coef(z))




xtabs(am ~ hp + wt, data = mtcars)
sort(mtcars$wt)



 
 
 
 
 
 