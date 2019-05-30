load("my_model1.rda")
library(ggplot2)
ggplot()+geom_point(aes(model$fitted.values,model$residuals))+geom_abline(intercept = 0,slope = 0,size=2,color='blue')+labs(title="Residuals vs Fitted Values",y="residuals",x='Fitted Values')
qqnorm(model$residuals);qqline(model$residuals)
