#Libraries
library(boot)
library(miscTools)

#Working Directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Import data
df=read.csv("results-survey446656_anonym.csv", sep=";")
df$AttCheckSuccess=ifelse(df$QAttentionCheck.env.=="Y" & df$QAttentionCheck.health.=="Y", 1,0)
df=df[df$AttCheckSuccess==1,]

#Function to transform into disagree, Neither, agree
transformIntoAgree=function(i_funct){
  x_funct=ifelse(i_funct=="AO01","Disagree",
                 ifelse(i_funct=="AO02","Disagree",
                        ifelse(i_funct=="AO03","Neither",
                               ifelse(i_funct=="AO04","Agree",
                                      ifelse(i_funct=="AO05","Agree",NA)))))
  x_funct
}

#Transform data
df$F1=ifelse(df$randomQuestions.Q1.==1,df$AllQuestionsS1.F1., ifelse(df$randomQuestions.Q1.==2,df$AllQuestionsS2.F1.,ifelse(df$randomQuestions.Q1.==3,df$AllQuestionsS3.F1.,df$AllQuestionsS4.F1.))) 
df$F11=ifelse(df$randomQuestions.Q2.==1,df$AllQuestionsS1.F11., ifelse(df$randomQuestions.Q2.==2,df$AllQuestionsS2.F11.,ifelse(df$randomQuestions.Q2.==3,df$AllQuestionsS3.F11.,df$AllQuestionsS4.F11.))) 
df$F12=ifelse(df$randomQuestions.Q3.==1,df$AllQuestionsS1.F12., ifelse(df$randomQuestions.Q3.==2,df$AllQuestionsS2.F12.,ifelse(df$randomQuestions.Q3.==3,df$AllQuestionsS3.F12.,df$AllQuestionsS4.F12.))) 
df$F2=ifelse(df$randomQuestions.Q4.==1,df$AllQuestionsS1.F2., ifelse(df$randomQuestions.Q4.==2,df$AllQuestionsS2.F2.,ifelse(df$randomQuestions.Q4.==3,df$AllQuestionsS3.F2.,df$AllQuestionsS4.F2.))) 
df$F21=ifelse(df$randomQuestions.Q5.==1,df$AllQuestionsS1.F21., ifelse(df$randomQuestions.Q5.==2,df$AllQuestionsS2.F21.,ifelse(df$randomQuestions.Q5.==3,df$AllQuestionsS3.F21.,df$AllQuestionsS4.F21.))) 
df$F22=ifelse(df$randomQuestions.Q6.==1,df$AllQuestionsS1.F22., ifelse(df$randomQuestions.Q6.==2,df$AllQuestionsS2.F22.,ifelse(df$randomQuestions.Q6.==3,df$AllQuestionsS3.F22.,df$AllQuestionsS4.F22.))) 
df$F23=ifelse(df$randomQuestions.Q7.==1,df$AllQuestionsS1.F23., ifelse(df$randomQuestions.Q7.==2,df$AllQuestionsS2.F23.,ifelse(df$randomQuestions.Q7.==3,df$AllQuestionsS3.F23.,df$AllQuestionsS4.F23.))) 
df$F24=ifelse(df$randomQuestions.Q8.==1,df$AllQuestionsS1.F24., ifelse(df$randomQuestions.Q8.==2,df$AllQuestionsS2.F24.,ifelse(df$randomQuestions.Q8.==3,df$AllQuestionsS3.F24.,df$AllQuestionsS4.F24.))) 
df$F3=ifelse(df$randomQuestions.Q9.==1,df$AllQuestionsS1.F3., ifelse(df$randomQuestions.Q9.==2,df$AllQuestionsS2.F3.,ifelse(df$randomQuestions.Q9.==3,df$AllQuestionsS3.F3.,df$AllQuestionsS4.F3.))) 
df$F31=ifelse(df$randomQuestions.Q10.==1,df$AllQuestionsS1.F31., ifelse(df$randomQuestions.Q10.==2,df$AllQuestionsS2.F31.,ifelse(df$randomQuestions.Q10.==3,df$AllQuestionsS3.F31.,df$AllQuestionsS4.F31.))) 
df$F32=ifelse(df$randomQuestions.Q11.==1,df$AllQuestionsS1.F32., ifelse(df$randomQuestions.Q11.==2,df$AllQuestionsS2.F32.,ifelse(df$randomQuestions.Q11.==3,df$AllQuestionsS3.F32.,df$AllQuestionsS4.F32.))) 
df$F41=ifelse(df$randomQuestions.Q13.==1,df$AllQuestionsS1.F41., ifelse(df$randomQuestions.Q13.==2,df$AllQuestionsS2.F41.,ifelse(df$randomQuestions.Q13.==3,df$AllQuestionsS3.F41.,df$AllQuestionsS4.F41.))) 
df$F42=ifelse(df$randomQuestions.Q14.==1,df$AllQuestionsS1.F42., ifelse(df$randomQuestions.Q14.==2,df$AllQuestionsS2.F42.,ifelse(df$randomQuestions.Q14.==3,df$AllQuestionsS3.F42.,df$AllQuestionsS4.F42.))) 
df$N1=ifelse(df$randomQuestions.Q15.==1,df$AllQuestionsS1.N1., ifelse(df$randomQuestions.Q15.==2,df$AllQuestionsS2.N1.,ifelse(df$randomQuestions.Q15.==3,df$AllQuestionsS3.N1.,df$AllQuestionsS4.N1.))) 
df$N2=ifelse(df$randomQuestions.Q16.==1,df$AllQuestionsS1.N2., ifelse(df$randomQuestions.Q16.==2,df$AllQuestionsS2.N2.,ifelse(df$randomQuestions.Q16.==3,df$AllQuestionsS3.N2.,df$AllQuestionsS4.N2.))) 
df$N3=ifelse(df$randomQuestions.Q17.==1,df$AllQuestionsS1.N3., ifelse(df$randomQuestions.Q17.==2,df$AllQuestionsS2.N3.,ifelse(df$randomQuestions.Q17.==3,df$AllQuestionsS3.N3.,df$AllQuestionsS4.N3.))) 
df$N4=ifelse(df$randomQuestions.Q18.==1,df$AllQuestionsS1.N4., ifelse(df$randomQuestions.Q18.==2,df$AllQuestionsS2.N4.,ifelse(df$randomQuestions.Q18.==3,df$AllQuestionsS3.N4.,df$AllQuestionsS4.N4.))) 

#Transform data
vecFrame=c("F1","F11","F12","F2","F21","F22","F23","F24","F3","F31","F32","F41","F42")
vecControlFrames=c("N1","N2","N3","N4")
vecAllFrames=c(vecFrame,vecControlFrames)
for(i in vecAllFrames){
  df[[i]]=transformIntoAgree(df[[i]])
}

#Function to compute bootstrapped p-value
bootSinglePvalue=function(B_funct,x_funct,hypothesis_funct="H01"){
  mean(sapply(1:B_funct, function(i){
    boot_data=sample(x_funct,length(x_funct),replace=TRUE)
    p_agree=mean(ifelse(boot_data=="Agree",1,0))
    p_disagree=mean(ifelse(boot_data=="Disagree",1,0))
    if(hypothesis_funct=="H01"){ 
      ifelse(p_agree<p_disagree,1,0)
    }else{
      ifelse(p_agree>p_disagree,1,0)
    }
  }))
}

#Summary statistics
set.seed(123456)
B=5000
results=matrix(data=NA,nrow=length(vecAllFrames),ncol=5)
loop_var=1
for(i in vecAllFrames){
  results[loop_var,1]=sum(df[[i]]=="Disagree")/nrow(df)
  results[loop_var,2]=sum(df[[i]]=="Neither")/nrow(df)
  results[loop_var,3]=sum(df[[i]]=="Agree")/nrow(df)
  results[loop_var,4]=bootSinglePvalue(B,df[[i]],hypothesis_funct="H01")
  results[loop_var,5]=bootSinglePvalue(B,df[[i]],hypothesis_funct="H02")
  loop_var=loop_var+1
}
rownames(results)=vecAllFrames
colnames(results)=c("Disagree","Neither","Agree","H01:p-value", "H02:p-value")
results=round(results,4)
results

write.csv(results,"resultsRepresentativeData_DD.csv")

#Additional Questions
df$HerdIncrease=transformIntoAgree(df$herdSize.incr.)
df$HerdMaintain=transformIntoAgree(df$herdSize.main.)
df$HerdDecrease=transformIntoAgree(df$herdSize.decr.)

df$StatusQuo=transformIntoAgree(df$statusQuo)

set.seed(123456)
B=5000
resultsAdditionalQuestions=matrix(data=NA,nrow=4,ncol=5)
vecVar=c("HerdIncrease","HerdMaintain","HerdDecrease","StatusQuo")
loop_var=1
for(i in vecVar){
  resultsAdditionalQuestions[loop_var,1]=sum(df[[i]]=="Disagree")/nrow(df)
  resultsAdditionalQuestions[loop_var,2]=sum(df[[i]]=="Neither")/nrow(df)
  resultsAdditionalQuestions[loop_var,3]=sum(df[[i]]=="Agree")/nrow(df)
  resultsAdditionalQuestions[loop_var,4]=bootSinglePvalue(B,df[[i]],hypothesis_funct="H01")
  resultsAdditionalQuestions[loop_var,5]=bootSinglePvalue(B,df[[i]],hypothesis_funct="H02")
  loop_var=loop_var+1
}
rownames(resultsAdditionalQuestions)=c("Increase","Maintain","Decrease","Status Quo")
colnames(resultsAdditionalQuestions)=c("Disagree","Neither","Agree","H01:p-value", "H02:p-value")
resultsAdditionalQuestions=round(resultsAdditionalQuestions,4)
resultsAdditionalQuestions

write.csv(resultsAdditionalQuestions,"resultsAdditionalQuestions_DD.csv")
