test_that("Questionnaire() works", {
  data<-data.frame(id=1:100,a=rbinom(100,7,0.3),b=rbinom(100,7,0.3),c=rbinom(100,7,0.3),d=rbinom(100,7,0.3))
  var<-c(id=1,a=2:4,d=5)
  test<-Questionnaire(data,var)
  test$RunCfa()
  test$itemAnalysis()
  test$AVE()
})

