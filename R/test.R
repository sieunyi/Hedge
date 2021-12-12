library(readr)
load("~/new folder/Hedge/data/DATAOBJECT.rda")

x = cbind(DATAOBJECT$...1, DATAOBJECT$...2)

# obtain the hr and he when WinLen =15
result <- mvhr(x, WinLen=30)
plot(result$HE, type = 'l')
plot(result$HR, type = 'l')
result_sv <- svhr(x, WinLen=30)
plot(result_sv$HE_sv, type = 'l', col=1)
lines(result$HE, type='l', col=2)

plot(result_sv$HR_sv, type= 'l')


### to do

#data format 바꾸기
#r-cmd --> push ---> badge
#testthat::
#  readme file 바꾸기

