library(tsdo)
nse_subset(iris,Species=='virginica')

nse_scramble(iris)


#test for function calls function

bb <-nse_subScramble(iris,Species=='virginica')
bb;



#真的是我的疑问
library(lattice)
xyplot(mpg~disp,mtcars)

x <- quote(mpg)
y <- quote(disp)
xyplot(x~y,mtcars)

#substitute 在全局环境中没有起作为

a <-1
b <-2
pryr::subs(a+b+c)
# 
# 
# f <- function(){
#   a <- 1
#   b <-2
#   substitute(a+b+c)
#   
# }


a <-1
b <-2
nse_subs(a+b+2)

library(pryr)

subs(1+b,list(b=3))

subs(1+2,list("+" = quote(f)))


x <- quote(mpg)
y <- quote(disp)
bb <-subs(xyplot(x~y,mtcars))
eval(bb)



x<-quote(a+b)
nse_substitute(x,list(a=1,b=3))




a <-1
b <-2
nse_substitute(quote(a+b+2),list(a=1,b=2))



nse_dots(a=1,b=2,c=3)
nse_dots(1,2,3,4)





