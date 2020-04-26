bb <- env_new()

bb$a <-1


bb$a;


bb <-globalenv()

a<-1;
cc <-1:10


bb$a
bb$a

bb$cc


test <- env_global()
test$test$test$test$test$cc

test$test$test$test$test$test$a


test <- NULL

test;


?parent.frame


emp <- env_empty()

emp

emp$a <- 1



aenv <- env_create()

parent.env(aenv)

benv <- env_new()


bbd <- parent.env(benv)

bbd$

bbd$res



bbd <-env_parent(benv)



bbd$res

parent.env(parent.env(parent.env(parent.env(benv))))

cenv <- new.env()
bb <-parent.env(cenv)

bb$cenv


cenv$aa <-1;


bb$cenv$aa

bb$emp

bb$cenv$aa


benv$aa <-1
benv$aa



tenv <- new.env()

bb <-env_parent(tenv)
bb$bb$bb$
  

fenv <- env_new()

genv <- env_create()

parent.env(fenv)  
parent.env(genv)  


environment()

env_current()

#
henv <- env_create()
henv$aa <-1
henv$bb <-2  
henv$cc <- 3

ls(henv)

env_ls(henv)

ls.str(henv)

bb <-env_lsStr(henv)
bb;


envs_are_identical(env_global(),env_base())

