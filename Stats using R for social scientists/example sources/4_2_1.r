# 4부. 통계분석
# 2장. 단일변량 기술통계분석
# 1. 변수의 기초정보

# 변수의 사례수
# PEW 데이터를 불러온다.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# 앞에서도 다루었던 q16을 살펴본다.  
table(pew$q16)
# 8과 9의 값에 대해서는 결측값을 부여하였다.  
pew$q16m <- pew$q16 
pew$q16m[pew$q16 == 8 | pew$q16 == 9] <- NA 
# length()는  벡터로 된 오브젝트의 길이가 얼마인지 알려준다. 
length(pew$q16m)
# is.na()를 이용해서 결측값의 사례수를 알 수 있다. !is.na()의 경우 결측값이 아닌 사례수가 
# 몇 개인지 계산할 수 있다.  
length(pew$q16m[is.na(pew$q16m)])
length(pew$q16m[!is.na(pew$q16m)]) 

# 변수의 총합 
# sum()는 오브젝트의 총합이 얼마인지 계산하여 준다.  
sum(pew$q16m, na.rm = TRUE)
# sum(pew$q16m)처럼 na.rm = TRUE 옵션이 없으면 결측값으로 인해 총합이 계산되지 않는다.  
# 아래와 같이 하면 결측값이 8과 9마저 실제값처럼 계산된다. 즉 계산은 가능하나 현실적 의미가 없는 값이 된다.  
sum(pew$q16)

  	