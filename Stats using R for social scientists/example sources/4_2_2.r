# 4부. 통계분석
# 2장. 단일변량 기술통계분석
# 2. 변수의 중심치(central tendency) 관련 통계값

# 변수의 평균 
# 반복되는 부분에 대해서는 설명을 생략한다.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
table(pew$q16)
pew$q16m <- pew$q16 
pew$q16m[pew$q16 == 8 | pew$q16 == 9] <- NA 
# 가장 쉽게는 mean()을 이용하는 것이다. sum()와 마찬가지로 결측값을 처리하는 옵션이 바로 na.rm이다.  
mean(pew$q16m, na.rm = TRUE)
# 혹은 총합과 사례수 정보를 이용하여 산술평균을 구할 수 있다.  
sum.q16m <- sum(pew$q16m, na.rm = TRUE)
length.q16m <- length(pew$q16m[!is.na(pew$q16m)]) 
sum.q16m/length.q16m 

# 변수의 중심값 
# 가장 쉽게는 mean()을 이용하는 것이다. sum()와 마찬가지로 결측값을 처리하는 옵션이 바로 na.rm이다.  
median(pew$q16m, na.rm = TRUE)
# 혹은 변수를 정렬한 후에 사례수 정보를 이용하여 중심값을 프로그램하는 것도 가능하다. 
# mid.q16m은 q16m의 중값값의 위치정보를 나타낸다. round()는 반올림을 수행한다. 
# 변수의 수가 홀수인 경우의 중심순위를 계산하기 위함이다.  
mid.q16m <- round(length(pew$q16m[!is.na(pew$q16m)])/2, digit=0)
# order()는 해당 오브젝트를 정렬한다. na.last = TRUE의 경우 결측값은 마지막에 위치한다. 
# length()에서 결측값이 제외되어 있기 때문에 na.last = TRUE가 없이는 계산이 잘못될 수 있다. 
# [mid.q16m]은 정렬된 q16m의 중간값을 인덱싱해준다.  
pew$q16m[order(pew$q16m, na.last = TRUE)][mid.q16m]

# 변수의 최빈값 
# 가장 간단하게는 table()을 이용해서 어떤 값에 가장 많은 사례수가 있는지를 눈으로 확인하는 것이다. 
table(pew$q16m, useNA = 'always')
# 조금 더 복잡하게 R프로그래밍을 하는 것도 가능하다.  
# 1. which.max()는 오브젝트에서 가장 큰 사례수의 위치를 나타낸다. 
# 2. [which.max()] 를 통해서 오브젝트에서 가장 큰 사례수가 있는 위치에 해당되는 값을 인덱싱해준다.  
table.q16m <- table(pew$q16m, useNA = 'always')
names(table.q16m)[which.max(table.q16m)]
# which.min()은 가장 작은 값이 무엇인지 나타낸다. 

# 변수의 최희값(最稀値, 가장 드문 값)
# 흔히 사용되는 통계치는 아니지만, 최희값 역시 비슷하게 구할 수 있다.  
table.q16m <- table(pew$q16m, useNA = 'always')
names(table.q16m)[which.min(table.q16m)]
# useNA옵션이 없으면 최희값은 다르게 나타난다. 
table.q16m <- table(pew$q16m)
names(table.q16m)[which.min(table.q16m)]

