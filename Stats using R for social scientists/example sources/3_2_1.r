# 3부. 데이터 사전처리
# 2장. 변수 관리
# 1. 결측값(missing values) 처리

# 결측값 다루기 
# PEW 데이터를 불러오기
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# table()은 응답수준별로 총 몇 개의 사례가 있는지 보여주는 함수이다. 데이터의 변수 중 
# q16의 값들이 무엇이 있는지 확인하였다. 
table(pew$q16)
# q16과 동일한 변수이지만 이름이 다른 q16m을 생성하였다. 이렇게 하는 이유는 q16을 바로 바꿀 경우 원데이터가
# 훼손되기 때문이다. 
pew$q16m <- pew$q16 
# []표시에서 알 수 있듯, 인덱싱을 이용하였다. 언급하였듯 8과 9일 경우 결측값을 부여하였다. 
pew$q16m[pew$q16 == 8] <- NA 
pew$q16m[pew$q16 == 9] <- NA
# 조금 더 간단한 프로그래밍은 아래와 같다. 
pew$q16m[pew$q16 >= 8] <- NA
# table()로 q16과 q16m의 교차표(crosstab)을 그려보았다. useNA 옵션은 결측값도 교차표에 보고하라는 
# 의미이다. 해당 옵션 없이 실행을 시켜보면 useNA 옵션의 역할을 확실하게 알 수 있을 것이다. 
table(pew$q16, pew$q16m, useNA = 'always')

# 우선 q16m과 동일한 변수인 q16m99를 만들어 보자. 
pew$q16mb <- pew$q16m
# is.na()는 해당 벡터에서 결측값인 사례와 그렇지 않은 사례를 각각 FALSE, TRUE로 나타내준다. 
# 즉 q16m에서 결측값일 경우는 TRUE로, 1∼4의 실측값의 경우는 FALSE가 된다. 
# 인덱싱을 통해 q16m이 결측값인 조건에 99라는 값을 부여하였다. 
pew$q16mb[is.na(pew$q16m)] <- 99
# table()로 q16m과 q16mb의 교차표(cross-tabulation)을 그려보았다. 
table(pew$q16m, pew$q16mb, useNA = 'always')




