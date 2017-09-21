# 4부. 통계분석
# 2장. 단일변량 기술통계분석
# 3. 변수의 변이(dispersion) 관련 통계값

# 변수의 평균과  표준편차 
# 반복되는 부분에 대해서는 설명을 생략한다.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
table(pew$q16)
pew$q16m <- pew$q16 
pew$q16m[pew$q16 == 8 | pew$q16 == 9] <- NA 
# 분산계산을 위한 가장 쉬운 함수는 var()이다. 
var(pew$q16m, na.rm = TRUE)
# 표준편차계산을 위한 가장 쉬운 함수는 sd()이다. 
sd(pew$q16m, na.rm = TRUE)
# 평균과 사례수 정보를 이용하여 분산을 구할 수 있다. 
mean.q16m <- mean(pew$q16m, na.rm = TRUE)
length.q16m <- length(pew$q16m[!is.na(pew$q16m)]) 
# 표본의 표준편차의 경우 사례수에서 1을 빼준다. 이에 대한 자세한 설명은 통계학 이론을 참조하라.
# ^2는 제곱을 한다는 뜻이다. 비슷하게 ^3은 세제곱을, ^4는 네제곱을 뜻한다.  
var.q16m <- sum((pew$q16m[!is.na(pew$q16m)] - mean.q16m)^2)/(length.q16m-1)
var.q16m
# 분산에 제곱근을 취하면 표준편차가 된다. sqrt()는 오브젝트의 표준편차를 구하는 함수이다.  
sqrt(var.q16m)

# 변수의 범위를 구하는 가장 쉬운 방법은 range()을 이용하는 것이다. 
range(pew$q16m, na.rm = TRUE)
# 25%째, 75%째의 경우 quantile()을 이용하면 간단하다.  
quantile(pew$q16m, na.rm = TRUE)
# 만약 10%와 90%와 같은 경우  prob 옵션으로 원하는  범위를 결정할 수 있다. 
quantile(pew$q16m, prob = c(0.1, 0.9), na.rm = TRUE)
# 데이터 정렬과 인덱싱을 이용하여 조금 복잡하게 프로그래밍하는 것도 가능하다.  
length.q16m <- length(pew$q16m[!is.na(pew$q16m)]) 
# 1번째와 가장 마지막의 변수값을 보고한다. 즉 변수의 최소값과 최대값을 보고하는 range()와 동일하다. 
range.q16m <- pew$q16m[order(pew$q16m, na.last = TRUE)][c(1,length.q16m)]
range.q16m
# 25%째와 75%째의 변수값을 보고한다. 즉 prob옵션을 별도로 지정하지 않는 quantile()과 동일하다. 
interquartile.q16m <- pew$q16m[order(pew$q16m, na.last = TRUE)][c(round(0.25*length.q16m,digit=0),round(0.75*length.q16m,digit=0))]
interquartile.q16m

# 기술통계치를 요약해서 보여주는 함수생성 
descriptive.statistics <- function(myvariable) {
  # 결측값을 포함하는 전체사례수 
  myvar.length.missing.include <- length(myvariable)
  # 결측값을 제외하는 전체사례수
  myvar.length.missing.exclude <- length(myvariable[!is.na(myvariable)])
  # 결측값제외한 변수의 값 총합 
  myvar.sum <- sum(myvariable, na.rm = TRUE)
  # 결측값 제외한 변수의 산술평균 
  myvar.mean <- mean(myvariable, na.rm = TRUE)
  # 결측값 제외한 변수의 중위값 
  myvar.median <- median(myvariable, na.rm = TRUE)
  # 결측값 제외한 변수의 최빈값 
  table.myvariable <- table(myvariable) 
  myvar.mode <- as.numeric(names(table.myvariable)[which.max(table.myvariable)])
  # 결측값을 제외한 변수의 최소값, range()의 경우 첫번째는 최소값을 두번째는 최대값을 보고하여준다.  
  myvariable.range.min <- range(myvariable, na.rm = TRUE)[1]
  # 결측값을 제외한 변수의 최대값
  myvariable.range.max <- range(myvariable, na.rm = TRUE)[2]  
  # 결측값을 제외한 변수의 25%째의 값
  myvariable.interquartile25 <- quantile(myvariable, prob = c(0.25, 0.75), na.rm = TRUE)[1]
  # 결측값을 제외한 변수의 75%째의 값
  myvariable.interquartile75 <- quantile(myvariable, prob = c(0.25, 0.75), na.rm = TRUE)[2]
  # 결측값을 제외한 변수의 분산 
  myvariable.var <- var(myvariable, na.rm = TRUE)
  # 결측값을 제외한 변수의 표준편차 
  myvariable.sd <- sd(myvariable, na.rm = TRUE)
  # cbind()를 이용해서 각 통계치들을 묶어낸 후 descriptive.myvariable이라는 오브젝트로 저장하였다.  
  descriptive.myvariable <- rbind(myvar.mean,myvar.median,myvar.mode,
        myvariable.range.min,myvariable.range.max,
        myvariable.interquartile25,myvariable.interquartile75,
        myvariable.var,myvariable.sd,myvar.sum,
        myvar.length.missing.exclude,myvar.length.missing.include)
  # rownames()는 descriptive.myvariable의 이름을 설정하는 함수이다. 순서에 맞도록 이름을 붙였다.  
  rownames(descriptive.myvariable) <- c('산술평균','중위값','최빈값',
        '최솟값','최댓값','25%값','75%값',
        '분산','표준편차','총합',
        '사례수(결측값 제외)','사례수(결측값 포함)')
  colnames(descriptive.myvariable) <- '값' 
  # 소수점 2자리만을 보고하도록 하였다.  
  round(descriptive.myvariable, digit = 2)
} 

# 위에서 점검한 q16m 변수를 실행하여 보면 위에서 했던 방식과 동일함을 알 수 있을 것이다.  
descriptive.statistics(pew$q16m)
# pew 데이터에서 '연령(age)'의 기술통계치를 구하면 다음과 같이 일목요연하게 나온다. 
descriptive.statistics(pew$age)

