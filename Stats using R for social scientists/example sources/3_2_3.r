# 3부. 데이터 사전처리
# 2장. 변수 관리
# 3. 리코딩하기(recoding)

# 앞서 언급했던 tess 데이터를 불러오는 것은 생략한다. 
# 우선 XREL1 변수가 어떠한가를 살펴본다. 
table(tess$XREL1, useNA = 'always')
# 리코딩된 변수를 우선 준비한다.
tess$religion5 <- tess$XREL1
# XREL1의 값에 따라 religion5변수의 값을 리코딩한다. []안의 | 표시는 or의 의미이다. 
# 즉 XREL1변수의 값의 조건에 하나라도 해당되면 우변의 값(100∼500)을 부여한다. 
tess$religion5[tess$XREL1 == 3] <- 100
tess$religion5[tess$XREL1 == 1 | tess$XREL1 == 2 | tess$XREL1 == 4 | tess$XREL1 == 9] <- 200
tess$religion5[tess$XREL1 == 10 | tess$XREL1 == 11] <- 300
tess$religion5[tess$XREL1 == 13] <- 400
tess$religion5[tess$XREL1 == 5 | tess$XREL1 == 6 | tess$XREL1 == 7 | 
	tess$XREL1 == 8 | tess$XREL1 == 12] <- 500
# 리코딩이 제대로 진행되었는지 확인한다. 
table(tess$XREL1, tess$religion5, useNA = 'always')

# 리코딩된 변수를 우선 준비한다.
tess$religion5.text <- tess$XREL1
# XREL1의 값에 따라 religion5.text변수의 값을 리코딩한다. 
tess$religion5.text[tess$XREL1 == 3] <- 'catholic'
tess$religion5.text[tess$XREL1 == 1 | tess$XREL1 == 2 | tess$XREL1 == 4 
	| tess$XREL1 == 9] <- 'protestant'
tess$religion5.text[tess$XREL1 == 10 | tess$XREL1 == 11] <- 'other.cristian'
tess$religion5.text[tess$XREL1 == 13] <- 'atheist'
tess$religion5.text[tess$XREL1 == 5 | tess$XREL1 == 6 | tess$XREL1 == 7 
	| tess$XREL1 == 8 | tess$XREL1 == 12] <- 'misc.religion'
# 리코딩이 제대로 진행되었는지 확인한다. 
table(tess$XREL1, tess$religion5.text, useNA = 'always')

# car 라이브러리가 인스톨되어 있지 않다면 install.packages('car')를 이용하여 먼저 인스톨을 시키도록 한다. 
library('car')
# recode()를 이용하여 tess$XREL1을 ""사이에 맞도록 리코딩한다. 
tess$religion5.car <- recode(tess$XREL1, "1=200; 2=200; 3=100; 4=200; 
	5=500; 6=500; 7=500; 8=500; 9=200; 10=300; 11=300; 12=500; 13=400")
# 리코딩이 제대로 진행되었는지 확인한다. 
table(tess$XREL1, tess$religion5.car, useNA = 'always')

# 앞에서 반복된 부분의 경우 설명을 생략하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", use.value.labels = FALSE, to.data.frame = TRUE)
# Q4의 무응답인 -1은 결측값으로 처리한다.  
table(tess$Q4)
# 3집단을 어떻게 나누는 것이 적절한지 판단하기 위해서 Q4의 분포를 점검하였다. dim()은 앞부분에서 설명하였다. 
# dim()으로 데이터를 살피면 첫번째 값은 가로줄 수, 그리고 두번째 값은 세로줄 수임을 알 수 있다. 
# 즉 아래의 명령문은 Q4의 각 값에 따른 비율(proportion)을 확인할 수 있다.  
table(tess$Q4)/dim(tess)[1]
# 위의 분포를 기준으로 [0,3]의 경우 'Very insensitive(100)', [4]의 경우 'Insensitive(200)', 
# [5,6] 의 경우 'Sensitive(300)', [7,10]의 경우 'Very Sensitive(400)'로 구분하였다. 
# (범주별 사례수를 비슷하게 함)
# 리코딩할 변수로 Q4r을 생성한다.
tess$Q4r <- tess$Q4
# 무응답을 처리하였다.  
tess$Q4r[tess$Q4 == -1] <- NA 
# 리코딩을 실시하였다.  []안의 &은 결합(joint)조건을 나타낸다. 
# A&B는 “A와 B의 조건을 다 만족시킬 때”라는 표현이다. 
tess$Q4r[tess$Q4 >= 0 & tess$Q4 <= 3] <- 100
tess$Q4r[tess$Q4 == 4]  <- 200
tess$Q4r[tess$Q4 == 5 | tess$Q4 == 6]  <- 300
tess$Q4r[tess$Q4 >= 7 & tess$Q4 <= 10]  <- 400
# 리코딩이 제대로 실시되었는지 체크하였다.  
table(tess$Q4, tess$Q4r, useNA = 'always')
# 각 범주별 비율을 체크하였다.  
table(tess$Q4r, useNA = 'always')/dim(tess)[1]

# 0∼10의 11개의 값을 4개로 동등하게 구획한 후 리코딩하는 경우로, [0, 2.5]는 'Very insensitive(100)', 
# (2.5,5]의 경우 'Insensitive(200)', (5,7.5]의 경우 'Sensitive(300)', 
# (7.5,10]의 경우 'Very Sensitive(400)'로 구분하였다. (범주별 스케일의 범위를 비슷하게 함)
# 리코딩할 변수로 Q4r2을 생성하고 ?1은 무응답으로 결측값처리하였다. 
tess$Q4r2 <- tess$Q4
tess$Q4r2[tess$Q4 == -1] <- NA 
# cut() 함수는 해당 벡터를 breaks 로 지정된 숫자에 맞도록 구획시켜준다. 언급했듯 총 4개의 그룹으로 나누었다. 
tess$Q4r2 <- cut(tess$Q4r2, breaks = 4)
# 리코딩이 제대로 실시되었는지 체크하였다.  
table(tess$Q4, tess$Q4r2, useNA = 'always')
# 각 범주별 비율을 체크하였다.  
table(tess$Q4r2, useNA = 'always')/dim(tess)[1]
# cut() 함수를 이용해서 3집단으로 구분하는 것을 보다 편하게 할 수도 있다. 이 경우 quantile()을 이용하면 
# 편하다. quantile()의 prob 옵션은 샘플의 비율에 맞는 지점에 해당하는 오브젝트의 값을 보고해준다. 
# 3집단을 구분하기 위해서는 4개의 포인트가 필요하다. 
# 리코딩할 변수로 Q4r3을 생성하고 ?1은 무응답으로 결측값처리하였다. 
tess$Q4r3 <- tess$Q4
tess$Q4r3[tess$Q4r3 == -1] <- NA 
# 해당변수를 3집단으로 구분할 4개의 포인트를 설정하였다. 보다 간단히 하는 방법으로는 
# prob = (0:3)/3을 이용할 수도 있다. na.rm = TRUE는 Q4r3에 결측값이 있기 때문이다. 
q4.point <- quantile(tess$Q4r3, prob = c(0, 1/3, 2/3, 1), na.rm = TRUE)
# 앞에서 저장된 q4.point에 따라 Q4r3를 나누어주었다. 
tess$Q4r3 <- cut(tess$Q4r3, q4.point)
# 앞부분에서 한 것과 cut()을 이용한 것이 같은지 확인하였다. 
table(tess$Q4r, tess$Q4r3, useNA = 'always')


