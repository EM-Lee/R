# 4부. 통계분석
# 3장. 다변량 추리통계분석
# 4. 분산분석

# 일원분산분석
# 앞에서 반복된 부분의 경우 설명을 생략하였다.
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav",
use.value.labels = FALSE, to.data.frame = TRUE)
# 실험설계에 대한 설명은 데이터와 함께 첨부된 문서를 참조할 것.
# 실험자극은 총 24개이며, 남성의 경우는 1~12까지의 남성 이미지를 받았고, 여성의 경우는
# 13~24까지의 여성 이미지를 받았다. 제시된 이미지는 남성의 경우 백인남성(1,5,9번),
# 흑인남성(2,6,10번), 히스패닉남성(3,7,11번), 아시아인남성(4,8,12번)이다. 여성은
# 백인여성(13,17,21번), 흑인여성(14,18,22번), 히스패닉여성(15,19,23번),
# 아사이인여성(16,20,24번)이다. 여기서는 제시된 이미지의 인종 8개를 실험자극으로 하였다.
table(tess$XTESS089, useNA = 'always')
tess$target.race <- tess$XTESS089
tess$target.race[tess$XTESS089 == 1| tess$XTESS089 == 5| tess$XTESS089 == 9] <- 1
tess$target.race[tess$XTESS089 == 2| tess$XTESS089 == 6| tess$XTESS089 == 10] <- 2
tess$target.race[tess$XTESS089 == 3| tess$XTESS089 == 7| tess$XTESS089 == 11] <- 3
tess$target.race[tess$XTESS089 == 4| tess$XTESS089 == 8| tess$XTESS089 == 12] <- 4
tess$target.race[tess$XTESS089 == 13| tess$XTESS089 == 17| tess$XTESS089 == 21] <- 5
tess$target.race[tess$XTESS089 == 14| tess$XTESS089 == 18| tess$XTESS089 == 22] <- 6
tess$target.race[tess$XTESS089 == 15| tess$XTESS089 == 19| tess$XTESS089 == 23] <- 7
tess$target.race[tess$XTESS089 == 16| tess$XTESS089 == 20| tess$XTESS089 == 24] <- 8
tess$target.race <- as.factor(tess$target.race)
# 리코딩이 잘 되었는지 확인하였다.
table(tess$XTESS089, tess$target.race, useNA = 'always')
# 결과변수로는 Q8을 이용하였다. Q8에 대해서는 앞에서 설명한 바 있다. -1의 경우 결측값처리하였다.
table(tess$Q8, useNA = 'always')
tess$pain.other <- tess$Q8
tess$pain.other[tess$Q8 == -1] <- NA
table(tess$Q8, tess$pain.other, useNA = 'always')
# aov()는 분산분석을 실시하는 R명령문이다. 결과변수 ~ 예측변수의 형태를 띈다.
# data에는 분석대상 데이터를 투입하면 된다. 해당결과를 obj.aov라는 이름의 오브젝트로 저장했다.
obj.aov <- aov(pain.other ~ target.race, data = tess)
# obj.aov의 결과를 제시한다. summary()를 쓰지 않은 상태와 비교해 보면 summary() 역할을 알 수 있다.
summary(obj.aov)

# 다중비교(투키의 Honest Significant Difference 기법)를 실시하였다.
# which 옵션에서는 원하는 요인의 이름을 적으면 된다.
TukeyHSD(obj.aov, which = 'target.race', ordered = FALSE)

# 현재 총 8개의 집단이 있기 때문에 그림이 보다 효과적이다. cex.axis는 축의 문자를 기본옵션의
# 반(0.5)으로 한다는 의미이다.
plot(TukeyHSD(obj.aov, which = 'target.race'), cex.axis = 0.5)

# 이원분산분석
# 앞에서 반복된 프로그래밍은 소개하지 않았다.
# 앞에서는 요인을 8개로 했다. 그러나 여기서는 이미지의 성별(남성;여성), 그리고
# 인종(백인, 흑인, 히스패닉, 아시아인)으로 2개로 설정하였다.
table(tess$XTESS089, useNA = 'always')
tess$target.race <- tess$XTESS089
tess$target.race[tess$XTESS089 == 1| tess$XTESS089 == 5| tess$XTESS089 == 9] <- 1
tess$target.race[tess$XTESS089 == 2| tess$XTESS089 == 6| tess$XTESS089 == 10] <- 2
tess$target.race[tess$XTESS089 == 3| tess$XTESS089 == 7| tess$XTESS089 == 11] <- 3
tess$target.race[tess$XTESS089 == 4| tess$XTESS089 == 8| tess$XTESS089 == 12] <- 4
tess$target.race[tess$XTESS089 == 13| tess$XTESS089 == 17| tess$XTESS089 == 21] <- 1
tess$target.race[tess$XTESS089 == 14| tess$XTESS089 == 18| tess$XTESS089 == 22] <- 2
tess$target.race[tess$XTESS089 == 15| tess$XTESS089 == 19| tess$XTESS089 == 23] <- 3
tess$target.race[tess$XTESS089 == 16| tess$XTESS089 == 20| tess$XTESS089 == 24] <- 4
tess$target.race <- as.factor(tess$target.race)
tess$taget.female <- tess$XTESS089
tess$target.female <- 0
tess$target.female[tess$XTESS089 >= 13] <- 1
tess$target.female <- as.factor(tess$target.female)
# 리코딩이 잘 되었는지 확인하였다.
table(tess$XTESS089, tess$target.race, useNA = 'always')
table(tess$XTESS089, tess$target.female, useNA = 'always')
# 결과변수로는 Q8을 이용하였다. Q8에 대해서는 앞에서 설명한 바 있다. -1의 경우 결측값처리하였다.
table(tess$Q8, useNA = 'always')
tess$pain.other <- tess$Q8
tess$pain.other[tess$Q8 == -1] <- NA
table(tess$Q8, tess$pain.other, useNA = 'always')
# aov()는 똑같지만, 공식의 표현부분이 다르다. 우선은 주효과항(main effect terms)만을 가정했다.
# A + B는 주효과항만을 투입한다는 의미이다.
obj.aov.m1 <- aov(pain.other ~ target.race + target.female, data = tess)
# 이제는 주효과항과 더불어 상호작용효과항(interaction effect term)을 추가하였다.
# A * B라는 표현은 A와 B의 주효과와 상호작용 효과항 모두를 포함한다.
obj.aov.m2 <- aov(pain.other ~ target.race * target.female, data = tess)
# 위에서 살펴본 두 오브젝트의 결과를 보여준다.
summary(obj.aov.m1)
summary(obj.aov.m2)

# 다중비교(투키의 Honest Significant Difference 기법)를 실시하였다.
# which 옵션에서는 원하는 요인의 이름을 적으면 되는데, 현재는 이원분산분석이기 때문에
# target.race와 target.female의 두 요인을 다 적을 수 있다. 그러나 target.female의 경우
# 2개 수준(level)이기 때문에 TukeyHSD의 의미가 없다.
TukeyHSD(obj.aov.m1, which = 'target.race', ordered = FALSE)
TukeyHSD(obj.aov.m2, which = 'target.race', ordered = FALSE)
TukeyHSD(obj.aov.m1, which = 'target.female', ordered = FALSE)
TukeyHSD(obj.aov.m2, which = 'target.female', ordered = FALSE)

# 2원분산분석의 상호작용을 표시하는 그래프는 아래와 같이 그릴 수 있다.
# x.factor의 경우는 X축에 놓이게 될 요인, trace.factor는 X축에 놓이 요인이 아닌 다른 요인,
# response의 경우는 결과변수, fun의 경우는 이용할 함수이다. 평균값을 설정하였는데, 결측값이
# 있기 때문에 function(x) {mean(x, na.rm = TRUE) }를 이용하여 결측값을 제거하는 옵션을 넣도록하였다.
# type은 그림을 보는 것이 이해가 빠를 것이다. 그리고 ylab은 Y축의 이름을, xlab은 X축의 이름을
# 그리고 trace.label은 구분되는 집단의 이름을 넣은 것이다. (myrecode.race / myrecode.gender의 경우
# 앞의 리코딩 부분에서 설명한 바 있다. 
myrecode.race <- c('White','Black','Hispanic','Asian')
myrecode.gender <- c('Male','Female')
interaction.plot(x.factor = myrecode.race[tess$target.race], 
trace.factor = myrecode.gender[tess$target.female], 
response = tess$pain.other, fun = function(x) {mean(x,na.rm = TRUE) }, type = 'b',
ylab = "Mean of subjective others' pain", xlab = "target's race",
trace.label = "target's gender")

# 실험자극에서는 상호작용효과가 발견되지 않았지만, 아래와 같이 응답자의 정당선호도와 인종과 같은
# 응답자의 개인적 특성을 요인으로 간주할 경우, 상호작용 효과를 발견할 수 있다.
# 응답자의 정당선호도를 공화당지지파, 무당파, 민주당지지파로 구분한다.
table(tess$XPARTY7, useNA = 'always')
tess$party3 <- tess$XPARTY7
tess$party3[tess$XPARTY7 <= 2] <- 'Republican'
tess$party3[tess$XPARTY7 <= 5 & tess$XPARTY7 >= 3] <- 'Independent'
tess$party3[tess$XPARTY7 >= 6] <- 'Democrat'
tess$party3 <- factor(tess$party3,levels=c('Republican','Independent','Democrat'))
table(tess$XPARTY7, tess$party3, useNA = 'always')
# 응답자의 인종을 백인, 흑인, 히스패닉, 아시아계의 4수준으로 구분한다.
tess$own.race <- NA
tess$own.race[tess$PPETHM == 1] <- 'White'
tess$own.race[tess$PPETHM == 2] <- 'Black'
tess$own.race[tess$PPETHM == 4] <- 'Hispanic'
tess$own.race[tess$PPETHM == 3] <- 'Asian'
tess$own.race <- factor(tess$own.race,c('White','Black','Hispanic','Asian'))
# 우선은 주효과항(main effect terms)만을 가정했다.
obj.aov.m3 <- aov(pain.other ~ own.race + party3, data = tess)
# 이제는 주효과항과 더불어 상호작용효과항(interaction effect term)을 추가하였다.
obj.aov.m4 <- aov(pain.other ~ own.race * party3, data = tess)
# 위에서 살펴본 두 오브젝트의 결과를 보여준다.
summary(obj.aov.m3)
summary(obj.aov.m4)

# 상호작용 효과의 그래프를 보면 상호작용 효과가 확실하게 나타난다.
interaction.plot(x.factor = tess$own.race, trace.factor = tess$party3,
response = tess$pain.other, fun = function(x) {mean(x,na.rm = TRUE) }, type = 'b',
ylab = "Mean of subjective others' pain", xlab = "respondent's race",
trace.label = "Party affiliation")

# anova()는 모형과 모형의 적합도를 비교한다. obj.aov.m3와 obj.aov.m4를 비교하면, 상호작용 효과를
# 추가투입하는 것이 모형의 설명도를 유의미하게 높이는지를 테스트할 수 있다.
anova(obj.aov.m3, obj.aov.m4)

table(tess$Q4, useNA = 'always')
tess$pain.own <- tess$Q4
tess$pain.own[tess$Q4 == -1 ] <- NA
# pain.own과 pain.other 변수간 실측값과 결측값 관계를 다음과 같이 살펴보았다. 
table(tess$pain.own, tess$pain.other, useNA='always')
# 두 변수중 하나만 결측값이 있어도 분석에서 제외하였다. 
tess.subset <- tess[complete.cases(tess[,c('pain.other','pain.own')]),]
# aov()는 똑같지만, 공식의 표현부분이 다르다. 우선은 공변량을 투입하지 않은 이원분산분석 결과를 실행하였다.
obj.aov.m5 <- aov(pain.other ~ target.race * target.female, data = tess.subset)
# 이제는 공변량을 포함한 이원공분산분석을 실시하였다.
obj.aov.m6 <- aov(pain.other ~ target.race * target.female + pain.own, data = tess.subset)
# 위에서 살펴본 두 오브젝트의 결과를 보여준다.
summary(obj.aov.m5)
summary(obj.aov.m6)
# anova()는 모형과 모형의 적합도를 비교한다. 이원분산분석에서처럼 obj.aov.m5와 obj.aov.m6를 비교해보자.
anova(obj.aov.m5, obj.aov.m6)

# 반복측정 분산분석
# 결과변수로는 Q5~Q8들을 이용하였다. 각 4개의 문항은 일상생활에서 아픔을 느끼게 되는 상황이다.
# 4개의 변수 모두 -1의 경우 결측값처리하였다.
recoding.Q5Q8 <- c(NA,0:10)
tess$q5r <- recoding.Q5Q8[tess$Q5+2]
tess$q6r <- recoding.Q5Q8[tess$Q6+2]
tess$q7r <- recoding.Q5Q8[tess$Q7+2]
tess$q8r <- recoding.Q5Q8[tess$Q8+2]
# 반복측정 분산분석을 쉽게 접근하기 위해서 car 라이브러리를 실행한다. car 라이브러리는 앞에서 설명한 바 있다
library('car')
# 개체내 요인에 해당되는 네개의 변수를 묶어 하나의 오브젝트화한다.
wsfactor <- as.matrix(tess[,c('q5r','q6r','q7r','q8r')])
# 개체내 요인의 수준별로 이름(label)을 붙인다. 고통의 상황을 상징하는 단어를 넣었다.
wsfname <- expand.grid(pains = c('disinfect','tongue','hand','toe'))
wsfname
# 아래는 네개의 고통상황들을 예측변수없이 선형분석(분산분석과 동일)을 한 것이다
# lm()은 회귀분석 섹션에서 보다 자세히 살펴보기로 한다. "~ 1"의 의미는 절편만으로 예측한다는
# 의미이다. 즉 어떠한 예측변수도 투입하지 않는다.
ws.aov.1 <- lm(wsfactor ~ 1, data = tess)
# Anova() 명령문은 개체내 요인을 이용하여 분산분석을 실시할 때 사용된다.
# ws.aov.1은 앞에서 이미 설명되었다. idata는 개체내 요인의 수준별 이름을 투입한다.
# idesign은 개체내 요인을 어떻게 투입할 것인지를 밝혀준다. 절편(intercept)없이 pains를 넣는다는 의미이다
# type은 제곱합(sum of squares)의 타입을 규정한다. 일반적으로 type 3이 가장 보편적이다.
ws.Anova.1 <- Anova(ws.aov.1, idata = wsfname, idesign = ~1 + pains, type='III')
# 저장된 결과를 보여준다. 결과를 해석하는 것은 가설을 해석하는 부분에 보다 자세하게 언급했다.
summary(ws.Anova.1)

# 개체내 요인과 개체간 요인의 분산분석
# 앞에서 설명한 부분은 다시 반복하지 않는다.
# 선형분석을 실시하되, 개체간 요인으로 어떤 정당을 지지하는지를 투입한다(party3)
ws.aov.2 <- lm(wsfactor ~ party3, data = tess)
# Anova()의 ws.aov.1을 ws.aov.2로 교체한다. 나머지는 동일하다.
ws.Anova.2 <- Anova(ws.aov.2, idata=wsfname, idesign = ~1 + pains, type='III')
# 저장된 결과를 보여준다. 결과를 해석하는 것은 가설을 해석하는 부분에 보다 자세하게 언급했다.
summary(ws.Anova.2)

# 반복측정 분산분석
# 결과변수로는 Q5~Q8들을 이용하였다. 각 4개의 문항은 일상생활에서 아픔을 느끼게 되는 상황이다.
# 4개의 변수 모두 -1의 경우 결측값처리하였다.
recoding.Q5Q8 <- c(NA,0:10)
tess$q5r <- recoding.Q5Q8[tess$Q5+2]
tess$q6r <- recoding.Q5Q8[tess$Q6+2]
tess$q7r <- recoding.Q5Q8[tess$Q7+2]
tess$q8r <- recoding.Q5Q8[tess$Q8+2]
# 반복측정 분산분석을 쉽게 접근하기 위해서 car 라이브러리를 실행한다. car 라이브러리는 앞에서 설명한 바 있다
library('car')
# 개체내 요인에 해당되는 네개의 변수를 묶어 하나의 오브젝트화한다.
wsfactor <- as.matrix(tess[,c('q5r','q6r','q7r','q8r')])
# 개체내 요인의 수준별로 이름(label)을 붙인다. 고통의 상황을 상징하는 단어를 넣었다.
wsfname <- expand.grid(pains = c('disinfect','tongue','hand','toe'))
wsfname
# 아래는 네개의 고통상황들을 예측변수없이 선형분석(분산분석과 동일)을 한 것이다
# lm()은 회귀분석 섹션에서 보다 자세히 살펴보기로 한다. "~ 1"의 의미는 절편만으로 예측한다는
# 의미이다. 즉 어떠한 예측변수도 투입하지 않는다.
ws.aov.1 <- lm(wsfactor ~ 1, data = tess)
# Anova() 명령문은 개체내 요인을 이용하여 분산분석을 실시할 때 사용된다.
# ws.aov.1은 앞에서 이미 설명되었다. idata는 개체내 요인의 수준별 이름을 투입한다.
# idesign은 개체내 요인을 어떻게 투입할 것인지를 밝혀준다. 절편(intercept)없이 pains를 넣는다는 의미이다
# type은 제곱합(sum of squares)의 타입을 규정한다. 일반적으로 type 3이 가장 보편적이다.
ws.Anova.1 <- Anova(ws.aov.1, idata = wsfname, idesign = ~1 + pains, type='III')
# 저장된 결과를 보여준다. 결과를 해석하는 것은 가설을 해석하는 부분에 보다 자세하게 언급했다.
summary(ws.Anova.1)

####################################################----------각주
# 계획비교(Planned Comparison)
# 앞에서 반복된 부분의 경우 설명을 생략하였다.
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav",
use.value.labels = FALSE, to.data.frame = TRUE)
# 실험설계에 대한 설명은 데이터와 함께 첨부된 문서를 참조할 것.
# 결과변수로는 Q8을 이용하였다. Q8에 대해서는 앞에서 설명한 바 있다. -1의 경우 결측값 처리하였다.
table(tess$Q8, useNA = 'always')
tess$pain.other <- tess$Q8
tess$pain.other[tess$Q8 == -1] <- NA
# 실험자극은 총 24개이며, 성별에 상관없이 인종별로 4집단을 구분하였다.
table(tess$XTESS089, useNA = 'always')
tess$target.race <- tess$XTESS089
tess$target.race[tess$XTESS089 == 1| tess$XTESS089 == 5| tess$XTESS089 == 9] <- 1
tess$target.race[tess$XTESS089 == 2| tess$XTESS089 == 6| tess$XTESS089 == 10] <- 2
tess$target.race[tess$XTESS089 == 3| tess$XTESS089 == 7| tess$XTESS089 == 11] <- 3
tess$target.race[tess$XTESS089 == 4| tess$XTESS089 == 8| tess$XTESS089 == 12] <- 4
tess$target.race[tess$XTESS089 == 13| tess$XTESS089 == 17| tess$XTESS089 == 21] <- 1
tess$target.race[tess$XTESS089 == 14| tess$XTESS089 == 18| tess$XTESS089 == 22] <- 2
tess$target.race[tess$XTESS089 == 15| tess$XTESS089 == 19| tess$XTESS089 == 23] <- 3
tess$target.race[tess$XTESS089 == 16| tess$XTESS089 == 20| tess$XTESS089 == 24] <- 4
# Planned Comparison을 편하게 하기 위해 multcomp 라이브러리가 필요하다. multcomp 가 없으면
# install.packages()를 통해서 설치한 후 사용하면 된다.
library('multcomp')
# 연구가설은 응답자 전체가 아니라 백인만에 해당된다. 즉 응답자의 인종을 확인하여야 한다.
# 여기서는 백인, 흑인, 히스패닉, 아시아계의 4수준으로 구분한다.
tess$own.race <- NA
tess$own.race[tess$PPETHM == 1] <- 1
tess$own.race[tess$PPETHM == 2] <- 2
tess$own.race[tess$PPETHM == 4] <- 3
tess$own.race[tess$PPETHM == 3] <- 4
# 사진 속의 인종과 자신의 인종을 교차시키면 총 16개의 상황이 도출된다.
table(tess$own.race, tess$target.race, useNA = 'always')
# 16개의 조건을 16개의 수준을 갖는 하나의 요인으로 바꾼다.
# 즉, 10의 자리의 숫자가 뜻하는 것은 응답자, 1의 자리의 숫자가 뜻하는 것은 타인의 인종이 된다.
tess$own.target.race <- as.factor(10*tess$own.race + tess$target.race)
table(tess$own.target.race, useNA = 'always')
# 연구가설에서 비교하고자 하는 집단은 백인이면서 백인을 평가하는 경우와 백인이면서
# 흑인을 평가하는 결과이다. 즉 '11'과 '12'가 비교되며 나머지 집단들은 관심의 대상이 아니다.
# 비교가 되는 집단은 1과 -1을, 그리고 나머지 집단들에는 0을 배분하는 벡터를 작성한다.
plan.compare <- c('WW v. WB' = c(1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
obj.aov <- aov(pain.other ~ own.target.race, data=tess)
summary(obj.aov)
obj.aov.compare <- glht(obj.aov,linfct=mcp(own.target.race = plan.compare),
alternative = 'two.sided')
summary(obj.aov.compare)


effectsize_function <- function(aov_object,mydigit) {
	  mycomponent <- unlist(summary(aov_object))
	  myaovlength <- length(mycomponent)/5
	  myaovtable <- data.frame(cbind(mycomponent[(0*myaovlength+1):myaovlength],
	  mycomponent[(1*myaovlength+1):(2*myaovlength)],
	  mycomponent[(2*myaovlength+1):(3*myaovlength)],
	  mycomponent[(3*myaovlength+1):(4*myaovlength)],
	  mycomponent[(4*myaovlength+1):(5*myaovlength)]))
	  rownames(myaovtable) <- c(paste('source',1:(myaovlength-1)),'residual')
	  colnames(myaovtable) <- c('df','SS','MS','F','p')
	  mySSeffect <- myaovtable[1:(myaovlength-1),2]
	  mypartialeta <- mySSeffect/(mySSeffect+myaovtable[myaovlength,2])
	  mydfeffect <- myaovtable[1:(myaovlength-1),1]
	  myMSerror <- myaovtable[myaovlength,3]
	  mySStotal <- myaovtable[myaovlength,2]
	  mypartialomega <- (mySSeffect - mydfeffect*myMSerror)/(mySStotal+myMSerror)
	  mypartialeta <- c(mypartialeta,NA)
	  mypartialomega <- c(mypartialomega,NA)
	  myeffectsize <- data.frame(mypartialeta,mypartialomega)
	  rownames(myeffectsize) <- c(paste('source',1:(myaovlength-1)),'Residual')
	  round(myeffectsize,mydigit)
  }
  
