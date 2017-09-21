# 2부. 데이터 관리
# 2장. 데이터 입력하기 및 불러오기
# 1. 데이터의 직접입력

## 직접입력법 1: 변수별 구성
# year 라는 이름의 변수를 구성함
# <- 라는 표시는 변수에 어떠한 값을 부여하는가 설정하는 역할을 한다. 흔히 assignment(배치) 라고 불린다.
# c(): 콤마로 구분된 일련의 괄호 속의 입력값(value)을 표시한다.
year <- c(1987, 1992, 1997, 2002, 2007, 2012)
# president(대통령이름)라는 이름을 갖는 변수를 생성한다.
# <- 과 c()는 위와 동일한 역할을 수행한다.
president <- c('TW Roh','YS Kim','DJ Kim','MH Roh','MB Lee','GH Park')
# rate(투표율)라는 이름의 변수를 설정하였다.
rate <- c(89.2, 81.9, 80.7, 70.8, 63.0, 75.8)
# cbind()라는 명령어를 통해 세 개의 변수(year, president, & rate)를 묶는다.
# cbind는 column bind(컬럼을 묶는다)의 약자이다.
# data.frame()이라는 명령문은 괄호 속의 오브젝트를 데이터셋 형태로 변환함을 의미한다.
# 오른쪽과 같이 변환된 데이터셋은 national.election이라는 데이터셋 이름으로 저장된다.
national.election <- data.frame(cbind(year, president, rate))
# national.election이라는 이름의 오브젝트를 PC화면에 보여준다.
national.election

## 직접입력법 2: 측정단위별 구성
# <- 과 c()은 변수별 구성과 동일한 역할을 수행한다.
# 총 6번의 측정단위(즉 선거)를 r1, r2, …, r6라는 이름의 오브젝트로 각각 설정한다.
r1 <- c(1987, 'TW Roh', 89.2)
r2 <- c(1992, 'YS Kim', 81.9)
r3 <- c(1997, 'DJ Kim', 80.7)
r4 <- c(2002, 'MH Roh', 70.8)
r5 <- c(2007, 'MB Lee', 63.0)
r6 <- c(2012, 'GH Park', 75.8)
# rbind()는 괄호안의 오브젝트를 묶는 역할을 한다. cbind가 세로줄(column)을 중심으로 묶는다면,
# rbind는 가로줄(row)을 중심으로 묶는다는 점에서 차이가 있다.
# data.frame(rbind())를 통해 저장된 데이터셋은 national.election이라는 오브젝트로 저장된다.
national.election <- data.frame(rbind(r1,r2,r3,r4,r5,r6))
# colnames()은 괄호안의 데이터셋 형식의 오브젝트의 세로줄 이름을 나타내는 명령문이다.
# national.election이라는 데이터셋의 세 변수의 이름을 순서대로 year, president, rate라고 붙여준다.
colnames(national.election) <- c('year','president','rate')
national.election

