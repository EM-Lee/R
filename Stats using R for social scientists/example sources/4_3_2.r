# 4부. 통계분석
# 3장. 다변량 추리통계분석
# 2. 카이제곱 검증

# 반복되는 부분에 대해서는 설명을 생략한다.  
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# 우선 각각의 변수들을 체크한다. 8과 9의 응답은 각각 '알지 못함', '응답거부'이기에 결측값으로 처리하였다.  
table(pew$intuse, useNA = 'always')
table(pew$q1, useNA = 'always')
pew$intuse.r <- pew$intuse
pew$life.quality <- pew$q1
pew$intuse.r[pew$intuse ==8 | pew$intuse == 9] <- NA
pew$life.quality[pew$q1 ==8 | pew$q1 == 9] <- NA
table(pew$intuse.r, pew$life.quality, useNA = 'always')
# 인터넷 이용여부에 따른 삶의 질의 응답붙포를 각 집단별로 살펴보았다 (이용자와 비이용자가 다르기 때문) 
round(table(pew$intuse.r, pew$life.quality)[1,]/sum(table(pew$intuse.r, pew$life.quality)[1,]),2) 
round(table(pew$intuse.r, pew$life.quality)[2,]/sum(table(pew$intuse.r, pew$life.quality)[2,]),2) 
# 카이제곱 검증은 table()로 얻은 오브젝트를 통해 계산가능하다. 
chisq.test(table(pew$intuse.r, pew$life.quality)) 

# 위와 같이 복잡하게 프로그래밍을 하는 것보다 외부 라이브러리를 사용하면 보다 깔끔한 결과를 쉽게 얻을 수 있다. 
# descr 라이브러리가 설치되어 있지 않으면 우선 install.packages('descr')을 이용해 설치한다. 
library('descr') 
# CrossTable()은 SPSS의 crosstabs나 SAS의 proc freq 와 유사하다. prop.r은 가로줄에 따른 비율, 
# prop.c는 세로줄에 따른 비율, prop.t는 전체칸들을 기준으로 한 비율을 의미한다. expected의 경우 
# 기대빈도를 표시할지를 결정하는 옵션이며, chisq의 경우 카이제곱 검증값을 보고할지 결정하는 옵션이다. 
CrossTable(pew$intuse.r, pew$life.quality, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, expected = TRUE, chisq = TRUE)

################################---- 각주 
CrossTable(pew$intuse.r, pew$sex, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, 
expected = TRUE, chisq = TRUE)

