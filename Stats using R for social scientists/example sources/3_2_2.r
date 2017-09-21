# 3부. 데이터 사전처리
# 2장. 변수 관리
# 2. 역코딩하기(reverse-coding)

# 앞서의 pew$q16m을 만드는 작업은 생략한다.  
# reversecoding이라는 오브젝트를 만드는데, 그 목록을 4, 3, 2, 1로 한다. 
reversecoding <- c(4,3,2,1)
# reversecoding의 목록에 해당되는 경우 1,2,3,4를 4,3,2,1로 바꾸어 q16mr이라는 변수로 만들어 
# pew데이터에 저장한다. 
pew$q16mr <- reversecoding[pew$q16m]
# 역코딩이 제대로 진행되었는지 확인한다. 
table(pew$q16mr, pew$q16m, useNA = 'always')

# 앞에서 반복된 부분의 경우 설명을 생략하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# Q4를 확인하면 무응답으로 -1이 있음을 알 수 있다.  
table(tess$Q4)
# Q4에서 역코딩을 할 Q4r을 하나 더 생성한다. 
tess$Q4r <- tess$Q4
# 무응답을 처리하였다.  
tess$Q4r[tess$Q4 == -1] <- NA 
# 역코딩을 실시하였다.  
tess$Q4r <- 10 - tess$Q4r
# 역코딩이 제대로 실시되었는지 체크하였다.  
table(tess$Q4, tess$Q4r, useNA = 'always')



