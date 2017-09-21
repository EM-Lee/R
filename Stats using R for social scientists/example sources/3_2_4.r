# 3부. 데이터 사전처리
# 2장. 변수 관리
# 4. (실험 데이터에서의) 요인(factor)으로 전환

# 앞에서 반복된 부분의 경우 설명을 생략하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# 해당 데이터에서 무작위로 배치된 실험자극은 XTESS089라는 변수이다.  
table(tess$XTESS089)
# is.numeric()는 해당 오브젝트가 수치형(numeric)인지를 테스트한다. TRUE의 경우 수치형임을, 
# FALSE일 경우는 수치가 아님을 나타낸다.  마찬가지로 is.factor() 역시 요인이면 TRUE, 
# 요인이 아니면 FALSE 로 보고한다.  
is.numeric(tess$XTESS089) 
is.factor(tess$XTESS089) 
# as.factor()는 해당 변수(벡터)를 요인(factor)로 바꾸어주는 함수이다.  
tess$stimulus <- as.factor(tess$XTESS089) 
# is.numeric()과 is.factor()를 통해서 stimulus가 성공적으로 변화하였음을 확인할 수 있다.  
is.numeric(tess$stimulus) 
is.factor(tess$stimulus) 
# 요인으로 된 변수의 경우 as.numeric을 사용하여 수치형인 변수로 다시 변화가능하다. 
tess$stimulus.num <- as.numeric(tess$stimulus)
# is.numeric()과 is.factor()를 통해서 stimulus.num가 수치형 변수로 변화했음을 확인할 수 있다.  
is.numeric(tess$stimulus.num) 
is.factor(tess$stimulus.num) 



