# 3부. 데이터 사전처리
# 1장. 데이터 중 일부만 골라내기
# 2. 데이터 중 일부 사례만 취하기(selecting cases)

# 데이터중 어떤 특징(feature)을 갖고 있는 사례들만을 취하기 
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# 공화당원으로만 구성된 표본 만들기 
# 인덱싱을 통해 원하는 사례들을 골라내는 방법은 아래와 같다. 특성을 나타내는 변수는 pew라는 
# 오브젝트에 속해있다. 즉 pew$party라는 오브젝트 중 그 값이 1과 같은(즉 공화당을 지지하는) 조건에 
# 해당되는 가로줄만을 익덱싱하여 해당 사례들로 구성된 
# 새로운 오브젝트 pew.republican.indexing 을 구성하였다.  
pew.republican.indexing <- pew[(pew$party == 1),]
# subset() 함수는 변수의 조건에 맞는 사례를 골라내는 기능을 한다. 
# 여기서는 pew 오브젝트 내의 party라는 변수가 1의 값을 갖는 사례들만으로 구성된 오브젝트를 저장하는 것이다.  
pew.republican.subset <- subset(pew, party == 1) 
# 여성이며 민주당원인 응답자로만 구성된 표본 만들기 
# 위와 달라진 것은 없지만, 추가조건을 덧붙였다. 흔히 &는 불리안(Boolean) 함수라고 불린다.  
pew.female.democrat.indexing <- pew[(pew$party == 2 & pew$sex == 2),]
# 앞의 인덱싱에서 설명한 것과  마찬가지이다.  
pew.female.democrat.subset <- subset(pew, party == 2 & sex == 2) 

# 어떠한 특성을 갖는 사례로 구성된 데이터에서 원하는 변수들만 선별하기
pew <- read.csv(file = "C:/data/example/February_2012_csv.csv", header = TRUE)
# colnames()를 통해서 변수이름들을 확인. 성별('sex'), 인종('race'), 연령('age'), 소득('inc'), 
# 교육수준('educ')과 지지정당('party')의 여섯 개 변수 목록 설정
var.keep <- c('sex','race','age','inc','educ','party')
# 아래의 4가지 방법은 과정이 조금 다르기는 하지만 동일함
# (1) 변수를 선별하는 과정, 그리고 개인의 특징을 선택하는 과정을 인덱싱을 통해 별개로 진행하는 방법
pew.select.1 <- pew[,var.keep] 
pew.select.1 <- pew.select.1[(pew.select.1$party==1 & pew.select.1$sex==1),]
# (2) 변수를 선별하는 과정, 그리고 개인의 특징을 선택하는 과정을 인덱싱과 
# subset()을 이용하여 별개로 진행하는 방법
pew.select.2 <- pew[,var.keep] 
pew.select.2 <- subset(pew.select.2, party==1 & sex==1)
# (3) 변수를 선별하는 과정, 그리고 개인의 특징을 선택하는 과정을 인덱싱을 통해 동시에 진행하는 방법
pew.select.3 <- pew[(pew$party==1 & pew$sex==1),var.keep] 
# (4) 변수를 선별하는 과정, 그리고 개인의 특징을 선택하는 과정을 subset()을 이용하여 동시에 진행하는 방법
pew.select.4 <- subset(pew, party==1 & sex==1)[,var.keep] 




