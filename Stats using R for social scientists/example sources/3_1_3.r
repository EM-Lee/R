# 3부. 데이터 사전처리
# 1장. 데이터 중 일부만 골라내기
# 3. 집단수준의 통계치로 집산된 데이터셋(aggregated dataset)

# 데이터의 집산(data aggregation)
# state의 경우 CSV 파일은 주(州)가 숫자로 입력되어 있다. 이 경우 SPSS버전의 파일을 불러오면 편한데, 
# 그 이유는 SPSS의 경우 숫자정보별로 레이블(label)이 붙어있어 어떤 숫자가 어떤 주인지 쉽게 알아볼 수 있기
# 때문이다. 레이블을 불러오기 위해서는 use.value.labels의 옵션을 TRUE로 바꾸어주면 된다. 
pew <- read.spss(file = "C:/data/example/Omnibus_Oct_2012_Political_Video_spss.sav", 
	use.value.labels = TRUE, to.data.frame = TRUE)
# aggregate()는 카테고리별로 특정한 변수별 통계치를 계산하여주는 함수이다. 아래의 명령문은 주(州)별로 
# 응답자들의 연령평균을 계산한 후 pew.state.age.mean이라는 오브젝트로 저장하라는 뜻이다. 
# FUN은 aggregation을 평균을 통해 실행하라는 의미이다. 
pew.state.age.mean <- aggregate(age ∼ state, data = pew, FUN = mean)
# 위와 의미는 같다. FUN = median에서 유추할 수 있듯 응답자들의 연령 중간값을 계산한 후 저장하는 점이 다르다.  
pew.state.age.median <- aggregate(age ∼ state, data = pew, FUN = median)
# 위와 의미는 같다. FUN = length는 각 주별로 응답자들이 몇 명인지를 계산하는 역할을 한다. 
# length()라는 함수는 주어진 벡터(vector)의 길이, 즉 사례수가 얼마인지 계산해주는 함수이다. 
pew.state.age.case <- aggregate(age ∼ state, data = pew, FUN = length)
# 위의 세 가지 오브젝트들의 세로줄 이름(즉 변수이름)을 확인하였다. 결과에서 알 수 있듯 두 번째 변수의 이름이 
# 모두 age로 나와 있지만, 우리는 해당 이름이 서로 질적으로 다르다는 것을 알고 있다. 
colnames(pew.state.age.mean)
colnames(pew.state.age.median)
colnames(pew.state.age.case)
# 따라서 두 번째 세로줄의 이름을 각각 다르게 명명하였다.  이를테면 colnames(pew.state.age.mean)[2]의 경우 
# [2]라는 인덱싱을 덧붙임으로써 오브젝트의 세로줄 이름 목록 중 두 번째 이름을 인덱싱할 수 있다. 
# 우변에서 확인할 수 있듯 해당 세로줄의 이름은 mean.age라는 이름으로 부여되었다. 
colnames(pew.state.age.mean)[2] <- "mean.age"
colnames(pew.state.age.median)[2] <- "median.age"
colnames(pew.state.age.case)[2] <- "number.cases"
# 앞서 학습했던 변수합치기를 통해 통합된 데이터(이름은 pew.state.age)를 만들어낼 수 있다. 
pew.state.age <- merge(pew.state.age.case, pew.state.age.mean, by = "state")
pew.state.age <- merge(pew.state.age, pew.state.age.median, by = "state")
# dim()을 통해 몇 개의 가로줄과 세로줄을 가진 데이터인지 확인하였다. 총 49개의 가로줄을 가지고 있는데,
# 그 이유는 알래스카(Alaska)와 하와이(Hawaii), 두 개 주의 경우 단 한명의 응답자도 표집되지 않았기 때문이다. 
dim(pew.state.age)

# merge()가 아닌 data.frame()를 이용하여 데이터를 합칠 수도 있다. 그러나 data.frame()의 경우 가로줄이 
# 정확하게 일치하지 않을 경우 데이터를 합칠 때 오류가 발생하기 때문에 개인적으로는 별로 권하지 않는다. 
# 이 경우 pew.state.age.mean, pew.state.age.median, pew.state.age.case 의 세 오브젝트가 정확하게 
# 일치하는 가로줄 구조를 가지고 있기 때문에 아래와 같은 데이터 합치기도 가능하다.  pew.state.age.median와 
# pew.state.age.case의 경우 두 번째 세로줄만 인덱싱을 한 이유는 첫 번째 세로줄(주(州)의 이름)의 경우 
# pew.state.age.mean에 이미 포함이 되어 있어 중복되기 때문이다. 
pew.state.age <- data.frame(pew.state.age.mean,pew.state.age.median[,2],pew.state.age.case[,2])




