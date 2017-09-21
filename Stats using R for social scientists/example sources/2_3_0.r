# 2부. 데이터 관리
# 3장. 데이터의 통합과 분석

# library()를 이용, 엑셀파일을 불러오기 위해서 xlsx 라이브러리를 활성화시킴
# 만약 xlsx 라이브러리가 없으면 install.packages("xlsx")를 이용하여 인스톨할 것.
library('xlsx')
# 우선 국가별 기대수명 엑셀파일을 불러옴 (sheetIndex = 1은 전제인구대상)
life.world.xlsx <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls",
		sheetIndex = 1, header = TRUE)
# 다음으로 국가별 일인당 GDP 엑셀파일을 불러옴
gdp.world.xlsx <- read.xlsx(file = "C:/data/example/WorldBank_GDPperCapita_2010.xls",
		sheetIndex = 1, header = TRUE)
# 국가코드(Country.Code)를 중심으로 일인당 GDP와 기대수명을 서로 일치시킴
life.gdp <- merge(gdp.world.xlsx, life.world.xlsx, by = 'Country.Code')
# head() 명령문은 괄호속 오브젝트의 첫 6줄에 해당하는 데이터를 제시해준다.
# 세 번의 head() 명령문을 통해서 데이터 합치기가 제대로 수행되었는지 확인하였다.
head(gdp.world.xlsx)
head(gdp.world.xlsx)
head(life.gdp)

# 우선 국가별 기대수명 엑셀파일을 불러옴 (sheetIndex = 2는 남성의 기대수명)
# 우선 국가별 기대수명 엑셀파일을 불러옴 (sheetIndex = 3는 여성의 기대수명)
life.world.fe <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls",
	sheetIndex = 2)
life.world.ma <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls",
	sheetIndex = 3)
# 각 데이터별로 해당되는 기대수명이 여성인지 아니면 남성인지를 구분함
# 데이터 프레임에 붙이는 $라는 표시는 데이터라는 상위 오브젝트에 딸려있는 gender라는 변수,
# 즉 데이터에 부속된 하위 오브젝트임을 나타낸다.
# 다시 말해 life.world.fe$gender <- 'female’ 는
# life.world.fe에 있는 gender라는 변수에 ‘female’이라는 입력값을 배치하였음을 의미한다.
life.world.fe$gender <- 'female'
life.world.ma$gender <- 'male'
# life.world.fe과 life.world.ma를 가로줄을 기준으로 쌓는 방식으로 결함시킴.
life.fe.ma <- data.frame(rbind(life.world.fe, life.world.ma))
# dim() 명령문을 통해서 가로줄(측정단위)과 세로줄(변수)의 수를 확인하였다.
# dim()은 주어진 오브젝트의 차원(dimension)별 사례수를 보여준다.
# 보통의 데이터 프레임은 R×C의 2차원 행렬(matrix)이기 때문에 가로줄의 수와 세로줄의 수가 표시된다.
# 결과를 보면 life.world.fe과 life.world.ma의 경우 246개의 가로줄과 3개의 세로줄이,
# 그리고 life.fe.ma의 경우 492개의 가로줄과 3개의 세로줄이 나오는 것을 확인할 수 있다.
# 즉 국가별로 여성의 기대수명과 남성의 기대수명이 각각 표시된 데이터임을 확인할 수 있다.
dim(life.world.fe)
dim(life.world.ma)
dim(life.fe.ma)


