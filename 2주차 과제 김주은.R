# 1. Importing data ----
setwd('C:/Users/Jooeun Kim/Desktop/ESC/Week2 Assignment/harris')
data = readline("Enter the data file name: ")
harris
data = paste(data, '.dat', sep='')
cat('Enter the coding format: ')
fm = scan(n = 1, quiet = T)
1
if(fm == 1){coding_format = ' '} else {coding_format = ','}
data = read.table(data, sep = coding_format)


# 2. Constructing Multiple Linear Regression Algorithm ----
my_lm = function(data){
  
  # Which variable is response variable?
  ### Hint : y_index 지정시 scan 함수 사용, 이후 이 index를 이용하여 y 변수를 추출
  cat('Which variable is response variable? Enter the column index(1,2,3,...): ')
  y_index = scan(n=1, quiet=T)
  
  # Design matrix 'X'
  ### Hint : data 내에서 y변수를 '-' indexing해서 제거, 이후 '1'로만 구성된 vector를 생성하여 이를 cbind() 함수로 묶어주기 
  X = cbind(v1=matrix(1,nrow(data),1), data[-y_index])
  Y = as.matrix(data[y_index])
  X = as.matrix(X)
  
  # Beta Coefficient and fitted value
  ### Hint : X를 이용한 행렬 연산(행렬 곱 시 '%*%' 이용 및 역행렬, 전치행렬에 대한 함수 필요)
  beta = solve(t(X) %*% X) %*% t(X) %*% Y
  Y_hat = X %*% beta
  
  # Model summary statistic
  Y_bar = mean(Y)
  SSTO = sum((Y - Y_bar)^2)
  SSR = sum((Y_hat - Y_bar)^2)
  SSE = sum((Y - Y_hat)^2)
  R_squared = SSR / SSTO
  MSE = SSE / (nrow(data) - ncol(data))

  #------------------------------------------------------------------------------------------------------------#
  
  # Output file
  output_filename = readline('Enter the output file name: ') ### 결과물로 도출될 text file의 이름
  output_filename = paste(output_filename, '.txt', sep = '')
  
  ## Coefficient output
  cat('Coefficients', '\n', '-------------', '\n', sep = '', file = output_filename)
  ### Hint : Beta_output을 만들기 위해서는 paste()와 as.matrix() 함수 필요함. 
  coeff = c('Constant', paste('Beta',1:(nrow(beta)-1)))
  beta_output = as.matrix(paste(coeff, ':', rep(' ',3), round(beta, 3), sep=''))
  write.table(beta_output, file = output_filename, quote = FALSE, append = TRUE, col.names = FALSE, row.names = FALSE)
  
  ## ID, Actual values, Fitted values output
  cat('\n', 'ID, ','Actual values, ','Fitted values','\n','--------------------------------', '\n', sep = '', file = output_filename, append = TRUE)
  ### Hint :  세로로 결과물을 출력하고 싶을 땐 as.matrix() form으로 바꿔주어야 한다. cbind() 역시 활용할 것. 또한 write.table 작성 시 append = TRUE 역시 잊지말 것.
  output = cbind(1:nrow(Y), Y, round(Y_hat,1))
  write.table(output, sep=', ', file = output_filename, quote = FALSE, append = TRUE, col.names = FALSE, row.names = FALSE)
  
  ## Model Summary output
  ### Hint : Text 결과물의 '모양'에 따라서 cbind()를 쓸 수도, rbind()를 쓸 수도 있다. 잘 생각해볼 것.
  cat('\n', 'Model Summary', '\n', '-------------', '\n','R-square ', '= ', round(R_squared,4), '\n', 'MSE ', '= ', round(MSE,3), '\n', sep='',file = output_filename, append=T)
}

# 3. Output result ----
my_lm(data)