# 1. Додайте датасет Orange в робоче середовище. Проведіть аналіз структури датасету. 
#    Створіть нову колонку під назвою year, в якій будуть нулі, якщо значення змінної age менше 366,
#    одиниці –age в межах від 366 до 731, 2 –якщо age більше 731 і менше 1096, 3 –якщо age більше 1097 
#    і менше 1462, 4 –якщо age більше 1463. 
#    Створіть  вектор circumference_4 і  збережіть  в  ньому  
#    значення  діаметру  дерев (circumference) для дерев віком чотири роки (year). 

Orange
Orange <- within(Orange,
                 {
                   year <- NA
                   year[age < 366] <- 0
                   year[age >= 366 & age <= 731] <- 1
                   year[age > 731 & age <= 1096] <- 2
                   year[age > 1097 & age < 1462] <- 3
                   year[age > 1463] <- 4
                 })
Orange

circumference_4 <- Orange$circumference[Orange$year == 4]

circumference_4

# 2.Створіть нову змінну
#   new_var в даних mtcars, яка містить одиниці в рядках, якщо в машині  не  менше  чотирьох  карбюраторів  
#   (змінна «carb»)  або  більше  шести циліндрів (змінна «cyl»). У рядках, в яких умова не виконується, 
#   повинні стояти нулі. 

mtcars

new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)
new_var


# 3.Для змінної new_var з попереднього завдання, використовуючи конструкцію: 
#   if () {} Else {} зробіть наступне: якщо середнє значення вектора new_var більше 0.7, в змінну result 
#   збережіть «My  mean  is  great», якщо середнє значення new_var менше або дорівнює0.7, то в змінну result 
#   збережіть рядок «My mean is not so great». 

if (mean(new_var) > 0.7) {
  result <- "My mean is great"
} else {
  result <- "My mean is not so great"
}
result

  
  
#  4.Додайте до датасету AirPassengers змінну good_months і збережіть 
#    в неї число пасажирів тільки в тих місяцях, в яких це число більше, ніж показник попереднього місяця. 

AirPassengers

good_months <- c()
index <- 1
for (i in 2: length(AirPassengers)) {
  if (AirPassengers[i] > AirPassengers[i-1]){
    good_months[index] <- AirPassengers[i]
    index <- index + 1
  }
}


good_months
AirPassengers




#  5.Створіть data.frame у робочому середовищі: 

df <- data.frame(var1=c(11,21,31), var2=c(12,22,32), 
                 var3=c(13,23,33), var4=c(14,24,34), 
                 row.names=c("case1", "case2", "case3")) 
df
# 1)Виведіть значення var1, var2, var3 для case1.

print(df[c(1), c(1:3)])

# 2)Виведіть значення всіх змінних для case2, які більші за 22. 

print(df[c(2), c(1:3)])


# 3)Виведіть імена змінних для колонок 1 і 3. 

print(df[c(0), c(1,3)])

# 4)Додайте колонку з іменем Y і значеннями -1, 0, 1. 

df['Y'] = c(-1, 0, 1)
df

# 5)Видаліть рядок case2. 

dfd <- df[-2,]
dfd

# 6)Значення другої колонки піднесіть в третій степінь.

df[,2] = df[,2]^3
df

# 6.Напишіть код, який запитує у користувача кількість елементів вектору («Enter number of elements:»),
#   зберігає його в змінну n і створює вектор заданої довжини, що складається з пропущених значень(NA). 
#   Далі, якщо індекс елемента парний, то цей елемент замінюється на 1, якщо непарний –на 0.

n <- readline(prompt = "Enter number of elements: ")
vec <- rep(NA, n)
vec

for (i in 1:n) {
  vec[i] <- ifelse(i %% 2 == 0, 1, 0)
}

vec

# 7.Напишіть функцію smart_squares(), яка приймає на вхід вектор, і якщо він числовий, 
#   то повертає вектор з квадратів його елементів, а якщо ні–повертає вектор з пропущених 
#   значень (NA) і виводить на екран повідомлення «Your  vector  is  not numeric».

smart_squares <- function(inp_vec) {
  if (is.numeric(inp_vec)) {
    squ_vec <- inp_vec^2
    return(squ_vec)
  } 
  else {
    cat("Your vector is not numeric\n")
    return(rep(NA, length(inp_vec)))
  }
}

smart_squares(c(1,2,3,4,5,6))
smart_squares(c("boba", "buba", "pupa"))