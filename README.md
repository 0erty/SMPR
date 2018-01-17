# Байесовские алгоритмы классификации
Байесовский классификатор — широкий класс алгоритмов классификации, основанный на принципе максимума апостериорной вероятности. Для классифицируемого объекта вычисляются функции правдоподобия каждого из классов, по ним вычисляются апостериорные вероятности классов. Объект относится к тому классу, для которого апостериорная вероятность максимальна. 

Вероятность p\{x|y\} = p_y(x) называют апостериорной вероятностью. Значение P\{y|x\} = P_y p_y(x) интерпретируется как апостериорная вероятность того, что объект x принадлежит классу y.

Задача классификации - получить алгоритм ![alt text](https://camo.githubusercontent.com/510e72a4d8abc6ab04c841af2afb1ea5fdb188ca/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f253543696e6c696e652532306125334125354325334225323058253543746f25323059) , способный классифицировать произвольный объект ![alt text](https://camo.githubusercontent.com/fa468569b697b464a6963aaa0d3be61157d816f6/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f253543696e6c696e6525323078253230253543696e25323058) .




## Вывод:
![alt text](https://i.screenshot.net/s/55r0zh5)

# KNN
KNN - алгоритм в котором задана обучающая выборка и множество классов. Требуется найти к какому классу относится выбранный объект. В случае использования тода для классификации объект присваивается тому классу, который является наиболее распространённым среди k соседей данного элемента, классы которых уже известны.

## Определение класса

При таком способе во внимание принимается не только количество попавших в область определенных классов, но и их удаленность от нового значения.

Для каждого класса j определяется оценка близости:

![alt text](https://wikimedia.org/api/rest_v1/media/math/render/svg/3ade505a0c21f3115a7af19d1bb8244f4a1b63d8) где d(x, a) — дистанция от нового значения x до объекта а.

У какого класса выше значение близости, тот класс и присваивается новому объекту.

Недостатки:

 + Приходится хранить обучающие выборку целиком.

 + Трудоемкость поиска ближайших соседей.

## Вывод:
![alt text](https://i.screenshot.net/s/lq2yrtz)


# LOO

Является частным случаем полного скользящего контроля при k=1, соотвественно, N=L. Это, пожалуй, самый распространённый вариант скользящего контроля.

Преимущества LOO в том, что каждый объект ровно один раз участвует в контроле, а длина обучающих подвыборок лишь на единицу меньше длины полной выборки.

Недостатком LOO является большая ресурсоёмкость, так как обучаться приходится L раз. 

Указываем интервал между числами и выборку чисел
```R
arr <- c(seq(1,150))
  while (k <= 150){ 
    while (i <= 150){ 
 ```

## Вывод:
![alt text](https://i.screenshot.net/s/2xwy2b0)

# Метод парзеновского окна

Метод парзеновского окна — метод байесовской классификации, основанный на непараметрическом восстановлении плотности по имеющейся выборке.

В основе подхода лежит идея о том, что плотность выше в тех точках, рядом с которыми находится большое количество объектов выборки.

Парзеновская оценка плотности имеет вид:
![alt text](http://www.machinelearning.ru/mimetex/?p_{y,h}(x)%20=%20\frac{1}{l_y%20V(h)}%20\sum_{i=1}^l%20[y_i%20=%20y]%20K(\frac{\rho(x,%20x_i)}{h}))

Дано: xl - обучающая выборка, z - классифицируемый объект и параметр h (ширина окна).

Вычисляем сумму весов объектов, относящихся к одному и тому же классу:

```R
for (i in 1:l)
  {
    counts[which(xl[i,n+1]==list)] =
      counts[which(xl[i,n+1]==list)] + kernel(euclideanDistance(z,xl[i,1:n])/h)
  }
  ```
  
Класс с максимальным весом и является классом заданного классифицируемого объекта:

```R
return (list[which.max(counts)])
```

## Вывод:
![alt text](https://i.screenshot.net/s/nrlm9id)


