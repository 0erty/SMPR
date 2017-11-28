# KNN
KNN - алгоритм в котором задана обучающая выборка и множество классов. Требуется найти к какому классу относится выбранный объект. В случае использования метода для классификации объект присваивается тому классу, который является наиболее распространённым среди k соседей данного элемента, классы которых уже известны.

Определение класса

При таком способе во внимание принимается не только количество попавших в область определенных классов, но и их удаленность от нового значения.

Для каждого класса j определяется оценка близости:

![alt text](https://wikimedia.org/api/rest_v1/media/math/render/svg/3ade505a0c21f3115a7af19d1bb8244f4a1b63d8) где d(x, a) — дистанция от нового значения x до объекта а.

У какого класса выше значение близости, тот класс и присваивается новому объекту.

Вывод:
![alt text](https://screenshot.net/lq2yrtz)


# LOO

Является частным случаем полного скользящего контроля при k=1, соотвественно, N=L. Это, пожалуй, самый распространённый вариант скользящего контроля.

Преимущества LOO в том, что каждый объект ровно один раз участвует в контроле, а длина обучающих подвыборок лишь на единицу меньше длины полной выборки.

Недостатком LOO является большая ресурсоёмкость, так как обучаться приходится L раз. 

# Метод парзеновского окна

Метод парзеновского окна — метод байесовской классификации, основанный на непараметрическом восстановлении плотности по имеющейся выборке.

В основе подхода лежит идея о том, что плотность выше в тех точках, рядом с которыми находится большое количество объектов выборки.

Парзеновская оценка плотности имеет вид:
![alt text](http://www.machinelearning.ru/mimetex/?p_{y,h}(x)%20=%20\frac{1}{l_y%20V(h)}%20\sum_{i=1}^l%20[y_i%20=%20y]%20K(\frac{\rho(x,%20x_i)}{h}))


