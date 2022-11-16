# MyWavelet
---
Данный алгоритм реализует Вейвлет преобразование временного ряда при помощи вейвлета Морле. Код находится в папке **bin**

Пока что интерфейс сырой и неудобный. Для работы программы нужны два файла: 
    
1. "t.dat" --- временная сетка 

2. "x.dat" --- значения ряда

В первой строке должно быть указано целое число --- количество элементов в ряду.

Чтобы не загромождать дирректорию результаты работы программы помещаются в соответствующие дирректории

- tabs    --- текстовые таблицы 
- images  --- графики

---

В папке test лежат тесты: 

1. test.f95      --- синусоида с гауссовым шумом
2. test\_2sin.f95 --- две последовательно включающиеся синусоиды
3. test\_movesin.f95 --- синусоида с изменяющимся периодом
4. test\_switch   --- переключение двух синусоид
5. test\_auto     --- авторегрессионая модель 

## Запуск выполняется при помощи make-файла. Ниже возможные опции.

Сборка исполнимого файла wavelet

- $ make built 

Запуск алгоритма со своими данными
- $make run

Удаление объектников из директории
- $ make clean

Запуск теста 1)
- $make test\_sin

Запуск теста 2)
- $make test\_2sin

Запуск теста 3)
- $make test\_movesin

Запуск теста 4)
- $make test\_switch

Запуск теста 5)
- $make test\_auto

