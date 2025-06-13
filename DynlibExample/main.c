/* Программа на языке C, использующая бибоиотеку на Паскале. */

/* Стандартный ввод/вывод */
#include <stdio.h>

/* Объявления внешних подпрограмм */

/* Сложить два целых числа */
int add_numbers(int, int);
/* Вывести сообщение message, начиная строку с 'Message: ' */
void print_message(const char *message);

/* Главная программа */
int main() {
    /* Тест функции add_numbers. Должен отобразить '42 bratyxa' */
	printf("%d bratyxa", add_numbers(40, 2) );
	putchar('\n'); /* Перевод строки */

	/* Тест функции (процедуры) print_message.
	   Должен отобразить 'Message: The cake is a lie' */
	print_message("The cake is a lie");
}
