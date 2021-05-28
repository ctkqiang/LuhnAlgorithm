# Luhn Algorithm

Luhn Algorithm is a **Modulus 10** and **Mod 10** , is a simple checksum formulae used for validating a variety of identification numbers, such as credit card numbers, IMEI numbers and Canadian Social Insurance Numbers. 

<br />

##### How is the Formulae  Work? 
Let's supposed an example of a Debit / Credit Card number ```4716 2495 3356 7731```.
First, seperate card number into seperation of array, then followed by double every 
second digit in the array. 

| 4 | 7 | 1 | 6 | 2 | 4 | 9 | 5 | 3 | 3 | 5 | 6 | 7 | 7 | 3 | 0 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|   | 14 |   | 12 |  | 8 |  | 10 |   | 6 |  | 12 |  | 14 |  | 0 |

Secondly, If the doubled value ishigher than 9, seperate those numbers
into array and get the sum of both.

| 4 | 7 | 1 | 6 | 2 | 4 | 9 | 5 | 3 | 3 | 5 | 6 | 7 | 7 | 3 | 0 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|   | 14 |   | 12 |  | 8 |  | 10 |   | 6 |  | 12 |  | 14 |  |  |
|   | 5 |   | 3 |  |  |  | 1 |   |  |  | 3 |  | 5 |  |  |

Now Override those values before 9 , to existing card number,

```latex

4 + 7 + 1 + 6 + 2 + 8 + 9 + 5 + 3 + 6 + 5 + 6 + 7 + 7 + 3 + 0 = 79

\newline
\therefore This Card Is Not Valid:: Not A Multiple Of Ten
```
