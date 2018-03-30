# Bonus 2: Custom Solitaire
Haskell implementation that tracks the progress of a card game, Custom Solitaire, invented just for this exercise.

# Bonus 1: Counting Sundays (Project Euler: Problem 19)
Haskell implementation that finds how many Sundays fell on the first of the month during the 
twentieth century (1 Jan 1901 to 31 Dec 2000).

### Answers of the Questions

- What does the helper function ```sundays'``` calculate?
> The helper function ```sundays'``` recursively calculates how many Sundays fall on the first of the month in the given start and end year interval.

- What if you don't define a ```rest``` and use its expression where it's needed?
> It does not change the result of the function. However, it hurts readability of the code. On the other hand, it may improve the performance by reducing the unnecessary call for ```sundays'``` in the ```y > end``` case since it will call ```sundays'``` to assign it to ```rest``` even though it is not necessary in this case. 

- Is the number of weeks in 400 years an integer value? In other words, is the number of days in 400 years a multiple of 7? If so, what is the possibility that a certain day of a month (such as 1 Jan, or your birthday) is a Sunday (or some other day)? Are all days equally possible?
> Leap days occur in years that are divisible by 4. However, leap day does not occur in years that divisible by 100 unless the year is divisible by 400. Hence, the number of leap days in 100 years will be 24 (the number of years divisible by 4, but not by 100) if there is not a year divisible by 400, and 25 (the year divisible by 400 and the number of years divisible by 4, but not by 100) if there is a year divisible by 400. Given the fact that there will always be a year divisible by 400 in 400 years, the number of leap days in 400 years will always be 97 (96+1). Therefore, the number of days in 400 years will always be 146.097 which is divisible by 7. Hence, the possibility that a certain day of a month is a Sunday is same as the possibility of being Monday (or some other day).
