# [DigitHoles](/tc?module=ProblemDetail&rd=14236&pm=11127)
*Single Round Match 483 Round 1 - Division II, Level One*

## Statement
Elly's evil teacher assistant Torb has given her the following puzzle: 42 -> 1, 1337 -> 0, 669 -> 3, 1882 -> 4, 688 -> 5, 12345 -> 1, 67890 -> 5, 123 -> 0, 456 -> 2, 789 -> 3. Given this information, 45678 -> ? Thanks to her fast thinking and problem-solving intuition she has found the solution: Google. The answer turned out to be the total number of "holes" in the digits of the number's decimal representation (with no extra leading zeroes). We can see that the digits 1, 2, 3, 5, and 7 contain no holes, 0, 4, 6, and 9 each has one hole, and 8 contains two holes. Therefore the answer to the puzzle is 45678 -> 1 + 0 + 1 + 0 + 2 = 4.

You want to impress Elly, so you decide to write a program that will find the correct answer for certain integers. Given an int *number*, return the total number of holes in that number.

## Definitions
- *Class*: `DigitHoles`
- *Method*: `numHoles`
- *Parameters*: `int`
- *Returns*: `int`
- *Method signature*: `int numHoles(int number)`

## Notes
- In some fonts, the digit '4' might not contain an enclosed hole, but for this problem you should assume it does.

## Constraints
- *number* will be between 1 and 1000, inclusive.

## Examples
### Example 1
#### Input
<c>42</c>
#### Output
<c>1</c>
#### Reason
4 has one hole, and 2 has no holes.

### Example 2
#### Input
<c>669</c>
#### Output
<c>3</c>
#### Reason
Both sixes are counted.

### Example 3
#### Input
<c>688</c>
#### Output
<c>5</c>
#### Reason
Note that 8 is the only digit that has 2 holes.

### Example 4
#### Input
<c>123</c>
#### Output
<c>0</c>
#### Reason
A number without holes.

### Example 5
#### Input
<c>456</c>
#### Output
<c>2</c>
### Example 6
#### Input
<c>789</c>
#### Output
<c>3</c>

