def sum_of_fifth_powers_of_digits(n):
    return sum(int(digit)**5 for digit in str(n))

upper_bound = 6 * (9**5)
answer = 0
for i in range(2, upper_bound):
    if i == sum_of_fifth_powers_of_digits(i):
        answer += i

print(answer)
