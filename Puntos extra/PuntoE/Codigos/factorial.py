import sys

def factorial(n, stop=0):
	o=1
	while(n>stop):
		o*=n
		n-=1 
	return o

m=10000000000

print(factorial(m))