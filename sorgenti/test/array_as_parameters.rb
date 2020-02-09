=begin
Testing arrays as parameters of functions, with different modalities
=end

def int main()
	char[2][3] arr = [['a','b','c'],['d','e','f']]

	workingWithArray(arr, arr, arr[1])

	return 0
end

def char[3]* workingWithArray(char[2][3] a, ref char[2][3] b, valres char[3] c)

	return &b[0]

end