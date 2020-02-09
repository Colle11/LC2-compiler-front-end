def swapByValue(int *px, int *py)
	int temp = 0;

	temp = *px;
	*px = *py;
	*py = temp;
end

def swapByRef(ref int x, ref int y)
	int temp = 0;

	temp = x;
	x = y;
	y = temp;
end

def int main()
	int a=5; int b=10;

	swapByValue(&a, &b)

	swapByRef(a, b)

	return 0
end