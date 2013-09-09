package test;

public class Test {
	static final int V1 = 10;
	static final int V2 = 2;

	@MyAnnotation
	int f1 = 0;

	@MyAnnotation(7)
	int f2 = 0;

	@MyAnnotation(18 + 20)
	int f3 = 0;
	
	@MyAnnotation(18 + V1)
	int f4 = 0;
	
	@MyAnnotation(V2 + V1)
	int f5 = 0;
	
	@MyAnnotation(name="A" + "b")
	int f6 = 0;
	
	@MyAnnotation(name="A" + "c" + 1, age = V1 + 5)
	int f7 = 0;
	
	@MyAnnotation(age = V1 + (int)(5 + 2l))
	int f8 = 0;
	
}
