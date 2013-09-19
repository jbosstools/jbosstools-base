package test;

import test.Constants.Inner;
import static test2.MyConstants.Values.QUANTITY;
import static test2.MoreConstants.MoreValues.AMOUNT;
import static test2.MyAnnotation.SomeValues.SOME_AMOUNT;
import javax.annotation.Priority;
import static javax.interceptor.Interceptor.Priority.APPLICATION;

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
	
	@MyAnnotation(age = Inner.NUMBER)
	int f9 = 0;
	
	@MyAnnotation(age = QUANTITY)
	int f10 = 0;
	
	@MyAnnotation(age = AMOUNT)
	int f11 = 0;
	
	@MyAnnotation(age = SOME_AMOUNT)
	int f12 = 0;

	@Priority(APPLICATION)
	int f13 = 0;
}
