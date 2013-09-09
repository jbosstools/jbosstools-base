package test;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;


@Target(FIELD)
@Retention(RUNTIME)
public @interface MyAnnotation {
	String name() default "x" + "y";
	int age() default 2 + 3;
	int value() default 0;
}
