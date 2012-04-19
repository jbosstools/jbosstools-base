package test;

/**
 * Test checks that info for method foo() returned by TypeInfoCollector is created for TestC1.foo() 
 * rather than for overriden methods from super classes.
 */
public class TestC1 extends TestC2 {

	public String foo() {
		return null;
	}
}