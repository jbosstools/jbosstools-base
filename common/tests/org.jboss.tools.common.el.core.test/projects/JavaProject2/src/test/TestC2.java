package test;

/**
 * Test checks that info for method foo() returned by TypeInfoCollector is created for TestC2.foo() 
 * rather than for overriden methods from super classes.
 */
public class TestC2 extends TestC3 {

	public String foo() {
		return null;
	}
}