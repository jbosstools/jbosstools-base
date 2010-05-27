package org.jboss.tools.common.model.util.test;

import java.util.Properties;

import org.jboss.tools.common.model.util.Paths;

import junit.framework.TestCase;

public class PathsTest extends TestCase {
	static Properties p = new Properties();
	static {
		p.put("p1","pvalue1");
		p.put("p2","pvalue2");
		p.put("p3","pvalue3");
		p.put("p4","pvalue4");
	}

	public void testExpandNothingToExpand() {
		final String value = 
			"Value for p1 is p1\n" + 
			"Value for p2 is p2\n" +
			"Value for p3 is p3\n" +
			"Value for p4 is p4\n" +
			"No more values";
		String actual = Paths.expand(value, p);
		assertEquals(value, actual);
	}
	
	public void testExpandMultipleCorrectVars() {
		final String value = 
			"Value for p1 is %p1%\n" + 
			"Value for p2 is %p2%\n" +
			"Value for p3 is %p3%\n" +
			"Value for p4 is %p4%\n" +
			"No more values";
		String actual = Paths.expand(value, p);
		String expected = value.replace("%p1%", p.getProperty("p1"));
		expected = expected.replace("%p2%", p.getProperty("p2"));
		expected = expected.replace("%p3%", p.getProperty("p3"));
		expected = expected.replace("%p4%", p.getProperty("p4"));
		assertEquals(expected, actual);
	}
	
	public void testExpandMultipleCorrectVarsStartsFromVariable() {
		final String value = 
			"%p1%\n" + 
			"Value for p2 is %p2%\n" +
			"Value for p3 is %p3%\n" +
			"Value for p4 is %p4%" +
			"No more values";
		String actual = Paths.expand(value, p);
		String expected = value.replace("%p1%", p.getProperty("p1"));
		expected = expected.replace("%p2%", p.getProperty("p2"));
		expected = expected.replace("%p3%", p.getProperty("p3"));
		expected = expected.replace("%p4%", p.getProperty("p4"));
		assertEquals(expected, actual);
	}
	
	public void testExpandMultipleCorrectVarsNoTextOnlyVars() {
		final String value = 
			"%p1%%p2%%p3%%p4%";
		String actual = Paths.expand(value, p);
		String expected = value.replace("%p1%", p.getProperty("p1"));
		expected = expected.replace("%p2%", p.getProperty("p2"));
		expected = expected.replace("%p3%", p.getProperty("p3"));
		expected = expected.replace("%p4%", p.getProperty("p4"));
		assertEquals(expected, actual);
	}

	public void testExpandMultipleVarsWithClosingVarError() {
		final String value = 
			"%p1% %p2% %p3% %p4";
		String actual = Paths.expand(value, p);
		String expected = value.replace("%p1%", p.getProperty("p1"));
		expected = expected.replace("%p2%", p.getProperty("p2"));
		expected = expected.replace("%p3%", p.getProperty("p3"));
		expected = expected.replace("%p4%", p.getProperty("p4"));
		assertEquals(expected, actual);
	}

	public void testExpandMultipleVarsWithSpaceInVarError() {
		final String value = 
			"%p1 % %p2% %p3% %p4";
		String actual = Paths.expand(value, p);
		String expected = value.replace("%p1%", p.getProperty("p1"));
		expected = expected.replace("%p2%", p.getProperty("p2"));
		expected = expected.replace("%p3%", p.getProperty("p3"));
		expected = expected.replace("%p4%", p.getProperty("p4"));
		assertEquals(expected, actual);
	}
}
