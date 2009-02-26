package org.jboss.tools.common.ant.parser.test;

import org.jboss.tools.common.ant.parser.AntParser;

import junit.framework.TestCase;

public class AntParserTest extends TestCase {
	
	public static final String ANT_FILE_CONTENT="<project><target name=\"name1\"/><target name=\"name2\"/><target name=\"name3\"/></project>";
	
	public void testGetTargets() {
		AntParser parser = new AntParser(ANT_FILE_CONTENT);
		assertEquals(3,parser.getTargets().length);
	}

}
