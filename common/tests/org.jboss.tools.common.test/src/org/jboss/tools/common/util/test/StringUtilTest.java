package org.jboss.tools.common.util.test;

import static org.junit.Assert.*;
import junit.framework.TestCase;

import org.jboss.tools.common.util.StringUtil;
import org.junit.Test;

public class StringUtilTest extends TestCase{

	@Test
	public void testTrimQuotes() {
		String text = "text";
		final String[] OPTIONS = new String[] {"","'","\""}; 
		for(int i = 0;i<OPTIONS.length;i++) {
			for(int j = 0;j<OPTIONS.length;j++) {
				String target = OPTIONS[i] + text + OPTIONS[j];
				System.out.println(target);
				assertTrue(StringUtil.trimQuotes(target).equals(text));		
			}
		}
	}

}
