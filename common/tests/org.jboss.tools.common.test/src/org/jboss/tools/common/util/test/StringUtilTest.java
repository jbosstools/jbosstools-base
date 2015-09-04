/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.util.test;

import junit.framework.TestCase;

import org.jboss.tools.common.util.StringUtil;
import org.junit.Test;

public class StringUtilTest extends TestCase{

	final String[] OPTIONS = new String[] {"","'","\""};
	
	@Test
	public void testTrimQuotes() {
		String text = "t";
		for(int i = 0;i<OPTIONS.length;i++) {
			for(int j = 0;j<OPTIONS.length;j++) {
				String target = OPTIONS[i] + text + OPTIONS[j];
				System.out.println(target);
				assertTrue(StringUtil.trimQuotes(target).equals(text));		
			}
		}
	}

	public void testTrimForStringLength0to1() {
		for (String charq: OPTIONS) {
			assertTrue("".equals(StringUtil.trimQuotes(charq)));	
		}
	}

}
