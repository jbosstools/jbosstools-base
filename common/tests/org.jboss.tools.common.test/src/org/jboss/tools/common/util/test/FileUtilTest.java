/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.util.test;

import junit.framework.TestCase;

import org.jboss.tools.common.util.FileUtil;

public class FileUtilTest extends TestCase {

	public void testDoctypeHTML() {
		assertTrue(FileUtil.isDoctypeHTML("<!dOCTYPE \tHtml>"));
		assertTrue(FileUtil.isDoctypeHTML("\n\n\n \t<!dOCTYPE \tHtml>"));
		assertTrue(FileUtil.isDoctypeHTML("<!-- --><!dOCTYPE \tHtml>"));
		assertTrue(FileUtil.isDoctypeHTML("<!-- <html>\n</html> --><!dOCTYPE \tHtml>"));
		assertFalse(FileUtil.isDoctypeHTML("<!dOCTYPE xHtml>"));
	}
	
}
