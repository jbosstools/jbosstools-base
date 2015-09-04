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
package org.jboss.tools.runtime.test;

import static org.jboss.tools.runtime.core.util.RuntimeModelUtil.IN_LINE_DELIMITER;
import static org.jboss.tools.runtime.core.util.RuntimeModelUtil.LINE_DELIMITER;

import java.io.ByteArrayInputStream;
import java.util.Set;

import junit.framework.TestCase;

import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeModelUtil;
import org.junit.Test;

public class RuntimePathUtilTest extends TestCase {
	@Test
	public void testSimplePropertyFile() {
		String twoLine = 
				"name1=/home/some/path" + IN_LINE_DELIMITER
				+ "true" + LINE_DELIMITER 
				+ "name2=/home/some/path2" + IN_LINE_DELIMITER
				+ "false" + LINE_DELIMITER; 
		byte[] asBytes = twoLine.getBytes();
		ByteArrayInputStream is = new ByteArrayInputStream(asBytes);
		Set<RuntimePath> paths = RuntimeModelUtil.parseRuntimeFile(is, true);
		assertEquals(2, paths.size());
		
		is = new ByteArrayInputStream(asBytes);
		paths = RuntimeModelUtil.parseRuntimeFile(is, false);
		assertEquals(0, paths.size());
	}
	
	@Test
	public void testDuplicatePathDifferentNameRescan() {
		String twoLine = 
				"name1=/home/some/path" + IN_LINE_DELIMITER
				+ "true" + LINE_DELIMITER 
				+ "name2=/home/some/path" + IN_LINE_DELIMITER
				+ "false" + LINE_DELIMITER;
		ByteArrayInputStream is = new ByteArrayInputStream(twoLine.getBytes());
		Set<RuntimePath> paths = RuntimeModelUtil.parseRuntimeFile(is, true);
		assertEquals(1, paths.size());
		
		is = new ByteArrayInputStream(twoLine.getBytes());
		paths = RuntimeModelUtil.parseRuntimeFile(is, false);
		assertEquals(0, paths.size());
	}
}
