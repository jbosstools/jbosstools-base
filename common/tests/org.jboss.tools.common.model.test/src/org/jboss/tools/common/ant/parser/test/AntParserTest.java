/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
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
