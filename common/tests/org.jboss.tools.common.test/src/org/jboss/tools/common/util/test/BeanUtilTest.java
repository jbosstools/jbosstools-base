/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.util.test;

import java.io.IOException;

import org.apache.commons.httpclient.HttpClient;
import org.jboss.tools.common.util.BeanUtil;
import org.jboss.tools.common.util.HttpUtil;

import junit.framework.TestCase;

public class BeanUtilTest extends TestCase {

	public void testBeanUtil() throws IOException {
		assertTrue(BeanUtil.isGetter("getX", 0));
		assertFalse(BeanUtil.isGetter("getX", 1));
		assertFalse(BeanUtil.isGetter("get", 0));
		assertFalse(BeanUtil.isGetter("g", 0));
		assertFalse(BeanUtil.isGetter("a", 0));
		assertFalse(BeanUtil.isGetter("agetX", 0));

		assertTrue(BeanUtil.isGetter("isBig", 0));
		assertFalse(BeanUtil.isGetter("is", 0));

		assertTrue(BeanUtil.isSetter("setX", 1));
		assertFalse(BeanUtil.isSetter("setX", 0));
		assertFalse(BeanUtil.isSetter("set", 1));
		assertFalse(BeanUtil.isSetter("s", 1));
		assertFalse(BeanUtil.isSetter("a", 1));
		assertFalse(BeanUtil.isSetter("asetX", 1));
		
		assertEquals("x", BeanUtil.getPropertyName("getX"));
		assertEquals("x", BeanUtil.getPropertyName("setX"));
		assertEquals("x", BeanUtil.getPropertyName("isX"));
		assertEquals("X0", BeanUtil.getPropertyName("isX0"));
		assertEquals("xo", BeanUtil.getPropertyName("isXo"));
		assertEquals("XO", BeanUtil.getPropertyName("isXO"));
	}

}
