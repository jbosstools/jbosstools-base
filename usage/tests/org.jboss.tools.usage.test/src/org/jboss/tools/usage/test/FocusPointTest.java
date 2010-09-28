/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.jboss.tools.usage.tracker.IFocusPoint;
import org.jboss.tools.usage.tracker.internal.FocusPoint;
import org.jboss.tools.usage.util.HttpEncodingUtils;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class FocusPointTest {

	private static final String ROOT = "root";
	
	private static final String CHILD1 = "child1";
	
	private static final String URI_SEPARATOR_ENCODED = HttpEncodingUtils.checkedEncodeUtf8(FocusPoint.URI_SEPARATOR);
	
	private static final String TITLE_SEPARATOR_ENCODED = HttpEncodingUtils.checkedEncodeUtf8(FocusPoint.TITLE_SEPARATOR);
	
	@Test
	public void testGetContentURI_Simple() throws Exception {
		IFocusPoint focusPoint = new FocusPoint(ROOT);
		String contentURI = focusPoint.getURI();
		assertNotNull(contentURI);
		assertEquals(URI_SEPARATOR_ENCODED + ROOT, contentURI);
	}

	@Test
	public void testGetContentURI_OneLevel() throws Exception {
		IFocusPoint focusPoint = new FocusPoint(ROOT).setChild(new FocusPoint(CHILD1));
		String contentURI = focusPoint.getURI();
		assertNotNull(contentURI);
		assertEquals(URI_SEPARATOR_ENCODED + ROOT + URI_SEPARATOR_ENCODED + CHILD1, contentURI);
	}

	@Test
	public void testGetContentTitle_Simple() throws Exception {
		IFocusPoint focusPoint = new FocusPoint(ROOT);
		String contentTitle = focusPoint.getTitle();
		assertNotNull(contentTitle);
		assertEquals(ROOT, contentTitle);
	}

	@Test
	public void testGetContentTitle_OneLevel() throws Exception {
		IFocusPoint focusPoint = new FocusPoint(ROOT).setChild(new FocusPoint(CHILD1));
		String contentTitle = focusPoint.getTitle();
		assertNotNull(contentTitle);
		assertEquals(ROOT + TITLE_SEPARATOR_ENCODED + CHILD1, contentTitle);
	}
}
