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

import org.jboss.tools.usage.FocusPoint;
import org.jboss.tools.usage.util.HttpEncodingUtils;

import junit.framework.TestCase;

/**
 * @author Andre Dietisheim
 */
public class FocusPointTest extends TestCase {

	private static final String root = "root";
	
	private static final String child1 = "child1";
	
	private static final String URI_SEPARATOR_ENCODED = HttpEncodingUtils.checkedEncodeUtf8(FocusPoint.URI_SEPARATOR);
	
	private static final String TITLE_SEPARATOR_ENCODED = HttpEncodingUtils.checkedEncodeUtf8(FocusPoint.TITLE_SEPARATOR);
	
	public void testGetContentURI_Simple() throws Exception {
		FocusPoint focusPoint = new FocusPoint(root);
		String contentURI = focusPoint.getContentURI();
		assertNotNull(contentURI);
		assertEquals(URI_SEPARATOR_ENCODED + root, contentURI);
	}

	public void testGetContentURI_OneLevel() throws Exception {
		FocusPoint focusPoint = new FocusPoint(root).setChild(new FocusPoint(child1));
		String contentURI = focusPoint.getContentURI();
		assertNotNull(contentURI);
		assertEquals(URI_SEPARATOR_ENCODED + root + URI_SEPARATOR_ENCODED + child1, contentURI);
	}

	public void testGetContentTitle_Simple() throws Exception {
		FocusPoint focusPoint = new FocusPoint(root);
		String contentTitle = focusPoint.getContentTitle();
		assertNotNull(contentTitle);
		assertEquals(root, contentTitle);
	}

	public void testGetContentTitle_OneLevel() throws Exception {
		FocusPoint focusPoint = new FocusPoint(root).setChild(new FocusPoint(child1));
		String contentTitle = focusPoint.getContentTitle();
		assertNotNull(contentTitle);
		assertEquals(root + TITLE_SEPARATOR_ENCODED + child1, contentTitle);
	}
}
