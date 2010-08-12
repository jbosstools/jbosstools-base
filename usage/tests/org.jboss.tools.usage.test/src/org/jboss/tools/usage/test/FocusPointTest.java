package org.jboss.tools.usage.test;

import org.jboss.tools.usage.jgoogleanalytics.FocusPoint;

import junit.framework.TestCase;

public class FocusPointTest extends TestCase {

	private static final String root = "root";
	private static final String child1 = "child1";
	private static final String child2 = "child2";
	
	public void testGetContentURI_Simple() throws Exception {
		FocusPoint focusPoint = new FocusPoint(root);
		String contentURI = focusPoint.getContentURI();
		assertNotNull(contentURI);
		assertEquals(FocusPoint.URI_SEPARATOR + root, contentURI);
	}

	public void testGetContentURI_OneLevel() throws Exception {
		FocusPoint focusPoint = new FocusPoint(root).setChild(new FocusPoint(child1));
		String contentURI = focusPoint.getContentURI();
		assertNotNull(contentURI);
		assertEquals(FocusPoint.URI_SEPARATOR + root + FocusPoint.URI_SEPARATOR + child1, contentURI);
	}

	public void testGetContentTitle_Simple() throws Exception {
		FocusPoint focusPoint = new FocusPoint(root);
		String contentTitle = focusPoint.getContentTitle();
		assertNotNull(contentTitle);
		assertEquals(root, contentTitle);
	}

	public void testGetContentTitle_OneLevel() throws Exception {
//		FocusPoint parentFocusPoint = new FocusPoint(JBOSS_TOOLS_USAGE_ID);
//		FocusPoint childFocusPoint = new FocusPoint(JBOSS_TOOLS_SMOOKS_UI,
//				parentFocusPoint);
//		String contentTitle = childFocusPoint.getContentTitle();
//		assertNotNull(contentTitle);
//		assertEquals(JBOSS_TOOLS_USAGE_ID + FocusPoint.TITLE_SEPARATOR + JBOSS_TOOLS_SMOOKS_UI, contentTitle);
	}
}
