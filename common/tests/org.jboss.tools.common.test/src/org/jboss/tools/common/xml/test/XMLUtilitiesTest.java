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
package org.jboss.tools.common.xml.test;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import javax.xml.parsers.DocumentBuilder;

import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import junit.framework.TestCase;

public class XMLUtilitiesTest extends TestCase {

	public final void testCreateDocumentBuilder() {
		assertNotNull("Element builder without validation wasn't created", XMLUtilities.createDocumentBuilder()); // TODO
	}

	public static final String ELEMENT_NAME = "ElementName";
	
	public final void testCreateDocumentElementString() {
		Element element = XMLUtilities.createDocumentElement(ELEMENT_NAME);
		assertNotNull(element);
		assertTrue(element.getNodeName().equals(ELEMENT_NAME));
	}

	public void testCreateDocumentBuilderBoolean() {
		assertNotNull("Element builder with validation wasn't created", XMLUtilities.createDocumentBuilder(true)); // TODO
	}
	
	public void testGetDocumentReaderEntityResolver() throws IOException {
		assertNotNull(getGoodDocument());;
	}
	
	public void testHasAttributeElementString() {
		Element element = getGoodDocument().getDocumentElement();
		assertTrue(XMLUtilities.hasAttribute(element, "attribute1"));
	}
	
	public void testGetChildrenElementsElementString() {
		Element element = getGoodDocument().getDocumentElement();
		assertTrue(XMLUtilities.getChildren(element, "child1").length==4);
	}
	
	public void testGetUniqueChildElementString() {
		Element element = getGoodDocument().getDocumentElement();
		assertNotNull(XMLUtilities.getUniqueChild(element, "child1"));
	}
	
	public void testGetChildrenElementString() {
		Element element = getGoodDocument().getDocumentElement();
		assertTrue(XMLUtilities.getChildren(element, "child1").length==4);
	}
	
	public void testGetFirstChildElementString() {
		Element element = getGoodDocument().getDocumentElement();
		Element first = XMLUtilities.getFirstChild(element,"child1");
		assertTrue(first.getAttribute("first").equals("true"));
		first = XMLUtilities.getFirstChild(element,"child2");
		assertTrue(first.getAttribute("first").equals("true"));
		first = XMLUtilities.getFirstChild(element,"child10");
		assertNull(first);
	}
	
	public static final Document getGoodDocument() {
		InputStream in = null;
		try {
			in = XMLUtilitiesTest.class.getResourceAsStream("XMLUtilitiesTest1.xml");
			Document document = XMLUtilities.getDocument(new InputStreamReader(in), XMLUtilities.EMPTY_RESOLVER);
			assertNotNull(document);
			return document;
		} finally {
			try {
				in.close();
			} catch (IOException e) {
			}
		}
	}
}
