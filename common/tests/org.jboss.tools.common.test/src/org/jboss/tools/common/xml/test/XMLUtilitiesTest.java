/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.xml.test;

import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Element;

import junit.framework.TestCase;

public class XMLUtilitiesTest extends TestCase {

	public final void testCreateDocumentBuilder() {
		assertNotNull("Element builder without validation wasn't created", XMLUtilities.createDocumentBuilder(true)); // TODO
		assertNotNull("Element builder with validation wasn't created", XMLUtilities.createDocumentBuilder(true)); // TODO		
	}

	public static final String ELEMENT_NAME = "ElementName";
	
	public final void testCreateDocumentElementString() {
		Element element = XMLUtilities.createDocumentElement(ELEMENT_NAME);
		assertNotNull(element);
		assertTrue(element.getNodeName().equals(ELEMENT_NAME));
	}

}
