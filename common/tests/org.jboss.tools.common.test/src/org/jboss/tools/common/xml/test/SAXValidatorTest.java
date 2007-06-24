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

import java.io.StringReader;

import org.jboss.tools.common.xml.SAXValidator;
import org.xml.sax.InputSource;

import junit.framework.TestCase;

public class SAXValidatorTest extends TestCase {

	
	public static final String GOOD_XML = 
		"<?xml version=\"1.0\"?>" +
			"<!DOCTYPE node1 [" +
		                "<!ELEMENT node1 (node2)>" +
		                "<!ELEMENT node2 (#PCDATA)>" +
		    "]>" +
		"<node1><node2/></node1>";

	public static final String BAD_XML = 
		"<?xml version=\"1.0\"?>" +
			"<!DOCTYPE node1 [" +
		                "<!ELEMENT node1 (node2)>" +
		                "<!ELEMENT node2 (#PCDATA)>" +
		    "]>" +
		"<node1><node2/></node3></node1>";
	
	public void testGetXMLErrorsInputSource() {
		SAXValidator validator = new SAXValidator();
		InputSource is = new InputSource(new StringReader(GOOD_XML));
		String[] errors = validator.getXMLErrors(is);
		assertTrue("There should be no error in simple exml parsing", errors.length==0);
	}

	public void testGetXMLErrorsReader() {
		SAXValidator validator = new SAXValidator();
		String[] errors = validator.getXMLErrors(new StringReader(GOOD_XML));
		assertTrue("There should be no error in simple exml parsing", errors.length==0);
	}

	public void testGetXMLErrorsInputSourceNegative() {
		SAXValidator validator = new SAXValidator();
		InputSource is = new InputSource(new StringReader(BAD_XML));
		String[] errors = validator.getXMLErrors(is);
		assertTrue("There should be 1 error in simple exml parsing", errors.length==1);
	}

	public void testGetXMLErrorsReaderNegative() {
		SAXValidator validator = new SAXValidator();
		String[] errors = validator.getXMLErrors(new StringReader(BAD_XML));
		assertTrue("There should be 1 error in simple exml parsing", errors.length==1);
	}
	
}
