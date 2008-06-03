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
package org.jboss.tools.common.model.util;

import java.io.*;
import org.w3c.dom.*;
import org.xml.sax.*;

import org.jboss.tools.common.xml.SAXValidator;
import org.jboss.tools.common.xml.XMLEntityResolver;
import org.jboss.tools.common.xml.XMLUtilities;
import javax.xml.parsers.*;

public class XMLUtil {

    public static boolean hasAttribute(Element e, String s) {
        return XMLUtilities.hasAttribute(e, s);
    }

    public static Element[] getChildren(Element parent, String name){
        return XMLUtilities.getChildren(parent, name);
    }

    public static Element getUniqueChild(Element parent, String name){
        return XMLUtilities.getUniqueChild(parent, name);
    }

    public static Element getFirstChild(Element parent, String name) {
        return XMLUtilities.getFirstChild(parent, name);
    }

    public static Element[] getAncestors(Element parent, String name) {
        return XMLUtilities.getAncestors(parent, name);
    }

    public static Element createElement(Element parent, String path) {
        return XMLUtilities.createElement(parent, path);
    }

	public static DocumentBuilder createDocumentBuilder() {
		return XMLUtilities.createDocumentBuilder();
	}

	public static DocumentBuilder createDocumentBuilder(boolean validating) {
		return XMLUtilities.createDocumentBuilder(validating);
	}

    public static Element createDocumentElement(String name) {
        return XMLUtilities.createDocumentElement(name);
    }

    public static Element createDocumentElement(String name, String qName, String publicId, String systemId, String namespaceURI) {
        return XMLUtilities.createDocumentElement(name, qName, publicId, systemId, namespaceURI);
    }

    public static Element getElement(String filename) {
        return XMLUtilities.getElement(filename, XMLEntityResolver.getInstance());
    }

    public static Element getElement(File file) {
        return XMLUtilities.getElement(file, XMLEntityResolver.getInstance());
    }

    public static Element getElement(Reader reader) {
        return XMLUtilities.getElement(reader, XMLEntityResolver.getInstance());
    }

    public static Document getDocument(Reader reader) {
        return XMLUtilities.getDocument(reader, XMLEntityResolver.getInstance());
    }

    public static Element getElement(InputStream is) {
        return XMLUtilities.getElement(is, XMLEntityResolver.getInstance());
    }

    public static Element getElement(InputSource is) throws Exception {
        return XMLUtilities.getElement(is, XMLEntityResolver.getInstance());
    }
    
    public static Document getDocument(InputSource is) throws Exception {
        return XMLUtilities.getDocument(is, XMLEntityResolver.getInstance());
    }

	public static String[] getXMLErrors(Reader reader) {
        return XMLUtilities.getXMLErrors(reader, XMLEntityResolver.getInstance());
	}
    
    public static String[] getXMLErrors(Reader reader, boolean checkDTD) {
        return XMLUtilities.getXMLErrors(reader, checkDTD, XMLEntityResolver.getInstance());
    }
    
	public static String[] getXMLErrors(org.xml.sax.InputSource is) {
        return XMLUtilities.getXMLErrors(is, XMLEntityResolver.getInstance());
	}
		
    public static String[] getXMLErrors(org.xml.sax.InputSource is, boolean checkDTD) {
        return XMLUtilities.getXMLErrors(is, checkDTD, XMLEntityResolver.getInstance());
    }

    public static String[] getXMLErrors(Reader reader, boolean checkDTD, boolean checkSchema) {
    	return (checkSchema) ? new SAXValidator().getXMLErrors(reader)
				: getXMLErrors(reader, checkDTD);
    }
    
}
