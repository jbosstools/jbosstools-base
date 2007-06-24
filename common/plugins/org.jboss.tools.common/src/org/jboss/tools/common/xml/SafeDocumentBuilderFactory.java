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
package org.jboss.tools.common.xml;

import java.io.ByteArrayInputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.core.runtime.Platform;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class SafeDocumentBuilderFactory extends Thread {
	public static final EntityResolver EMPTY_RESOLVER = new EntityResolver() {
		public InputSource resolveEntity(java.lang.String publicId, java.lang.String systemId) throws SAXException, java.io.IOException {
			if((systemId != null) && systemId.toLowerCase().endsWith(".dtd")) { // this deactivates DTD
				return new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes()));
			} else {
				return null;
			}
		}
	};
	DocumentBuilder d = null;
	boolean validate = false;
	
	private SafeDocumentBuilderFactory(boolean validate) {
		this.validate = validate;
	}
	
	public void run () {
		ClassLoader loader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
		try {
			DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
			if(validate) f.setValidating(validate);
///			f.setExpandEntityReferences(false);
			d = f.newDocumentBuilder();
			if(!validate) {
				d.setEntityResolver(EMPTY_RESOLVER);
			}
			d.setErrorHandler(new ErrorHandlerImpl());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			Thread.currentThread().setContextClassLoader(loader);
		}
	}
	
	public static DocumentBuilder createDocumentBuilder(boolean validating) {
/*
 * This Thread call is workaround that except empty thread Context ClassLoader
 */
		SafeDocumentBuilderFactory t = new SafeDocumentBuilderFactory(validating);
		try {
			t.start();
			t.join();
		} catch (Exception e) {
			t.run();
		} 
		return t.d;
	}
	
}