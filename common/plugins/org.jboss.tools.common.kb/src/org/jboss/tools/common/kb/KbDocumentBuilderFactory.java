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
package org.jboss.tools.common.kb;

import java.io.ByteArrayInputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class KbDocumentBuilderFactory {
	
//	private static DocumentBuilderCreator creator = new DocumentBuilderCreator();

	/**
	 * 
	 */
	public static synchronized DocumentBuilder createDocumentBuilder(boolean validating) {
		try {
			return new DocumentBuilderCreator().createDocumentBuilder(validating);
		} catch (Exception t) {
			KbPlugin.getPluginLog().logError("Cannot create document builder.", t);
			return null;
		} 
	}

	static class DocumentBuilderCreator extends Thread {
		boolean validate = false;
		DocumentBuilder documentBuilder = null;

		public DocumentBuilder createDocumentBuilder(boolean validate) throws Exception {
			this.validate = validate;
			setContextClassLoader(getClass().getClassLoader());
			start();
			join();
			return documentBuilder;
		}

		public void run () {
			try {
				DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
				documentBuilderFactory.setValidating(validate);
				documentBuilder = documentBuilderFactory.newDocumentBuilder();

				if(!validate) {
					documentBuilder.setEntityResolver(new EntityResolver() {
						public InputSource resolveEntity(java.lang.String publicId, java.lang.String systemId) throws SAXException, java.io.IOException {
							if((systemId != null) && systemId.toLowerCase().endsWith(".dtd")) { // this deactivates DTD
								return new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes()));
							} else {
								return null;
							}
						}
					});
				}
			} catch (Exception t) {
				KbPlugin.getPluginLog().logError("Document builder creation failed.", t);
			}
		}
	}
}