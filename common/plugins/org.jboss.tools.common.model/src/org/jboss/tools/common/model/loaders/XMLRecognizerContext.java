/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.loaders;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilder;

import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class XMLRecognizerContext extends EntityRecognizerContext {
	protected boolean isDTD = false;

	protected DoctypeInfo doctypeInfo = null;
	
	static protected class DoctypeInfo {
		protected String publicId;
		protected String systemId;
		protected String name;		
	}
	static DoctypeInfo NULL_INFO = new DoctypeInfo();

	public XMLRecognizerContext(EntityRecognizerContext context) {
		super(context.getFileName(), context.getExtension(), context.getBody());
		init();
	}

	void init() {
		if(body == null) return;

		String doctypeText = getUnformattedDoctypeFromBody(body);
		if (doctypeText != null) {
			isDTD = true;
			doctypeInfo = checkDocType(doctypeText);
		}

	}

	public boolean isDTD() {
		return isDTD;
	}

	public String getPublicId() {
		return doctypeInfo == null ? null : doctypeInfo.publicId;
	}

	public String getSystemId() {
		return doctypeInfo == null ? null : doctypeInfo.systemId;
	}

	public String getRootName() {
		if(doctypeInfo != null) {
			return doctypeInfo.name;
		}
		//TODO
		return null;
	}

	public XMLRecognizerContext getXMLContext() {
		return this;
	}

	static HashMap<String, DoctypeInfo> doctypes = new HashMap<String, DoctypeInfo>();

	private DoctypeInfo checkDocType(String docTypeString) {
		if(doctypes.containsKey(docTypeString)) {
			return doctypes.get(docTypeString);
		}
		Reader xml = new StringReader(docTypeString + "<root></root>"); //$NON-NLS-1$
		DocumentBuilder db = XMLUtilities.createDocumentBuilder(false);
		if (db == null) 
			return NULL_INFO;
	
		try {
			db.setErrorHandler(new ErrorHandler() {
				public void warning(SAXParseException exception)
						throws SAXException {
				}

				public void fatalError(SAXParseException exception)
						throws SAXException {
				}

				public void error(SAXParseException exception)
						throws SAXException {
				}
			});
			db.setEntityResolver(new EntityResolver() {
				public InputSource resolveEntity(String publicId,
						String systemId) throws SAXException, IOException {
					return new InputSource(new StringReader("")); //$NON-NLS-1$
				}
			});
			Document doc = db.parse(new InputSource(xml));
			if(doc != null) {
				DocumentType dt = doc.getDoctype();
				DoctypeInfo doctypeInfo = new DoctypeInfo();
				doctypeInfo.publicId = dt.getPublicId();
				doctypeInfo.systemId = dt.getSystemId();
				doctypeInfo.name = dt.getName();
				//'docTypeString' is a substring of the complete text of the file,
				//which can be very long; and this substring keeps reference to
				//char[] of the entire text. Adding an empty string ensures
				//that the result will not keep the reference. 
				doctypes.put("" + docTypeString, doctypeInfo);
				// doctypeInfo.publicId + " " + doctypeInfo.systemId + " " +
				// doctypeInfo.name);
				return doctypeInfo;
			}
		} catch (SAXException e1) {
			//ignore - doctype is corrupted
		} catch (IOException e2) {
			//ignore - impossible
		} finally {
			try {
				xml.close();
			} catch (IOException e) {
			}
		}
		//See comment to 'docTypeString' above.
		doctypes.put("" + docTypeString, NULL_INFO);
		return NULL_INFO;
	}

	private String getUnformattedDoctypeFromBody(String body) {
		int i = body.indexOf("<!DOCTYPE"); //$NON-NLS-1$
		if (i < 0)
			return null;
		int j = body.indexOf(">", i); //$NON-NLS-1$
		if (j < 0)
			return null;
		return body.substring(i, j+1);
	}

}
