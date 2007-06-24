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
package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.jface.text.IRegion;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Node;

import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

public class XMLDoctypeHyperlink extends XMLXmlNsHyperlink {
	
	protected IRegion getRegion(int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n instanceof Attr) n = ((Attr)n).getOwnerElement();
			if ((n == null) || !(n instanceof DocumentType)) return null;
			
			int start = Utils.getValueStart(n);
			if(start < 0) return null;
			int end = Utils.getValueEnd(n);

			DocumentType node = (DocumentType)n;

			String text = "";
			try {
				text = getDocument().get(start, end - start); 
			} catch (Exception x) {};
			String publicId = (node.getPublicId() == null ? "" : node.getPublicId());
			String systemId = (node.getSystemId() == null ? "" : node.getSystemId());

			int valueStart = -1;
			int valueLength = 0;
			
			int index = -1;
			
			if (publicId.length() > 0) {
				index = text.indexOf(publicId);
				if (offset >= start + index && offset < start + index + publicId.length()) {
	 				valueStart = start + index;
	 				valueLength = publicId.length();
				}
 			} 
			if (valueStart == -1) {
				if (systemId.length() > 0) {
	 				index = text.indexOf(systemId);
	 				if (systemId.length() > 0 && offset >= start + index && offset < start + index + systemId.length()) {
		 				valueStart = start + index;
		 				valueLength = systemId.length();
	 				}
				}
 			}
			if (valueStart == -1) {
				valueStart = start;
				valueLength = end - start;
			}
			final int propStart = valueStart;
			final int propLength = valueLength;
			
			if (propStart > offset || propStart + propLength < offset) return null;
			
			IRegion region = new IRegion () {

				public int getLength() {
					return propLength;
				}

				public int getOffset() {
					return propStart;
				}

				public boolean equals(Object arg) {
					if (!(arg instanceof IRegion)) return false;
					IRegion region = (IRegion)arg;
					
					if (getOffset() != region.getOffset()) return false;
					if (getLength() != region.getLength()) return false;
					return true;
				}
			};
			return region;
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
		
	}
	
	protected String getPublicId(IRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());

			if (n instanceof Attr) n = ((Attr)n).getOwnerElement();
			if (!(n instanceof DocumentType)) return null;
			DocumentType node = (DocumentType)n;
			
			return node.getPublicId();
		} catch (Exception x) {
			//ignore
		} finally {
			smw.dispose();
		}

		return null;
	}

	protected String getSystemId(IRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());

			if (n instanceof Attr) n = ((Attr)n).getOwnerElement();
			if (!(n instanceof DocumentType)) return null;
			DocumentType node = (DocumentType)n;
			
			return node.getSystemId();
		} catch (Exception x) {
			//ignore
		} finally {
			smw.dispose();
		}

		return null;
	}


}
