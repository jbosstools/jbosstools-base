/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.jface.text.IDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * @author Jeremy
 */
public class XMLElementNameHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String XML_ELEMENT_NAME_PARTITION = "org.jboss.tools.common.text.ext.xml.XML_ELEMENT_NAME"; //$NON-NLS-1$

	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, int offset, IHyperlinkRegion superRegion) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			if (n == null || !(n instanceof IDOMElement)) return null;
			IHyperlinkRegion r = getRegion(document, offset);
			if (r == null) return null;// document.get(r.getOffset(), r.getLength())
			
			String axis = getAxis(document, offset);
			String contentType = superRegion.getContentType();
			String type = XML_ELEMENT_NAME_PARTITION;

			return new HyperlinkRegion(r.getOffset(), r.getLength(), axis, contentType, type);
		} finally {
			smw.dispose();
		}
	}
	
	public static IHyperlinkRegion getRegion(IDocument document, final int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			if (n == null || !(n instanceof IDOMElement)) return null;
			
			IDOMElement elem = (IDOMElement)n;
			String tagName = elem.getTagName();
			int start = elem.getStartOffset();
			int nameStart = start + (elem.isEndTag() ? "</" : "<").length(); //$NON-NLS-1$ //$NON-NLS-2$
			int nameEnd = nameStart + tagName.length();

			if (nameStart > offset || nameEnd <= offset) return null;

			return new HyperlinkRegion(nameStart, nameEnd - nameStart, null, null, null);
		} finally {
			smw.dispose();
		}
	}
	
	/**
	 * @see com.ibm.sse.editor.extensions.hyperlink.IHyperlinkPartitionRecognizer#recognize(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	public boolean recognize(IDocument document, int offset, IHyperlinkRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return false;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			if (!(n instanceof IDOMElement)) return false;

			IDOMElement elem = (IDOMElement)n;
			String tagName = elem.getTagName();
			int start = elem.getStartOffset();
			int nameStart = start + (elem.isEndTag() ? "</" : "<").length(); //$NON-NLS-1$ //$NON-NLS-2$
			int nameEnd = nameStart + tagName.length();
			return (offset >= nameStart && offset <= nameEnd);
		} finally {
			smw.dispose();
		}
	}
}
