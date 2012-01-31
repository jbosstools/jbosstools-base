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

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * @author Jeremy
 */
public abstract class XMLLinkHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, int offset, IHyperlinkRegion superRegion) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			IRegion r = getRegion(document, offset);
			if (r == null) return null;
			
			String axis = getAxis(document, offset);
			String contentType = superRegion.getContentType();
			String type = getPartitionType();
			
			return new HyperlinkRegion(r.getOffset(), r.getLength(), axis, contentType, type);
		} finally {
			smw.dispose();
		}
	}
	
	protected abstract String getPartitionType();

	public IRegion getRegion(IDocument document, final int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n == null || !(n instanceof Attr || n instanceof Text)) return null;
			
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);
			
			String text = document.get(start, end - start);
			StringBuffer sb = new StringBuffer(text);

			int bStart = 0; 
			int bEnd = sb.length();
			
			// In case of attribute value we need to skip leading and ending quotes && whitespaces
			while (bStart < bEnd && (sb.charAt(bStart) == '"' || sb.charAt(bStart) == '\'' ||
					sb.charAt(bStart) == 0x09 || sb.charAt(bStart) == 0x0A ||
					sb.charAt(bStart) == 0x0D || sb.charAt(bStart) == 0x20)) {
				bStart++;
			}
			
			while (bEnd - 1 > bStart && (sb.charAt(bEnd - 1) == '"' || sb.charAt(bEnd - 1) == '\'' ||
					sb.charAt(bEnd - 1) == 0x09 || sb.charAt(bEnd - 1) == 0x0A ||
					sb.charAt(bEnd - 1) == 0x0D || sb.charAt(bEnd - 1) == 0x20)) {
				bEnd--;
			}
			if (start + bStart > offset || start + bEnd - 1 < offset) return null;
			
			//find start and end of path property
			while (bStart >= 0) { 
				if (!Character.isJavaIdentifierPart(sb.charAt(bStart)) &&
						sb.charAt(bStart) != '\\' && sb.charAt(bStart) != '/' &&
						sb.charAt(bStart) != ':' && sb.charAt(bStart) != '-' &&
						sb.charAt(bStart) != '.' && sb.charAt(bStart) != '_' &&
						sb.charAt(bStart) != '%' && sb.charAt(bStart) != '?' &&
						sb.charAt(bStart) != '&' && sb.charAt(bStart) != '=') {
					bStart++;
					break;
				}
			
				if (bStart == 0) break;
				bStart--;
			}
			// find end of bean property
			bEnd = bStart;
			while (bEnd < sb.length()) { 
				if (!Character.isJavaIdentifierPart(sb.charAt(bEnd)) &&
						sb.charAt(bEnd) != '\\' && sb.charAt(bEnd) != '/' &&
						sb.charAt(bEnd) != ':' && sb.charAt(bEnd) != '-' &&
						sb.charAt(bEnd) != '.' && sb.charAt(bEnd) != '_' &&
						sb.charAt(bEnd) != '%' && sb.charAt(bEnd) != '?' &&
						sb.charAt(bEnd) != '&' && sb.charAt(bEnd) != '=') {
					break;
				}
				bEnd++;
			}

			int propStart = bStart + start;
			int propLength = bEnd - bStart;
			if (propStart > offset + 1 || propStart + propLength < offset) return null;
			
			return new HyperlinkRegion(propStart, propLength);
		} catch (BadLocationException x) {
			// Ignore
			return null;
		} finally {
			smw.dispose();
		}
	}
}
