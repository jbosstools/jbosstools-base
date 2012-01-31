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
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
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
public class XMLClassHyperlinkPartitioner extends AbstractHyperlinkPartitioner {
	public static final String XML_CLASS_PARTITION = "org.jboss.tools.common.text.ext.xml.XML_CLASS"; //$NON-NLS-1$

	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, int offset, IHyperlinkRegion superRegion) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			IHyperlinkRegion r = getRegion(document, offset);
			if (r == null) return null;
			
			String axis = getAxis(document, offset);
			String contentType = superRegion.getContentType();
			String type = XML_CLASS_PARTITION;
			
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

			if (n == null || !(n instanceof Attr || n instanceof Text)) return null;
			
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);

			if (start < 0 || start > offset || end < offset) return null;

			String text = document.get(start, end - start);
			StringBuffer sb = new StringBuffer(text);

			//find start and end of class property
			int bStart = 0;
			int bEnd = text.length() - 1;

			while (bStart < bEnd && (Character.isWhitespace(sb.charAt(bStart)) 
					|| sb.charAt(bStart) == '\"' || sb.charAt(bStart) == '\"')) { 
				bStart++;
			}
			while (bEnd > bStart && (Character.isWhitespace(sb.charAt(bEnd)) 
					|| sb.charAt(bEnd) == '\"' || sb.charAt(bEnd) == '\"')) { 
				bEnd--;
			}
			bEnd++;

			final int propStart = bStart + start;
			final int propLength = bEnd - bStart;
			
			if (propStart > offset || propStart + propLength < offset) return null;
	
			// Check that text is a 'class name'
			text = document.get(bStart + start, bEnd - bStart);
			int positionInText = offset - start - bStart;
			if (positionInText >= text.length()) return null;
			
			int classNameStart = positionInText;
			while (classNameStart > 0 && !Character.isWhitespace(text.charAt(classNameStart))) {
				classNameStart--;
			}
			boolean firstOrAfterDot = true;
			int classNameEnd = classNameStart;
			for (; classNameEnd < text.length(); classNameEnd++) {
				if (Character.isWhitespace(text.charAt(classNameEnd)))
					break; // End of class name
				if ('.' == text.charAt(classNameEnd)) {
					if (firstOrAfterDot) return null; // Double DOT or first char is DOT
					firstOrAfterDot = true;
					continue;
				}
				
				if (firstOrAfterDot) {
					if (!Character.isJavaIdentifierStart(text.charAt(classNameEnd))) return null; // Not Java Id-r start
					firstOrAfterDot = false;
				} else {
					if (!Character.isJavaIdentifierPart(text.charAt(classNameEnd))) return null; // Not Java Id-r part
				}
			}
			
			String className = text.substring(classNameStart, classNameEnd).trim();
			if (className.length() == 0 ||
					className.startsWith(".") || //$NON-NLS-1$
					className.endsWith(".")) { //$NON-NLS-1$
				return null;
			}
			IHyperlinkRegion region = new HyperlinkRegion(propStart, propLength, null, null, null);
			return region;
		} catch (BadLocationException x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}
}
