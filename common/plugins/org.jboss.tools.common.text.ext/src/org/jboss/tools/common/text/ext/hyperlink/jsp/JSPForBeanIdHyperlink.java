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
package org.jboss.tools.common.text.ext.hyperlink.jsp;

import java.text.MessageFormat;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.StructuredSelectionHelper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * @author Jeremy
 */
public class JSPForBeanIdHyperlink extends AbstractHyperlink {
	private static final String USEBEAN_TAGNAME = "jsp:useBean"; //$NON-NLS-1$
	private static final String ID_ATTRNAME = "id"; //$NON-NLS-1$

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
		String forID = getForId(region);
		IRegion elementByID = findElementByIDBackward(forID, region.getOffset(), USEBEAN_TAGNAME);
		if (elementByID != null) {
			StructuredSelectionHelper.setSelectionAndRevealInActiveEditor(elementByID);
		} else {
			openFileFailed();
		}
	}
	
	private IRegion findElementByIDBackward (String id, int endOffset, String tagname) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			Node n = Utils.findNodeForOffset(xmlDocument, endOffset);

			if (n == null) return null;
			if (n instanceof Attr) n = ((Attr)n).getOwnerElement();
			if (n == null) return null;

			Element element = null;
			for (Node parent = n; parent != null && element == null; parent = parent.getParentNode()) {
				element = findElementByIDBackward(xmlDocument.getChildNodes(), id, endOffset, tagname); 
			}

			if (!(element instanceof IDOMElement)) return null;
			
			final int offset = Utils.getValueStart(element);
			final int length = ((IDOMElement)element).getStartStructuredDocumentRegion().getLength();
			return new Region (offset, length);
		} finally {
			smw.dispose();
		}
	}
	
	private Element findElementByIDBackward(NodeList list, String id, int endOffset, String tagName) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			for (int i = list.getLength() - 1; list != null && i >= 0; i--) {
				if(!(list.item(i) instanceof Element)) continue;
					Element element = (Element)list.item(i);
					int start = Utils.getValueStart(element);
					if (start < 0 || start >= endOffset) continue;
					
					String elementName = element.getNodeName();
					if (tagName.equals(elementName)) {
						
						Attr idAttr = (Attr)element.getAttributeNode(ID_ATTRNAME);
						if (idAttr != null) {
							String val = Utils.trimQuotes(idAttr.getNodeValue());
							if (id.equals(val)) {
								return element;
							}
						}
					}
					
					if (element.hasChildNodes()) {
						Element child = findElementByIDBackward(element.getChildNodes(), id, endOffset, tagName);
						if (child != null) return child;
					}
			}
		} finally {
			smw.dispose();
		}
		return null;
	}


	String getForId(IRegion region) {
		try {
			return Utils.trimQuotes(getDocument().get(region.getOffset(), region.getLength()));
		} catch (BadLocationException x) {
			//ignore
			return null;
		}
	}

	IRegion fLastRegion = null;
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		fLastRegion = getRegion(offset);
		return fLastRegion;
	}

	private IRegion getRegion(int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n == null || !(n instanceof Attr || n instanceof Text)) return null;
			
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);

			if (start > offset || end < offset) return null;

			String text = getDocument().get(start, end - start);
			StringBuffer sb = new StringBuffer(text);

			//find start and end of path property
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
	
			return new Region (propStart,propLength);
		} catch (BadLocationException x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getHyperlinkText()
	 */
	public String getHyperlinkText() {
		String forId = getForId(fLastRegion);
		if (forId == null)
			return  MessageFormat.format(Messages.BrowseFor, Messages.BeanId);
		
		return MessageFormat.format(Messages.BrowseForBeanId, forId);
	}

}
