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

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

/**
 * @author Jeremy
 */
public class XMLJumpToHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String XML_JUMP_TO_PARTITION = "org.jboss.tools.common.text.xml.XML_JUMP_TO";

	protected String getPartitionType() {
		return XML_JUMP_TO_PARTITION;
	}
	
	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, IHyperlinkRegion superRegion) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Utils.findNodeForOffset(xmlDocument, superRegion.getOffset());
			if (!recognize(document, superRegion)) return null;
			IRegion r = getRegion(document, superRegion.getOffset());
			if (r == null) return null;
			
			String axis = getAxis(document, superRegion);
			String contentType = superRegion.getContentType();
			String type = getPartitionType();
			int length = r.getLength() - (superRegion.getOffset() - r.getOffset());
			int offset = superRegion.getOffset();
			
			IHyperlinkRegion region = new HyperlinkRegion(offset, length, axis, contentType, type);
			return region;
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	protected IRegion getRegion(IDocument document, final int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n == null || !(n instanceof Text)) return null;
			
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);
			
			if (start < 0 || start > offset) return null;

			String attrText = document.get(start, end - start);
			StringBuffer sb = new StringBuffer(attrText);

			//find start and end of path property
			int bStart = 0;
			int bEnd = attrText.length() - 1;

			while (bStart < bEnd && 
					(sb.charAt(bStart) == '\'' || sb.charAt(bStart) == '\"' ||
							Character.isWhitespace(sb.charAt(bStart)))) { 
				bStart++;
			}
			while (bEnd > bStart && 
					(sb.charAt(bEnd) == '\'' || sb.charAt(bEnd) == '\"' ||
							Character.isWhitespace(sb.charAt(bEnd)))) { 
				bEnd--;
			}
			bEnd++;

			final int propStart = bStart + start;
			final int propLength = bEnd - bStart;
			
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

	protected String[] getValidAxisEndings() {
		return null;
	}
	
	private boolean validAxis(Node n, String validAxisEnding) {
		try {
			if (validAxisEnding == null || validAxisEnding.lastIndexOf('/') == -1) return false;
			StringTokenizer st = new StringTokenizer(validAxisEnding, "/");
			List<String> tokens = new ArrayList<String>();
			while (st.hasMoreTokens()) {
				tokens.add(st.nextToken());
			}
			if (tokens.size() == 0) return false;
			Node currentElement = n;
			for (int i = tokens.size() - 1; i >= 0; i--) {
				if (currentElement == null || !(currentElement instanceof Element))
					return false;
				String token = (String)tokens.get(i);
				if (!token.equals(currentElement.getNodeName()))
					return false;
				currentElement = currentElement.getParentNode();
			}
			return true;

		} catch (Exception x) {
			//ignore
			return false;
		}
	}

	/**
	 * @see com.ibm.sse.editor.extensions.hyperlink.IHyperlinkPartitionRecognizer#recognize(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	public boolean recognize(IDocument document, IHyperlinkRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return false;
			
			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());

			if (!(n instanceof Text)) return false;
	
			String[] validAxisEndings = getValidAxisEndings();
			if (validAxisEndings == null || validAxisEndings.length == 0) return false;
			for (int i = 0; validAxisEndings != null && i < validAxisEndings.length; i++) {
				if (validAxis(n.getParentNode(), validAxisEndings[i])) 
					return true;
			}
			return false;
		} catch (Exception x) {
			//ignore
			return false;
		} finally {
			smw.dispose();
		}
	}

}