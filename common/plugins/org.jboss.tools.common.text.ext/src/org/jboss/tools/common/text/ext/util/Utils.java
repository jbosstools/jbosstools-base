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
package org.jboss.tools.common.text.ext.util;

import java.util.StringTokenizer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMNode;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Jeremy
 */
public class Utils {
	public static String trimQuotes(String word) {
		if(word == null) return null;
		String attrText = word;
		int bStart = 0;
		int bEnd = word.length() - 1;
		StringBuffer sb = new StringBuffer(attrText);

		//find start and end of path property
		while (bStart < bEnd && 
				(sb.charAt(bStart) == '\'' || sb.charAt(bStart) == '\"' ||
						Character.isWhitespace(sb.charAt(bStart)))) { 
			bStart++;
		}
		while (bEnd >= 0 && 
				(sb.charAt(bEnd) == '\'' || sb.charAt(bEnd) == '\"' ||
						Character.isWhitespace(sb.charAt(bEnd)))) { 
			bEnd--;
		}
		bEnd++;
		if (bEnd < bStart) bEnd = bStart;
		return sb.substring(bStart, bEnd);	
	}

	static public Node findNodeForOffset(Node node, int offset) {
		return (node instanceof IDOMNode) ? findNodeForOffset((IDOMNode)node, offset) : null;
	}

	static public Node findNodeForOffset(IDOMNode node, int offset) {
		if(node == null) return null;
		if (!node.contains(offset)) return null;
			
		if (node.hasChildNodes()) {
			// Try to find the node in children
			NodeList children = node.getChildNodes();
			for (int i = 0; children != null && i < children.getLength(); i++) {
				IDOMNode child = (IDOMNode)children.item(i);
				if (child.contains(offset)) {
					return findNodeForOffset(child, offset);
				}
			}
		}
			// Not found in children or nave no children
		if (node.hasAttributes()) {
			// Try to find in the node attributes
			NamedNodeMap attributes = node.getAttributes();
			
			for (int i = 0; attributes != null && i < attributes.getLength(); i++) {
				IDOMNode attr = (IDOMNode)attributes.item(i);
				if (attr.contains(offset)) {
					return attr;
				}
			}
		}
		// Return the node itself
		return node;
	}

	public static String getParentAxisForNode(Document xmlDocument, Node node) {
		String axis = ""; //$NON-NLS-1$
		if (node == null) return null;
		Node parent = (node instanceof Attr)? ((Attr)node).getOwnerElement():node.getParentNode();
		while (parent instanceof Element) {
			String nodeName = parent.getNodeName();
			axis = "/" + nodeName + axis; //$NON-NLS-1$
			parent = parent.getParentNode();
		}
		if (axis == null || axis.length() == 0) 
			axis = null;
		return axis + "/"; //$NON-NLS-1$
	}
	
	public static String trimFilePath(String name) {
		if(name == null) return null;

		StringTokenizer st = new StringTokenizer(name.replace('\\', '/'), "/"); //$NON-NLS-1$
		String result = ""; //$NON-NLS-1$
		while(st.hasMoreTokens()) {
			String token = st.nextToken();
			if (!".".equals(token)){ //$NON-NLS-1$
				result += (result.length() == 0 ? token : "/" + token); //$NON-NLS-1$
			}
		}
		if (name.startsWith("/")) //$NON-NLS-1$
			result = "/" + result; //$NON-NLS-1$
		return result;
	}

	public static IPath getRelativePath (final IPath base, final IPath path) {
		// Make web-root path to be relative to project
		IPath relativePath = new Path("/"); //$NON-NLS-1$
		String last = path.lastSegment();
		IPath absolutePath = path.removeLastSegments(1);

		while (base.isPrefixOf(absolutePath)) {
			relativePath = new Path(last).append(relativePath);
			last = absolutePath.lastSegment();
			absolutePath = absolutePath.removeLastSegments(1);
		}
		
		return relativePath;
	}
	
	public static String getTrimmedValue(IDocument document, Attr attr) throws BadLocationException {
		if(!(attr instanceof IDOMAttr)) return null;
		IDOMAttr domAttr = (IDOMAttr)attr;
		
		return Utils.trimQuotes(document.get(domAttr.getValueRegionStartOffset(), 
				domAttr.getEndOffset() - domAttr.getValueRegionStartOffset()));
	}
	
	public static String getAttributeValue (IDocument document, Node node, String attrName) {
		if(node == null || attrName == null || node.getAttributes() == null) return null;
		Attr attr = (Attr)node.getAttributes().getNamedItem(attrName);
		try {
			return Utils.getTrimmedValue(document, attr);
		} catch (BadLocationException x) {
			ExtensionsPlugin.getPluginLog().logError(x);
			return null;
		}
	}

	public static int getValueStart(Node n) {
		return (n instanceof IDOMAttr)
			? ((IDOMAttr)n).getValueRegionStartOffset() 
			: (n instanceof IndexedRegion) 
			? ((IndexedRegion)n).getStartOffset()
			: -1;
	}

	public static int getValueEnd(Node n) {
		return (n instanceof IndexedRegion)
			? ((IndexedRegion)n).getEndOffset() 
			: -1;
	}

}
