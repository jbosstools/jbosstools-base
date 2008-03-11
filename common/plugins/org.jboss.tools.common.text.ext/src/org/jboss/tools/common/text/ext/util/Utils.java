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
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMText;
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
		try {
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
		} catch (Exception x) {
			return word;
		}		
	}

	static public Node findNodeForOffset(Node node, int offset) {
		return (node instanceof IDOMNode) ? findNodeForOffset((IDOMNode)node, offset) : null;
	}

	static public Node findNodeForOffset(IDOMNode node, int offset) {
		Node result = null;
		try {
			if (!node.contains(offset)) return null;
			
			if (node.hasChildNodes()) {
				// Try to find the node in children
				NodeList children = node.getChildNodes();
				for (int i = 0; children != null && i < children.getLength(); i++) {
					IDOMNode child = (IDOMNode)children.item(i);
					if (child.contains(offset)) {
						result = findNodeForOffset(child, offset);
						return result;
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
						result = attr;
						return attr;
					}
				}
			}
			// Return the node itself
			result = node;
			return node;
		} catch (Exception x) {
			ExtensionsPlugin.getPluginLog().logError("Error while finding node for offset", x);
			return null;
		}
	}

	public static String getParentAxisForNode(Document xmlDocument, Node node) {
		String axis = "";
		if (node == null) return null;
		Node parent = (node instanceof Attr)? ((Attr)node).getOwnerElement():node.getParentNode();
		while (parent != null && (parent instanceof Element)) {
			String nodeName = parent.getNodeName();
			axis = "/" + nodeName + axis;
			parent = parent.getParentNode();
		}
		if (axis == null || axis.length() == 0) 
			axis = null;
		return axis + "/";
	}
	
	public static String trimFilePath(String name) {
		try {
			StringTokenizer st = new StringTokenizer(name.replace('\\', '/'), "/");
			String result = "";
			while(st.hasMoreTokens()) {
				String token = st.nextToken();
				if (!".".equals(token)){
					result += (result.length() == 0 ? token : "/" + token);
				}
			}
			if (name.startsWith("/"))
				result = "/" + result;
			return result;
		} catch (Exception x) {
			//ignore
		}
		return name;
	}

	public static IPath getRelativePath (final IPath base, final IPath path) {
		// Make web-root path to be relative to project
		IPath relativePath = new Path("/");
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
