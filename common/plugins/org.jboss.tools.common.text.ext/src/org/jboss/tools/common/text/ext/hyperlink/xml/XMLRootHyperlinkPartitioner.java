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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.text.IDocument;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * @author Jeremy
 *
 */
public class XMLRootHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String XML_ROOT_PARTITION = "org.jboss.tools.common.text.ext.xml.XML_ROOT"; //$NON-NLS-1$

	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, IHyperlinkRegion superRegion) {
		if (!recognize(document, superRegion)) return null;
		
		String axis = computeAxis(document, superRegion.getOffset()) + "/"; //$NON-NLS-1$
		String contentType = superRegion.getContentType();
		String type = XML_ROOT_PARTITION;
		int length = superRegion.getLength();
		int offset = superRegion.getOffset();
		
		IHyperlinkRegion region = new HyperlinkRegion(offset, length, axis, contentType, type);
		return region;
	}

	public static String computeAxis(IDocument document, int offset) {
		String axis = ""; //$NON-NLS-1$
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			Map trackersMap = new HashMap();
			
			if (n != null) {
				if ((n instanceof Element || n instanceof Attr) &&
						n.getNodeName() != null && n.getNodeName().length() > 0) {
					String name = n.getNodeName();

					// The node name extraction must be done by taking into account 'jsfc' attribute value
					if (n instanceof Element) {
						Element e = (Element)n;
						String jsfcAttrValue = e.getAttribute("jsfc"); //$NON-NLS-1$
						if (jsfcAttrValue != null && jsfcAttrValue.trim().length() > 0) {
							name = jsfcAttrValue;
						}
					}
					
					String nodeName = extractName(name, trackersMap);
					axis = "/" + nodeName; //$NON-NLS-1$
				}
				Node parent = (n instanceof Attr)? ((Attr)n).getOwnerElement():n.getParentNode();
				while (parent instanceof Element) {
					// Get the axis part depending on the type and name of node
					String name = parent.getNodeName();

					// The node name extraction must be done by taking into account 'jsfc' attribute value
					if (parent instanceof Element) {
						Element e = (Element)parent;
						String jsfcAttrValue = e.getAttribute("jsfc"); //$NON-NLS-1$
						if (jsfcAttrValue != null && jsfcAttrValue.trim().length() > 0) {
							name = jsfcAttrValue;
						}
					}
					
					String nodeName = extractName(name, trackersMap);
					if (nodeName != null && nodeName.length() > 0) 
						axis = "/" + nodeName + axis; //$NON-NLS-1$
					parent = parent.getParentNode();
				}
			}
		} finally {
			smw.dispose();
		}
		
		if (axis == null || axis.length() == 0) axis = ""; //$NON-NLS-1$
		return axis;
	}
	
	public static String extractName (String name, Map trackersMap) {
		if (trackersMap == null || trackersMap.size() == 0) return name;
		if (name == null) return null;
		int column = name.indexOf(":"); //$NON-NLS-1$
		if (column == -1) return name;
		String prefix = name.substring(0, column);
		if (prefix == null || prefix.trim().length() == 0) return name;
		
		String uri = (String)trackersMap.get(prefix);
		if (uri == null || uri.length() == 0) return name;
		
		return "[" + uri + "]" + name.substring(column); //$NON-NLS-1$ //$NON-NLS-2$
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

			return n != null;
		} finally {
			smw.dispose();
		}
	}

}
