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
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

/**
 * @author Jeremy
 *
 */
public class XMLRootHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String XML_ROOT_PARTITION = "org.jboss.tools.common.text.ext.xml.XML_ROOT";

	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, IHyperlinkRegion superRegion) {
		if (!recognize(document, superRegion)) return null;
		
		String axis = computeAxis(document, superRegion.getOffset()) + "/";
		String contentType = superRegion.getContentType();
		String type = XML_ROOT_PARTITION;
		int length = superRegion.getLength();
		int offset = superRegion.getOffset();
		
		IHyperlinkRegion region = new HyperlinkRegion(offset, length, axis, contentType, type);
		return region;
	}

	public static String computeAxis(IDocument document, int offset) {
		String axis = "";
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
					String nodeName = extractName(n.getNodeName(), trackersMap);
					axis = "/" + nodeName;
				}
				Node parent = (n instanceof Attr)? ((Attr)n).getOwnerElement():n.getParentNode();
				while (parent != null && (parent instanceof Element)) {
					// Get the axis part depending on the type and name of node
					String nodeName = extractName(parent.getNodeName(), trackersMap);
					if (nodeName != null && nodeName.length() > 0) 
						axis = "/" + nodeName + axis;
					parent = parent.getParentNode();
				}
			}
		} catch (Exception x) {
		} finally {
			smw.dispose();
		}
		
		if (axis == null || axis.length() == 0) axis = "";
		return axis;
	}
	
	public static String extractName (String name, Map trackersMap) {
		if (trackersMap == null || trackersMap.size() == 0) return name;
		if (name == null) return null;
		try {
			int column = name.indexOf(":");
			if (column == -1) return name;
			String prefix = name.substring(0, column);
			if (prefix == null || prefix.trim().length() == 0) return name;
			
			String uri = (String)trackersMap.get(prefix);
			if (uri == null || uri.length() == 0) return name;
			
			return "[" + uri + "]" + name.substring(column);
		} catch (Exception x) {
			return name;
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

			return n != null;
		} catch (Exception x) {
			return false;
		} finally {
			smw.dispose();
		}
	}

}
