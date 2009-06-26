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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jst.jsp.core.internal.contentmodel.TaglibController;
import org.eclipse.jst.jsp.core.internal.contentmodel.tld.TLDCMDocumentManager;
import org.eclipse.jst.jsp.core.internal.contentmodel.tld.TaglibTracker;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.jboss.tools.jst.web.tld.ITaglibMapping;
import org.jboss.tools.jst.web.tld.WebProjectFactory;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * @author Jeremy
 */
public class JSPRootHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String JSP_ROOT_PARTITION = "org.jboss.tools.common.text.ext.jsp.JSP_ROOT"; //$NON-NLS-1$
	
	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, IHyperlinkRegion superRegion) {
		if (!recognize(document, superRegion)) return null;
		
		String axis = computeAxis(document, superRegion.getOffset()) + "/"; //$NON-NLS-1$
		String contentType = superRegion.getContentType();
		String type = JSP_ROOT_PARTITION;
		int length = superRegion.getLength();
		int offset = superRegion.getOffset();
		
		IHyperlinkRegion region = new HyperlinkRegion(offset, length, axis, contentType, type);
		return region;
	}

	public static String computeAxis(IDocument document, int offset) {
		String axis = ""; //$NON-NLS-1$
		StructuredModelWrapper smw = new StructuredModelWrapper();
		smw.init(document);
		try {
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			Map trackersMap = getTrackersMap(document, offset);
			
			IFile file = smw.getFile();
			XModel xModel = AbstractHyperlink.getXModel(file);
			ITaglibMapping tm = xModel == null ? null : WebProjectFactory.instance.getWebProject(xModel).getTaglibMapping();

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
					
					String nodeName = extractName(name, trackersMap, tm);
					axis = "/" + nodeName; //$NON-NLS-1$
				}
				Node parent = (n instanceof Attr)? ((Attr)n).getOwnerElement() : n.getParentNode();
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
					
					String nodeName = extractName(name, trackersMap, tm);
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
	
	public static String extractName (String name, Map trackersMap, ITaglibMapping tm) {
		if (trackersMap == null || trackersMap.size() == 0) return name;
		if (name == null) return null;
			int column = name.indexOf(":"); //$NON-NLS-1$
			if (column == -1) return name;
			String prefix = name.substring(0, column);
			if (prefix == null || prefix.trim().length() == 0) return name;
			
			String uri = (String)trackersMap.get(prefix);
			if (uri == null || uri.length() == 0) return name;
			
			String resolvedUri = (tm == null ? uri : tm.resolveURI(uri));
			
			return "[" + resolvedUri + "]" + name.substring(column); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static Map getTrackersMap(IDocument document, int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			TLDCMDocumentManager manager = TaglibController.getTLDCMDocumentManager(document);
			
			List trackers = (manager == null? null : manager.getCMDocumentTrackers(offset));
			
			Map<String,String> map = new HashMap<String,String>();
			// Find F tracker
//			TaglibTracker fTT = null;
			for (int i = 0; trackers != null && i < trackers.size(); i++) {
				TaglibTracker tt = (TaglibTracker)trackers.get(i);
				String prefix = tt.getPrefix();
				String uri = tt.getURI();
				if (prefix != null && prefix.trim().length() > 0 &&
						uri != null && uri.trim().length() > 0) {
					map.put(prefix.trim(), uri.trim());
				}
			}
			map = addAlternativeTrackersMap(map, document, offset);

			return map;
		} finally {
			smw.dispose();
		}
	}

	// DONE: alternative trackers map (for use along with xhtml documents)
	public static Map<String,String> addAlternativeTrackersMap(Map<String,String> map, IDocument document, int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			if (map == null) map = new HashMap<String,String>();

			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			while (n != null) {
				if (!(n instanceof Element)) {
					if (n instanceof Attr) {
						n = ((Attr)n).getOwnerElement();
					} else {
						n = n.getParentNode();
					}
					continue;
				}
				
				NamedNodeMap attrs = n.getAttributes();
				for (int j = 0; attrs != null && j < attrs.getLength(); j++) {
					Attr a = (Attr)attrs.item(j);
					String name = a.getName();
					if (name.startsWith("xmlns:")) { //$NON-NLS-1$
						String uri = a.getValue();
						String prefix = name.substring("xmlns:".length()); //$NON-NLS-1$
						map.put(prefix.trim(), uri.trim());
					}
				}
				
				n = n.getParentNode();
			}

			return map;
		} finally {
			smw.dispose();
		}
	}

	/* (non-Javadoc)
	 * @see com.ibm.sse.editor.extensions.hyperlink.IHyperlinkPartitionRecognizer#recognize(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	public boolean recognize(IDocument document, IHyperlinkRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return false;
			
			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());

			if (n == null) return false;
			
			return true;
		} finally {
			smw.dispose();
		}
	}

}
