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
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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

/**
 * @author Jeremy
 */
public class JSPRootHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String JSP_ROOT_PARTITION = "org.jboss.tools.common.text.ext.jsp.JSP_ROOT";
	
	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, IHyperlinkRegion superRegion) {
		if (!recognize(document, superRegion)) return null;
		
		String axis = computeAxis(document, superRegion.getOffset()) + "/";
		String contentType = superRegion.getContentType();
		String type = JSP_ROOT_PARTITION;
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
			Map trackersMap = getTrackersMap(document, offset);
			
			IFile file = smw.getFile();
			XModel xModel = AbstractHyperlink.getXModel(file);
			ITaglibMapping tm = xModel == null ? null : WebProjectFactory.instance.getWebProject(xModel).getTaglibMapping();

			if (n != null) {
				if ((n instanceof Element || n instanceof Attr) &&
						n.getNodeName() != null && n.getNodeName().length() > 0) {
					String nodeName = extractName(n.getNodeName(), trackersMap, tm);
					axis = "/" + nodeName;
				}
				Node parent = (n instanceof Attr)? ((Attr)n).getOwnerElement() : n.getParentNode();
				while (parent != null && (parent instanceof Element)) {
					// Get the axis part depending on the type and name of node
					String nodeName = extractName(parent.getNodeName(), trackersMap, tm);
					if (nodeName != null && nodeName.length() > 0) 
						axis = "/" + nodeName + axis;
					parent = parent.getParentNode();
				}
			}
		} catch (Exception x) {
			//ignore
		} finally {
			smw.dispose();
		}
		
		if (axis == null || axis.length() == 0) axis = "";
		return axis;
	}
	
	public static String extractName (String name, Map trackersMap, ITaglibMapping tm) {
		if (trackersMap == null || trackersMap.size() == 0) return name;
		if (name == null) return null;
		try {
			int column = name.indexOf(":");
			if (column == -1) return name;
			String prefix = name.substring(0, column);
			if (prefix == null || prefix.trim().length() == 0) return name;
			
			String uri = (String)trackersMap.get(prefix);
			if (uri == null || uri.length() == 0) return name;
			
			String resolvedUri = (tm == null ? uri : tm.resolveURI(uri));
			
			return "[" + resolvedUri + "]" + name.substring(column);
		} catch (Exception x) {
			//ignore
			return name;
		}
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
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	private static final String JSP_SOURCE_CONTENTTYPE_ID = "org.eclipse.jst.jsp.core.jspsource";
	private static final String JSP_SOURCE_ROOT_ELEMENT = "jsp:root";
	private static final String HTML_SOURCE_CONTENTTYPE_ID = "org.eclipse.wst.html.core.htmlsource";
	private static final String HTML_SOURCE_ROOT_ELEMENT = "html";
	
	// @TODO: alternative trackers map (for use along with xhtml documents)
	public static Map<String,String> addAlternativeTrackersMap(Map<String,String> map, IDocument document, int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			String cti = smw.getContentTypeIdentifier();
			NodeList roots = null;
			if (JSP_SOURCE_CONTENTTYPE_ID.equalsIgnoreCase(cti)) 
				roots = xmlDocument.getElementsByTagName(JSP_SOURCE_ROOT_ELEMENT);
			else if (HTML_SOURCE_CONTENTTYPE_ID.equalsIgnoreCase(cti)) 
				roots = xmlDocument.getElementsByTagName(HTML_SOURCE_ROOT_ELEMENT);
			
			if (roots == null)
					return map;
			
			if (map == null) map = new HashMap<String,String>();
			
			for (int i = 0; roots != null && i < roots.getLength(); i++) {
				Node n = roots.item(i);
				NamedNodeMap attrs = n.getAttributes();
				for (int j = 0; attrs != null && j < attrs.getLength(); j++) {
					Attr a = (Attr)attrs.item(j);
					String name = a.getName();
					if (name.startsWith("xmlns:")) {
						String uri = a.getValue();
						String prefix = name.substring("xmlns:".length());
						map.put(prefix.trim(), uri.trim());
					}
				}
			}
			
			return map;
		} catch (Exception x) {
			//ignore
			return null;
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
		} catch (Exception x) {
			//ignore
			return false;
		} finally {
			smw.dispose();
		}
	}

}
