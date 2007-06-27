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

import org.eclipse.jface.text.IDocument;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

/**
 * @author Jeremy
 */
public class JSPTagAttributeHyperlinkPartitioner extends JSPElementAttributeValueHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String JSP_TAG_ATTRIBUTE_PARTITION = "org.jboss.tools.common.text.ext.jsp.JSP_TAG_ATTRIBUTE";
	
	private static final String JSP_ATTRIBUTE_TAGNAME = "jsp:attribute";
	private static final String JSP_ELEMENT_TAGNAME = "jsp:element";

	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document, IHyperlinkRegion superRegion) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, superRegion.getOffset());
			if (n instanceof Attr) n = ((Attr)n).getOwnerElement();
			if (!(n instanceof Element)) return null;

			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);
			if(start < 0) return null;

			String axis = getAxis(document, superRegion);
			String contentType = superRegion.getContentType();
			String type = JSP_TAG_ATTRIBUTE_PARTITION;
			int length = end - start - (superRegion.getOffset() - start);
			int offset = superRegion.getOffset();

			IHyperlinkRegion region = new HyperlinkRegion(offset, length, axis, contentType, type);
			return region;
		} catch (Exception x) {
			ExtensionsPlugin.getPluginLog().logError("Error in parsing region", x);
			return null;
		} finally {
			smw.dispose();
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
			if (n instanceof Attr) n = ((Attr)n).getOwnerElement();
			if (!(n instanceof Element)) return false;
			return (JSP_ATTRIBUTE_TAGNAME.equals(n.getNodeName()) && 
					!JSP_ELEMENT_TAGNAME.equals(n.getParentNode().getNodeName()) && 
					n.getNodeName().indexOf(':') != -1);
		} catch (Exception x) {
			//ignore
			return false;
		} finally {
			smw.dispose();
		}
	}

	protected String getAxis(IDocument document, IHyperlinkRegion superRegion) {
		if (superRegion.getAxis() == null || superRegion.getAxis().length() == 0) {
			return JSPRootHyperlinkPartitioner.computeAxis(document, superRegion.getOffset()) + "/";
		}
		return superRegion.getAxis();
	}

}
