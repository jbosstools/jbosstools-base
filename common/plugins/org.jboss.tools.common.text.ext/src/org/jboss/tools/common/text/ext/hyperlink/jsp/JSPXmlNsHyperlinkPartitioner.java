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
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

/**
 * @author Jeremy
 */
public class JSPXmlNsHyperlinkPartitioner extends AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {
	public static final String JSP_XMLNS_PARTITION = "org.jboss.tools.common.text.ext.jsp.JSP_XMLNS";

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
			if (!(n instanceof IDOMAttr)) return null; 
			IDOMAttr xmlnsAttr = (IDOMAttr)n;
			if (xmlnsAttr.getName() == null || !xmlnsAttr.getName().startsWith("xmlns:")) return null;
			Element rootElem = xmlnsAttr.getOwnerElement();
			if (!rootElem.getNodeName().equals("jsp:root")) return null;
			String xmlns = xmlnsAttr.getValueRegionText();
			String axis = getAxis(document, superRegion);
			String contentType = superRegion.getContentType();
			String type = JSP_XMLNS_PARTITION;
			int length = xmlns.length() - (superRegion.getOffset() - xmlnsAttr.getValueRegionStartOffset());
			int offset = superRegion.getOffset();

			IHyperlinkRegion region = new HyperlinkRegion(offset, length, axis, contentType, type);
			return region;
		} catch (Exception x) {
			ExtensionsPlugin.log("Error in parsing region", x);
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
			if (!(n instanceof Attr)) return false; 
			Attr xmlnsAttr = (Attr)n;
			if (xmlnsAttr.getName() == null || !xmlnsAttr.getName().startsWith("xmlns:")) return false;
			Element rootElem = xmlnsAttr.getOwnerElement();
			if (!rootElem.getNodeName().equals("jsp:root")) return false;
			return true;
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
