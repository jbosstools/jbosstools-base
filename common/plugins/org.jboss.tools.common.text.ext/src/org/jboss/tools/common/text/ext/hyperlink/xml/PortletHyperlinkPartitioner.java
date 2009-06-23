/*******************************************************************************
 * Copyright (c) 2009 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.jface.text.IDocument;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

public class PortletHyperlinkPartitioner extends AbstractHyperlinkPartitioner {
	public static final String PORTLET_CLASS_PARTITION = "org.jboss.tools.common.text.ext.hyperlink.xml.PORTLET_CLASS";
	public static final String PORTLET_RESOURCE_BUNDLE_PARTITION = "org.jboss.tools.common.text.ext.hyperlink.xml.PORTLET_RESOURCE";

	static final String textNodeName = "#text";

	static final String portletNodeName = "portlet";
	static final String portletClassNodeName = "portlet-class";
	static final String portletResourceBundleNodeName = "resource-bundle";

	public static Node getNode(IDocument document, int superOffset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null)
				return null;

			Node node = Utils.findNodeForOffset(xmlDocument, superOffset); // #text

			return node;
		} finally {
			smw.dispose();
		}
	}

	public static String getType(Node node) {
		Node parentNode = node.getParentNode(); // parent node
		if (parentNode == null)
			return null;

		Node portletNode = parentNode.getParentNode(); // portlet node
		if (parentNode == null)
			return null;

		if (node.getNodeName().equalsIgnoreCase(textNodeName)
				&& portletNode.getNodeName().equalsIgnoreCase(portletNodeName)) {
			if (parentNode.getNodeName().equalsIgnoreCase(portletClassNodeName)) {
				return PORTLET_CLASS_PARTITION;
			} else if (parentNode.getNodeName().equalsIgnoreCase(
					portletResourceBundleNodeName)) {
				return PORTLET_RESOURCE_BUNDLE_PARTITION;
			}
		}
		return null;
	}

	@Override
	protected IHyperlinkRegion parse(IDocument document,
			IHyperlinkRegion superRegion) {
		Node node = getNode(document, superRegion.getOffset());
		String type = getType(node);
		if (type == null)
			return null;

		IndexedRegion text = (IndexedRegion) node;

		int length = text.getLength();
		int offset = text.getStartOffset();

		String contentType = superRegion.getContentType();
		String axis = getAxis(document, superRegion);

		IHyperlinkRegion hyperRegion = new HyperlinkRegion(offset, length,
				axis, contentType, type);
		return hyperRegion;
	}

}
