/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.jface.text.IDocument;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

public class IncludeHyperlinkPartitioner extends AbstractHyperlinkPartitioner {
	public static final String INCLUDE_FILE_PARTITION = "org.jboss.tools.common.text.ext.hyperlink.xml.INCLUDE_FILE"; //$NON-NLS-1$
	public static final String URL_NAME="url";
	
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

	@Override
	protected IHyperlinkRegion parse(IDocument document, int offset, 
			IHyperlinkRegion superRegion) {
		
		Node node = getNode(document, offset);
		
		if(!URL_NAME.equals(node.getNodeName()))
			return null;
		
		int start = Utils.getValueStart(node);
		if(start < 0) return null;
		int end = Utils.getValueEnd(node);

		String contentType = superRegion.getContentType();
		String axis = getAxis(document, offset);

		IHyperlinkRegion hyperRegion = new HyperlinkRegion(start, end-start,
				axis, contentType, INCLUDE_FILE_PARTITION);
		return hyperRegion;
	}
}
