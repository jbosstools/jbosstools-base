/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink;

import org.eclipse.jface.text.IDocument;
import org.jboss.tools.common.text.ext.hyperlink.xml.XMLClassHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.xml.XMLTextHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * @author mareshkau
 *
 */
public class TaglibDefinitionFilesHyperlinkPartitioner extends XMLTextHyperlinkPartitioner {

	public static final String TAGLIB_XML_PARTITION = "org.jboss.tools.common.text.ext.xml.TAGLIB_XML"; //$NON-NLS-1$
	/* (non-Javadoc)
	 * @see org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument, org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion)
	 */
	/**
	 * @see com.ibm.sse.editor.extensions.hyperlink.IHyperlinkPartitionRecognizer#recognize(org.eclipse.jface.text.IDocument, com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	@Override
	public boolean recognize(IDocument document, IHyperlinkRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return false;
			
			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());
			return (n instanceof Text);
		} finally {
			smw.dispose();
		}
	}

	@Override
	protected  String getPartitionType(String axis) {
		if(axis!=null && axis.contains("class")) { //$NON-NLS-1$
			return XMLClassHyperlinkPartitioner.XML_CLASS_PARTITION;
		}
		return TAGLIB_XML_PARTITION;
	}
}
