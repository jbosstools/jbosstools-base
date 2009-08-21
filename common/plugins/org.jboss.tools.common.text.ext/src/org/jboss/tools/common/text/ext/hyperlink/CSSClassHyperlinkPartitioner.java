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
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkPartitionRecognizer;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * 
 * @author Sergey Dzmitrovich
 *
 */
public class CSSClassHyperlinkPartitioner extends
		AbstractHyperlinkPartitioner implements IHyperlinkPartitionRecognizer {

	public static final String CSS_CLASS_PARTITION = "org.jboss.tools.common.text.ext.CSS_CLASS"; //$NON-NLS-1$

	private static final String CSS_CLASS_TOKEN = "class/"; //$NON-NLS-1$

	/**
	 * @see com.ibm.sse.editor.hyperlink.AbstractHyperlinkPartitioner#parse(org.eclipse.jface.text.IDocument,
	 *      com.ibm.sse.editor.extensions.hyperlink.IHyperlinkRegion)
	 */
	protected IHyperlinkRegion parse(IDocument document,
			IHyperlinkRegion superRegion) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null)
				return null;

			Node n = Utils.findNodeForOffset(xmlDocument, superRegion
					.getOffset());
			if (n == null || !(n instanceof Attr))
				return null;

			String axis = getAxis(document, superRegion);
			String contentType = superRegion.getContentType();
			String type = getPartitionType(axis);
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);
			if (start < 0 || end < start)
				return null;
			int length = end - start;
			int offset = superRegion.getOffset();

			IHyperlinkRegion region = new HyperlinkRegion(offset, length, axis,
					contentType, type);
			return region;
		} finally {
			smw.dispose();
		}
	}

	private String getPartitionType(String axis) {
		return CSS_CLASS_PARTITION;
	}

	public boolean recognize(IDocument document, IHyperlinkRegion region) {

		if (region.getAxis() != null
				&& region.getAxis().toLowerCase().endsWith(CSS_CLASS_TOKEN))
			return true;
		return false;
	}

}
