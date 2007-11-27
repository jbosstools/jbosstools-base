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

import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.xml.XMLClassHyperlinkPartitioner;

/**
 * @author Jeremy
 */
public class JSPClassHyperlinkPartitioner extends XMLClassHyperlinkPartitioner {
	protected String getAxis(IDocument document, IHyperlinkRegion superRegion) {
		if (superRegion.getAxis() == null || superRegion.getAxis().length() == 0) {
			return JSPRootHyperlinkPartitioner.computeAxis(document, superRegion.getOffset()) + "/";
		}
		return superRegion.getAxis();
	}
}
