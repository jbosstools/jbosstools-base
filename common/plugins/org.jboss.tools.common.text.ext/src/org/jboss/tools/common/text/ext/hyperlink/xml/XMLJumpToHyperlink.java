/*******************************************************************************
 * Copyright (c) 2015 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.util.StructuredSelectionHelper;

public class XMLJumpToHyperlink extends AbstractHyperlink{
	private IRegion targetRegion;
	private String hyperlinkText;
	
	public XMLJumpToHyperlink(String hyperlinkText, IDocument document, IRegion sourceRegion, IRegion targetRegion) {
		this.hyperlinkText = hyperlinkText;
		this.targetRegion = targetRegion;
		setRegion(sourceRegion);
		setDocument(document);
	}
	
	@Override
	public String getHyperlinkText() {
		return hyperlinkText;
	}
	
	@Override
	protected void doHyperlink(IRegion region) {
		StructuredSelectionHelper.setSelectionAndRevealInActiveEditor(targetRegion);
	}
	
}
