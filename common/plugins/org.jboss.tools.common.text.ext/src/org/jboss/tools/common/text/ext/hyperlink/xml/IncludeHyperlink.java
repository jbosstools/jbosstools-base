/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.util.Utils;

public class IncludeHyperlink extends AbstractHyperlink {
	private String hyperlinkText = ""; //$NON-NLS-1$
	
	@Override
	protected void doHyperlink(IRegion region) {
		if (region == null)
			return;

		try {
			IDocument document = getDocument();
			hyperlinkText = document
					.get(region.getOffset(), region.getLength());
		} catch (BadLocationException ex) {
			ExtensionsPlugin.getPluginLog().logError(ex);
		}
		
		String fileName = Utils.trimQuotes(hyperlinkText);
		
		IFile file = getFileFromProject(fileName);
		
		if(file != null)
			openFileInEditor(file);
		else
			openFileFailed();
	}

	@Override
	public String getHyperlinkText() {
		return hyperlinkText;
	}
}
