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
package org.jboss.tools.common.text.ext.hyperlink;

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;

public class OpenJavaElementHyperlink  extends AbstractHyperlink{
	private IJavaElement javaElement;
	private String hyperlinkText;
	
	public OpenJavaElementHyperlink(String hyperlinkText, IDocument document, IRegion sourceRegion, IJavaElement javaElement) {
		this.hyperlinkText = hyperlinkText;
		this.javaElement = javaElement;
		setRegion(sourceRegion);
		setDocument(document);
	}
	
	@Override
	public String getHyperlinkText() {
		return hyperlinkText;
	}
	
	@Override
	protected void doHyperlink(IRegion region) {
		IEditorPart part = null;
		
		try{
			part = JavaUI.openInEditor(javaElement);
		}catch(JavaModelException ex){
			ExtensionsPlugin.getDefault().logError(ex);
		}catch(PartInitException ex){
			ExtensionsPlugin.getDefault().logError(ex);
		}
		
		if (part == null){
			openFileFailed();
		}
	}
	
}
