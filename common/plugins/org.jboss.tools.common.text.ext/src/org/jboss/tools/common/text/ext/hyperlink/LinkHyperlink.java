/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;

/**
 * @author Jeremy
 */
public abstract class LinkHyperlink extends AbstractHyperlink {

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
			
			String fileName = getFilePath(region);
			IFile fileToOpen = getFileFromProject(fileName);
			if (fileToOpen != null && fileToOpen.exists()) {
				IWorkbenchPage workbenchPage = ExtensionsPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
				try {
					IDE.openEditor(workbenchPage,fileToOpen,true);
				} catch (CoreException x) {
					// could not open editor
					openFileFailed();
				}
				return;
			} 
			try {
				URL url = new URL(fileName);
				IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
				IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
				browser.openURL(url);

			} catch (MalformedURLException e) {
				openFileFailed();
			} catch (PartInitException e) {
				openFileFailed();
			}
	}
	
	protected String getFilePath(IRegion region) {
		try {
			return getDocument().get(region.getOffset(), region.getLength());
		} catch (BadLocationException x) {
			//ignore
			return null;
		} 
	}
	
	protected String updateFilenameForModel(String filename, IProject project) {
		return filename;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getHyperlinkText()
	 */
	public String getHyperlinkText() {
		String filePath = getFilePath(getHyperlinkRegion());
		if (filePath == null)
			return  MessageFormat.format(Messages.OpenA, Messages.File);
		
		return MessageFormat.format(Messages.OpenFile, filePath);
	}

}
