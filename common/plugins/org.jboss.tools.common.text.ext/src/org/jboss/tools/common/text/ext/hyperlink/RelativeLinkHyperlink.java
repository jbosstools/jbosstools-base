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

import java.text.MessageFormat;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;

/**
 * @author Jeremy
 */
public abstract class RelativeLinkHyperlink extends AbstractHyperlink {

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
		try {
			String fileName = getFilePath(region);
			IFile fileToOpen = getFileFromProject(fileName);
			if ( fileToOpen!= null && fileToOpen.exists()) {
				IWorkbenchPage workbenchPage = ExtensionsPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
				IDE.openEditor(workbenchPage,fileToOpen,true);
			} else {
				openFileFailed();
			}
		} catch (CoreException x) {
			// could not open editor
			openFileFailed();
		}
	}
	
	private String getFilePath(IRegion region) {
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
	
	protected IFile getFileFromProject(String fileName) {
		if (fileName == null || fileName.trim().length() == 0)
			return null;
		
		IFile documentFile = getFile();
		IProject project = documentFile.getProject();
		return super.getFileFromProject(updateFilenameForModel(fileName, project));
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