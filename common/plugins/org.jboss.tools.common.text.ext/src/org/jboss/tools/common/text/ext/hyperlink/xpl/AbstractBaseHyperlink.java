/*******************************************************************************
 * Copyright (c) 2001, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Jens Lukowski/Innoopract - initial renaming/restructuring
 *     Exadel, Inc.
 *     Red Hat, Inc.     
 *******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xpl;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.eclipse.wst.sse.core.internal.util.PathHelper;
import org.eclipse.wst.sse.core.internal.util.URIResolver;
import org.eclipse.wst.sse.ui.internal.openon.ExternalFileEditorInput;
import org.eclipse.wst.sse.ui.internal.util.PlatformStatusLineUtil;

public abstract class AbstractBaseHyperlink {

	protected final String FILE_PROTOCOL = "file:/";//$NON-NLS-1$

	public final String HTTP_PROTOCOL = "http://";//$NON-NLS-1$


	
	public AbstractBaseHyperlink() {
		super();
	}

	private ITextViewer fTextViewer; 

	private int fOffset;

	public void setTextViewer(ITextViewer textViewer) {
		fTextViewer = textViewer;
	}

	public ITextViewer getTextViewer() {
		return fTextViewer;
	}

	public void setOffset(int offset) {
		fOffset = offset;
	}

	public int getOffset() {
		return fOffset;
	}

	/**
	 * Opens the appropriate editor for fileString
	 * 
	 * @param fileString
	 */
	protected void openFileInEditor(String fileString) {
		IEditorPart editor = null;
		if (fileString != null) {
			// open web browser if this is a web address
			String temp = fileString.toLowerCase();
			if (temp.startsWith(HTTP_PROTOCOL)) {
				return;
			} else {
				// chop off the file protocol
				if (temp.startsWith(FILE_PROTOCOL)) {
					fileString = fileString.substring(FILE_PROTOCOL.length());
				}
				IFile file = getFile(fileString);
				if (file != null) {
					editor = openFileInEditor(file);
				} else {
					editor = openExternalFile(fileString);
				}
			}
		}
		if (editor == null) {
			openFileFailed();
		}
	}

	/**
	 * Returns an IFile from the given uri if possible, null if cannot find file
	 * from uri.
	 * 
	 * @param fileString
	 *            file system path
	 * @return returns IFile if fileString exists in the workspace
	 */
	protected IFile getFile(String fileString) {
		if (fileString != null) {
			IFile[] files = ResourcesPlugin.getWorkspace().getRoot()
					.findFilesForLocation(new Path(fileString));
			for (int i = 0; i < files.length; i++)
				if (files[i].exists())
					return files[i];
		}
		return null;
	}

	/**
	 * Opens the IFile, input in its default editor, if possible, and returns
	 * the editor opened. Possible reasons for failure: input cannot be found,
	 * input does not exist in workbench, editor cannot be opened.
	 * 
	 * @return IEditorPart editor opened or null if input == null or does not
	 *         exist, external editor was opened, editor could not be opened
	 */
	protected IEditorPart openFileInEditor(IFile input) {
		if (input != null && input.exists()) {
			try {
				IWorkbenchPage page = PlatformUI.getWorkbench()
						.getActiveWorkbenchWindow().getActivePage();
				return IDE.openEditor(page, input, true);
			} catch (PartInitException pie) {
				ResourcesPlugin.getPlugin().getLog().log(
						createStatus(pie));
			}
		}
		return null;
	}

	private static IStatus createStatus(Exception ex) {
		return new Status(
				IStatus.ERROR,"org.jboss.tools.common.text.ext",0,
				ex.getMessage()+"",ex);
	}
	
	/**
	 * Opens the IFile, input in its default editor, if possible, and returns
	 * the editor opened. Possible reasons for failure: input cannot be found,
	 * input does not exist in workbench, editor cannot be opened.
	 * 
	 * @return IEditorPart editor opened or null if input == null or does not
	 *         exist, external editor was opened, editor could not be opened
	 */
	protected IEditorPart openFileInEditor(IEditorInput input, String fileString) {
		if (input != null && input.exists()) {
			try {
				String editorId = getEditorId(fileString);
				IWorkbenchPage page = PlatformUI.getWorkbench()
						.getActiveWorkbenchWindow().getActivePage();
				return IDE.openEditor(page, input, editorId, true);
			} catch (PartInitException pie) {
				ResourcesPlugin.getPlugin().getLog().log(
						createStatus(pie));
			}
		}
		return null;
	}

	/**
	 * Try to open the external file, fileString in its default editor
	 * 
	 * @param fileString
	 * @return IEditorPart editor opened or null if editor could not be opened
	 */
	protected IEditorPart openExternalFile(String fileString) {
		// file does not exist in workspace so try to open using system editor
		File file = new File(fileString);
		if (!file.exists())
			return null;

		IEditorInput input = new ExternalFileEditorInput(file);
		String editorId = getEditorId(fileString);

		try {
			IWorkbenchPage page = PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow().getActivePage();
			return page.openEditor(input, editorId, true);
		} catch (PartInitException pie) {
			ResourcesPlugin.getPlugin().getLog().log(
					createStatus(pie));
		}
		return null;
	}

	/**
	 * Determines the editor associated with the given file name
	 * 
	 * @param filename
	 * @return editor id of the editor associated with the given file name
	 */
	protected String getEditorId(String filename) {
		IWorkbench workbench = PlatformUI.getWorkbench();
		IEditorRegistry editorRegistry = workbench.getEditorRegistry();
		IEditorDescriptor descriptor = editorRegistry
				.getDefaultEditor(filename);
		if (descriptor != null)
			return descriptor.getId();
		return EditorsUI.DEFAULT_TEXT_EDITOR_ID;
	}

	/**
	 * Notifies user that open on selection action could not successfully open
	 * the selection (writes message on status bar and beeps)
	 */
	protected void openFileFailed() {
		PlatformStatusLineUtil.displayErrorMessage(Messages.cannotOpenLink);
		PlatformStatusLineUtil.addOneTimeClearListener();
	}

	/**
	 * @deprecated this method has moved up to DefaultHyperlinkHTML - TODO
	 *             remove in C5
	 */
	protected String resolveURI(String uriString) {
		String resolvedURI = uriString;

		if (uriString != null) {
			IStructuredModel sModel = null;
			try {
				sModel = getModelManager()
					.getExistingModelForRead(getDocument());
				if (sModel != null) {
					URIResolver resolver = sModel.getResolver();
					resolvedURI = resolver != null ? resolver.getLocationByURI(
							uriString, true) : uriString;
				}
			} finally {
				if (sModel != null) sModel.releaseFromRead();
			}
			// special adjustment for file protocol
			if (uriString.startsWith(FILE_PROTOCOL)) {
				PathHelper.removeLeadingSeparator(resolvedURI);
			}
		}
		return resolvedURI;
	}

	protected IModelManager getModelManager() {
		return StructuredModelManager.getModelManager();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getHyperlinkRegion()
	 */
	public IRegion getHyperlinkRegion() {
		IRegion region = doGetHyperlinkRegion(getOffset());
		return (region != null) ? region : new Region(getOffset(), 0);
	}

	abstract protected IRegion doGetHyperlinkRegion(int offset);

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#open()
	 */
	public void open() {
		IRegion region = getHyperlinkRegion();
		// if no region was given this action fails
		if (getDocument() == null || region == null)
			openFileFailed();
		else
			doHyperlink(region);
	}

	abstract protected void doHyperlink(IRegion region);

	/**
	 * Returns the current document associated with hyperlink
	 * 
	 * @return IDocument
	 */
	public IDocument getDocument() {
		return (fTextViewer == null ? null : fTextViewer.getDocument());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getHyperlinkText()
	 */
	public String getHyperlinkText() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getTypeLabel()
	 */
	public String getTypeLabel() {
		return null;
	}
	
	
}