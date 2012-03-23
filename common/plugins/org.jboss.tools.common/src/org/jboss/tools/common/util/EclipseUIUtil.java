/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.util;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.jboss.tools.common.CommonPlugin;

/**
 * @author Alexey Kazakov
 */
public class EclipseUIUtil {

	private static class SafeRunnableForActivePage extends SafeRunnable {

		public ITextEditor activeEditor;

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.core.runtime.ISafeRunnable#run()
		 */
		@Override
		public void run() throws Exception {
			IWorkbenchWindow window = PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow();
			if (window != null) {
				IWorkbenchPage page = window.getActivePage();
				if (page != null) {
					IEditorPart editor = page.getActiveEditor();
					if (editor instanceof IEditorWrapper) {
						editor = ((IEditorWrapper) editor).getEditor();
					}
					if (editor instanceof ITextEditor) {
						activeEditor = (ITextEditor) editor;
					} else {
						activeEditor = editor == null ? null : (ITextEditor)editor.getAdapter(ITextEditor.class);
					}
				}
			}
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jface.util.SafeRunnable#handleException(java.lang.Throwable)
		 */
		@Override
		public void handleException(Throwable e) {
			CommonPlugin.getDefault().logError(e);
		}
	}

	/**
	 * Returns the active text editor.
	 * @return
	 */
	public static ITextEditor getActiveEditor() {
		SafeRunnableForActivePage sr = new SafeRunnableForActivePage();
		SafeRunnable.run(sr);
		return sr.activeEditor;
	}

	/**
	 * Returns true if the file is open in active editor.
	 * 
	 * @param file
	 * @return
	 */
	public static boolean isOpenInActiveEditor(IFile file) {
		if(file == null)
			return false;
		
		ITextEditor editor = EclipseUIUtil.getActiveEditor();
		if (editor != null) {
			IEditorInput editorInput = editor.getEditorInput();
			if (editorInput instanceof IFileEditorInput) {
				IFileEditorInput fileInput = (IFileEditorInput) editorInput;
				return file.equals(fileInput.getFile());
			}
		}
		return false;
	}

	/**
	 * Returns all the modified but not saved files which are opened with all the editors.
	 * @return
	 */
	public static Set<IFile> getDirtyFiles() {
		final Set<IFile> dirtyFiles = new HashSet<IFile>();
		IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
		for (IWorkbenchWindow window : windows) {
			final IWorkbenchPage page = window.getActivePage();
			if (page != null) {
				// If this method is invoked in non-UI thread then some editors may throw Invalid Thread Access exception.
				// We use SafeRunnable as a workaround to avoid crashing. 
				// See https://issues.jboss.org/browse/JBIDE-11385
				SafeRunnable sr = new SafeRunnable() {
					@Override
					public void run() throws Exception {
						IEditorPart[] editors = page.getDirtyEditors();
						for (IEditorPart editor : editors) {
							IEditorInput input = editor.getEditorInput();
							if(input instanceof IFileEditorInput) {
								IFile file = ((IFileEditorInput)input).getFile();
								dirtyFiles.add(file);
							}
						}
					}

					/* (non-Javadoc)
					 * @see org.eclipse.jface.util.SafeRunnable#handleException(java.lang.Throwable)
					 */
					@Override
					public void handleException(Throwable e) {
						CommonPlugin.getDefault().logError(e);
					}
				};
				SafeRunnable.run(sr);
			}
		}
		return dirtyFiles;
	}
}