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

		public ITextEditor activeTextEditor;
		public IEditorPart activeEditor;

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
					activeEditor = page.getActiveEditor();
					if (activeEditor instanceof IEditorWrapper) {
						activeEditor = ((IEditorWrapper) activeEditor).getEditor();
					}
					if (activeEditor instanceof ITextEditor) {
						activeTextEditor = (ITextEditor) activeEditor;
					} else {
						activeTextEditor = activeEditor == null ? null : (ITextEditor)activeEditor.getAdapter(ITextEditor.class);
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

	public static boolean isActiveEditorDirty() {
		SafeRunnableForActivePage sr = new SafeRunnableForActivePage();
		SafeRunnable.run(sr);
		IEditorPart editor = sr.activeEditor;
		if(editor!=null) {
			return editor.isDirty();
		}
		return false;
	}

	/**
	 * Returns the active text editor.
	 * @return
	 */
	public static ITextEditor getActiveEditor() {
		SafeRunnableForActivePage sr = new SafeRunnableForActivePage();
		SafeRunnable.run(sr);
		return sr.activeTextEditor;
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
		return DirtyEditorTracker.getInstance().getDirtyFiles();
	}
}