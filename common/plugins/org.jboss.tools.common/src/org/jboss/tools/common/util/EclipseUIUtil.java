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

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * @author Alexey Kazakov
 */
public class EclipseUIUtil {

	/**
	 * Returns the active text editor.
	 * 
	 * @return
	 */
	public static ITextEditor getActiveEditor() {
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
					return (ITextEditor) editor;
				} else {
					return (ITextEditor) editor.getAdapter(ITextEditor.class);
				}
			}
		}
		return null;
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
}