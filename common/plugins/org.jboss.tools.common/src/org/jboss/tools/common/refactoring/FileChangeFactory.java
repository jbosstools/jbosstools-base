/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.refactoring;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.internal.ui.javaeditor.EditorUtility;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.EclipseUtil;

public class FileChangeFactory {
	public static TextFileChange getFileChange(IFile file){
		ICompilationUnit compilationUnit = EclipseUtil.getCompilationUnit(file);
		
		if(compilationUnit != null){
			return new BaseJavaFileChange("", compilationUnit);
		}else{
			return new BaseFileChange(file);
		}
	}
	
	public static boolean isOpenInEditor(IFile file) throws PartInitException{
		IEditorInput input = EditorUtility.getEditorInput(file);
		for(IWorkbenchWindow window : PlatformUI.getWorkbench().getWorkbenchWindows()){
			for(IWorkbenchPage page : window.getPages()){
				for(IEditorReference editorReference : page.getEditorReferences()){
					IEditorPart editor = editorReference.getEditor(true);
					if(editor != null && editor.getEditorInput().equals(input)){
						return true;
					}
				}
			}
		}
		return false;
	}
}
