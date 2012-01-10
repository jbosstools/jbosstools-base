/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.internal.ui.javaeditor.EditorUtility;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.MultiStateTextFileChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.UndoEdit;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.common.CommonPlugin;

public class BaseFileChange extends MultiStateTextFileChange{
	private IFile file;
	private BaseTextChange rootChange = null;

	public BaseFileChange(IFile file) {
		super(file.getName(), file);
		this.file = file;
		
		setSaveMode();
	}
	
	private void setSaveMode(){
		UIJob job = new UIJob("setSaveMode"){ //$NON-NLS-1$
			public IStatus runInUIThread(IProgressMonitor monitor) {
				try {
					if(isOpenInEditor(file)){
						setSaveMode(TextFileChange.LEAVE_DIRTY);
					}else{
						setSaveMode(TextFileChange.FORCE_SAVE);
					}
				} catch (PartInitException e) {
					CommonPlugin.getDefault().logError(e);
				}
				return Status.OK_STATUS;
			}};
		
		job.setSystem(true);
		job.schedule();
	}
	
	private static boolean isOpenInEditor(IFile file) throws PartInitException{
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
	
	public IFile getFile(){
		return file;
	}
	
	public void setEdit(TextEdit edit) {
		rootChange = new BaseTextChange();
		rootChange.setEdit(edit);
		super.addChange(rootChange);
	}
	
	public TextEdit getEdit(){
		return rootChange.getEdit();
	}
	
	public void addEdit(TextEdit edit){
		rootChange.addEdit(edit);
	}
	
	class BaseTextChange extends TextChange{

		protected BaseTextChange() {
			super("");
		}

		@Override
		protected IDocument acquireDocument(IProgressMonitor pm)
				throws CoreException {
			return null;
		}

		@Override
		protected void commit(IDocument document, IProgressMonitor pm)
				throws CoreException {
		}

		@Override
		protected void releaseDocument(IDocument document, IProgressMonitor pm)
				throws CoreException {
		}

		@Override
		protected Change createUndoChange(UndoEdit edit) {
			return null;
		}

		@Override
		public void initializeValidationData(IProgressMonitor pm) {
		}

		@Override
		public RefactoringStatus isValid(IProgressMonitor pm)
				throws CoreException, OperationCanceledException {
			return null;
		}

		@Override
		public Object getModifiedElement() {
			return null;
		}
		
	}
}
