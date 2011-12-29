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
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.MultiStateTextFileChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.UndoEdit;

public class JBDSFileChange extends MultiStateTextFileChange{
	private IFile file;
	private JBDSTextChange rootChange = null;

	public JBDSFileChange(IFile file) {
		super(file.getName(), file);
		this.file = file;
		setSaveMode(TextFileChange.LEAVE_DIRTY);
	}
	
	public IFile getFile(){
		return file;
	}
	
	public void setEdit(TextEdit edit) {
		rootChange = new JBDSTextChange();
		rootChange.setEdit(edit);
		super.addChange(rootChange);
	}
	
	public TextEdit getEdit(){
		return rootChange.getEdit();
	}
	
	public void addEdit(TextEdit edit){
		rootChange.addEdit(edit);
	}
	
	class JBDSTextChange extends TextChange{

		protected JBDSTextChange() {
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
