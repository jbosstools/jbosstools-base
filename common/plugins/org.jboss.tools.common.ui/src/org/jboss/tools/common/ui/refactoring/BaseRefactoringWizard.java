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
package org.jboss.tools.common.ui.refactoring;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class BaseRefactoringWizard extends RefactoringWizard {

	public BaseRefactoringWizard(Refactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
	}

	@Override
	public boolean performFinish() {
		boolean result = super.performFinish();
		
		saveAllEditors();
		
		return result;
	}
	
	private void saveAllEditors(){
		for(IWorkbenchWindow window : PlatformUI.getWorkbench().getWorkbenchWindows()){
			for(IWorkbenchPage page : window.getPages()){
				for(IEditorReference eReference : page.getEditorReferences()){
					IEditorPart editor = eReference.getEditor(true);
					if(editor != null && editor.isDirty()){
						editor.doSave(new NullProgressMonitor());
					}
				}
			}
		}
	}

}
