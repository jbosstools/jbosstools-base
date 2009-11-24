 /*******************************************************************************
  * Copyright (c) 2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributors:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.el.ui.refactoring;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.jboss.tools.common.el.core.refactoring.RenameELVariableProcessor;
import org.jboss.tools.common.model.ui.attribute.editor.IFieldEditor;

/**
 * @author Daniel Azarov
 */
public class RenameELVariableWizard extends RefactoringWizard {

	private String componentName;
	private IFieldEditor editor;

	public RenameELVariableWizard(Refactoring refactoring) {
		super(refactoring, WIZARD_BASED_USER_INTERFACE);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ltk.ui.refactoring.RefactoringWizard#addUserInputPages()
	 */
	@Override
	protected void addUserInputPages() {
	    setDefaultPageTitle(getRefactoring().getName());
	    RenameELVariableProcessor processor= (RenameELVariableProcessor) getRefactoring().getAdapter(RenameELVariableProcessor.class);
	    addPage(new RenameELVariableWizardPage(processor));
	}
	
	class RenameELVariableWizardPage extends UserInputWizardPage{
		private RenameELVariableProcessor processor;
		
		public RenameELVariableWizardPage(RenameELVariableProcessor processor){
			super("");
			this.processor = processor;
		}

		public void createControl(Composite parent) {
			Composite container = new Composite(parent, SWT.NULL);
			container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
	        GridLayout layout = new GridLayout();
	        container.setLayout(layout);
	        layout.numColumns = 2;
	        
//	        String defaultName = component.getName();
//	        editor = IFieldEditorFactory.INSTANCE.createTextEditor(componentName, SeamUIMessages.SEAM_WIZARD_FACTORY_SEAM_COMPONENT_NAME, defaultName);
//	        editor.doFillIntoGrid(container);
//	        
//	        ((CompositeEditor)editor).addPropertyChangeListener(new PropertyChangeListener(){
//	        	public void propertyChange(PropertyChangeEvent evt){
//	        		validatePage();
//	        	}
//	        });
	        setControl(container);
	        setPageComplete(false);
		}
		
		protected final void validatePage() {
			RefactoringStatus status= new RefactoringStatus();
			setPageComplete(status);
		}
		
		/* (non-Javadoc)
		 * @see org.eclipse.ltk.ui.refactoring.UserInputWizardPage#performFinish()
		 */
		protected boolean performFinish() {
			
			initializeRefactoring();
			return super.performFinish();
		}

		/* (non-Javadoc)
		 * @see org.eclipse.ltk.ui.refactoring.UserInputWizardPage#getNextPage()
		 */
		public IWizardPage getNextPage() {
			initializeRefactoring();
			return super.getNextPage();
		}
		
		private void initializeRefactoring() {
			//processor.setNewName(editor.getValueAsString());
		}
		
	}
}