/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.editors.dnd;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;

/**
 * 
 * @author eskimo
 *
 */
public abstract class DefaultDropWizardPage extends WizardPage {

	/**
	 * 
	 * @param pageName
	 * @param title
	 */
	protected DefaultDropWizardPage(String pageName, String title) {
		this(pageName, title,null);
	}
	
	/**
	 * 
	 * @param pageName
	 * @param title
	 * @param titleImage
	 */
	protected DefaultDropWizardPage(String pageName, String title, ImageDescriptor titleImage) {
		super(pageName, title, titleImage);
	}
	
	/**
	 * 
	 * @return
	 */
	public IDropWizard getSpecificWizard() {
		return (IDropWizard)getWizard();
	}

	/**
	 * 
	 * @return
	 */
	public IDropWizardModel getDropWizardModel() {
		return getSpecificWizard().getWizardModel();
	}
	
	/**
	 * Run validate method on every wizard page 
	 *
	 */
	public  void runValidation() {
		try {
			validate();
			setPageComplete(true);
			setMessage("",NONE);				
		} catch (ValidationException e) {
			validationErrorOccurs(e);
			setPageComplete(false);
		}
	}
	
	/**
	 * 
	 * @param e
	 */
	public void validationErrorOccurs(ValidationException e) {
		setMessage(e.getMessage(),ERROR);
	}

	/**
	 * 
	 * @throws ValidationException
	 */
	public void validate() throws ValidationException {
	}
}