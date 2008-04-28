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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.editors.dnd.composite.TagProposalsComposite;

public class TagProposalsWizardPage extends DefaultDropWizardPage {

	protected TagProposalsWizardPage() {
		super("DnD Wizard","Select Tag ");
	}
	
	protected TagProposalsWizardPage(String pageName) {
		super(pageName,"Select tag for inserting");
	}

	public void createControl(Composite parent) {
		TagProposalsComposite tableTree 
			= new TagProposalsComposite(
				parent, 
				SWT.NONE,
				getSpecificWizard().getWizardModel()
		);
		setControl(tableTree);
		setVisible(!tableTree.hasTagProposals());
		setPageComplete(false);
		setMessage("Select tag for inserting", TagProposalsWizardPage.WARNING);
	}

	public boolean hasTagProposals() {
		return ((TagProposalsComposite)getControl()).hasTagProposals();
	}
	
	public void validate() throws ValidationException{
/*		if(getSpecificWizard().getWizardModel().getTagProposal()==null)
			throw new ValidationException("There is no one tag selected!");
		else 
			setMessage("",this.NONE);
*/	}
}
