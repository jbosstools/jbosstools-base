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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;

import org.jboss.tools.common.model.ui.editors.dnd.composite.TagProposalsComposite;

/**
 * 
 * @author eskimo
 *
 */
public class DropWizard extends Wizard implements PropertyChangeListener, IDropWizard {
	IDropCommand fDropCommand;
//	IDropWizardModel fModel;
	/**
	 * 
	 * @param command
	 */
	public DropWizard() {
		setWindowTitle(DropWizardMessages.Wizard_Window_Title);
	}
	
	public void setCommand(IDropCommand command) {
		fDropCommand = command;
	} 
	private TagProposalsWizardPage page1 = null;
	private TagAttributesWizardPage page2 = null;	
	
	/**
	 * 
	 */
	public void addPages() {
		super.addPages();		
		page1 = new TagProposalsWizardPage();
		page2 = new TagAttributesWizardPage();
		ITagProposal[] proposals = 
			TagProposalsComposite.getTagProposals(getMimeType(),getMimeData(), fDropCommand.getTagProposalFactory());
		
		if(TagProposalsComposite.areThereTagProposals(
			getMimeType(),getMimeData(), fDropCommand.getTagProposalFactory())
		) {
			if(proposals.length>1) {
				this.addPage(page1);
			}
		}
		this.addPage(page2);

		getWizardModel().addPropertyChangeListener(this);		
		
		if(proposals.length==1) { 
			getWizardModel().setTagProposal(proposals[0]);
		}
	}
	
	/**
	 * 
	 */
	public boolean canFinish() {
		return getWizardModel().isValid();

	}

	/**
	 * 
	 */
	public boolean performFinish() {
		fDropCommand.execute();
		return true;
	}
	
	/**
	 * 
	 */
	public boolean performCancel() {
		return true;
	}

	/**
	 * 
	 * @return
	 */
	public IDropWizardModel getWizardModel() {
		return fDropCommand.getDefaultModel();
	}

	/**
	 * 
	 * @return
	 */
	public String getMimeData() {
		return  getWizardModel().getDropData().getMimeData();
	}

	/**
	 * 
	 * @return
	 */
	public String getMimeType() {
		return  getWizardModel().getDropData().getMimeType();
	}

	/**
	 * Unexplainable update logic for wizard buttons 
	 */
	public void propertyChange(PropertyChangeEvent evt) {
		IWizardPage[] pages = getPages();
		for (int i = 0; i < pages.length; i++) {
			DefaultDropWizardPage page = (DefaultDropWizardPage)pages[i];
			page.runValidation();
		}
		if(page1.isPageComplete()
				&& getWizardModel().getAttributeValueDescriptors().length==0) {
			page1.setPageComplete(false);
		}
	}

	public void dispose() {
		getWizardModel().removePropertyChangeListener(this);
		super.dispose();
	}

}
