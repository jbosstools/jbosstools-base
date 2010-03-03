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

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagProposalsComposite;

/**
 * 
 * @author eskimo
 *
 */

public abstract class DefaultDropCommand implements IDropCommand {
	protected ITagProposalFactory tagProposalFactory;
	
	private IDropWizardModel fDropWizardDataModel = null;
	
	public void setTagProposalFactory(ITagProposalFactory tagProposalFactory) {
		this.tagProposalFactory = tagProposalFactory;
	}
	public ITagProposalFactory getTagProposalFactory() {
		return tagProposalFactory;
	}
	
	/**
	 * 
	 * @return
	 */
	protected IDropWizardModel createSpecificModel() {
		IDropWizardModel newModel = new DefaultDropWizardModel(tagProposalFactory);
		return newModel;
	}

	/**
	 * 
	 */
	public IDropWizardModel getDefaultModel() {
		if(fDropWizardDataModel==null) {
			fDropWizardDataModel = createSpecificModel();
		}
		return fDropWizardDataModel;
	}
	
	/**
	 * Init model  
	 */
	public void initialize() {
		if(getDefaultModel().getDropData()==null) {
			throw new IllegalStateException("Wizard model isn't intialized, call setDropDataFirst"); //$NON-NLS-1$
		}
		ITagProposal[] proposals 
			= TagProposalsComposite.getTagProposals(
				getDefaultModel().getDropData().getMimeType(),
				getDefaultModel().getDropData(),
				tagProposalFactory
			);
		if(proposals.length==1) {
			getDefaultModel().setTagProposal(proposals[0]);
		}
	}
	
	/**
	 * Run command with or without wizard
	 * @param data
	 */
	public void execute(DropData data) {
		/*
		 * Fixes https://jira.jboss.org/jira/browse/JBIDE-5874
		 * Checks it the Dialog Shell was already opened.
		 * If so do not create one more instance.
		 */
		Shell[] existedShells = PlatformUI.getWorkbench().getDisplay().getShells();
		boolean dialogWasAlreadyOpened = false;
		for (Shell sh : existedShells) {
			if (DropWizardMessages.Wizard_Window_Title
					.equalsIgnoreCase(sh.getText())) {
				dialogWasAlreadyOpened = true;
			}
		}
		if (!dialogWasAlreadyOpened) {
			getDefaultModel().setDropData(data);
			initialize();
			if(getDefaultModel().isWizardRequired()) {
				WizardDialog dialog = new DropWizardDialog(
						PlatformUI.getWorkbench().getDisplay().getActiveShell(),
						createDropWizard()
				);
				dialog.open();
			} else {
				execute();
			}
			getDefaultModel().setDropData(null);
		}
	}
	
	protected IDropWizard createDropWizard() {
		DropWizard wizard = new DropWizard();
		wizard.setCommand(this);
		return wizard;
	}	
	
	/**
	 * Run command without wizard
	 */
	public void execute() {
		if(getDefaultModel().getDropData()==null) {
			throw new IllegalStateException("Call setDndData() first"); //$NON-NLS-1$
		} else if(getDefaultModel().getTagProposal()!=IDropWizardModel.UNDEFINED_TAG_PROPOSAL) {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			try {
				workspace.run(this,new NullProgressMonitor());
			} catch (CoreException e) {
//				VpePlugin.reportProblem(e);
				ModelUIPlugin.getPluginLog().logError(e);
			}
		} else {
			executeUnknownTag();
		}
	}
	
	protected void executeUnknownTag() {
		DropCommandFactory
		.getInstance()
		.getDropCommand(
			DropCommandFactory.UNKNOWN_MIME_TYPE, tagProposalFactory
		).execute(
			getDefaultModel().getDropData()
		);
	}
}
