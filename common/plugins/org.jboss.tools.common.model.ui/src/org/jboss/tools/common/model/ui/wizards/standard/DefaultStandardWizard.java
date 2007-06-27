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
package org.jboss.tools.common.model.ui.wizards.standard;

import java.util.*;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.action.impl.WizardDataValidator;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class DefaultStandardWizard extends Wizard {
	protected SpecialWizardSupport support = null;
	protected DefaultStandardStep[] steps = new DefaultStandardStep[0];
	Composite pageContainer;
	boolean isFinishEnabled = true;
	
	public void setSupport(SpecialWizardSupport support) {
		this.support = support;
	}

	public boolean performFinish() {
		try {
			support.action(SpecialWizardSupport.FINISH);
			return support.isFinished();
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		return false;
	}

	public boolean canFinish() {
		return support.isActionEnabled(SpecialWizardSupport.FINISH) && isFinishEnabled;
	}

	public void addPages() {
		XEntityData[] data = support.getEntityData();
		steps = new DefaultStandardStep[data.length];
		for (int i = 0; i < data.length; i++) {
			steps[i] = new DefaultStandardStep(support, i);
			steps[i].setWizard(this);
		}
	}
	
	public void createPageControls(Composite pageContainer) {
		this.pageContainer = pageContainer;
		for (int i = 0; i < steps.length; i++){
			IWizardPage page = steps[i];
			page.createControl(pageContainer);
		}
	}

	public void dispose() {
		for (int i = 0; i < steps.length; i++)steps[i].dispose();
		super.dispose();
	}

	public IWizardPage getNextPage(IWizardPage page) {
		try {
			support.action(SpecialWizardSupport.NEXT);
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		int id = support.getStepId();
		return steps[id];
	}

	public IWizardPage[] getPages() {
		return steps;
	}

	public IWizardPage getPreviousPage(IWizardPage page) {
		int id = support.getPreviousStepId();
		return (id < 0 || id >= steps.length) ? null : steps[id];
	}

	public IWizardPage getStartingPage() {
		return (steps.length == 0) ? null : steps[0];
	}

	public boolean needsPreviousAndNextButtons() {
		return steps.length > 1;
	}

	public void dataChanged(WizardDataValidator validator, Properties data) {
		if(validator == null) return;
		updateValidationData(validator, data);
	}	

	void updateValidationData(WizardDataValidator validator, Properties data) {
		if(validator == null) return; 
		validator.validate(data);
		String message = validator.getErrorMessage();
		DefaultStandardStep wizardStep = steps[support.getStepId()];
		if(wizardStep != null && !wizardStep.isDataChanged()) {
			String m = support.getMessage(support.getStepId());
			if (m == null || m.length() == 0) m = message;
			wizardStep.setDescription(m);
			wizardStep.setErrorMessage(null);
		} else {
			wizardStep.setDescription(null);
			wizardStep.setErrorMessage(message);
		}
		isFinishEnabled = validator.isCommandEnabled(SpecialWizardSupport.FINISH);
		boolean isNextEnabled = validator.isCommandEnabled(SpecialWizardSupport.NEXT);
		wizardStep.isNextEnabled = isNextEnabled;
		getContainer().updateButtons();
	}
	
}
