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
package org.jboss.tools.common.model.ui.wizards.special;

import java.util.Properties;

import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.wizards.standard.DefaultStandardWizard;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.help.HelpUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.ui.ModelUIImages;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class DefaultSpecialWizard implements SpecialWizard, SpecialWizardControlListener {
	protected SpecialWizardSupport support = null;
	protected ISpecialWizardStep wizardStep = null;
	protected DefaultSpecialWizardDialog dialog = null;
	ProgressPart progressPart = null;
	
	public DefaultSpecialWizard() {
	}
	
	public void setObject(Object object) {
		setSupport((SpecialWizardSupport)((Object[])object)[0]);
	}
	
	public int execute() {
		Shell shell = (Shell)support.getProperties().get("shell"); //$NON-NLS-1$
		if(shell == null) shell = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();

		if(support.canBeProcessedByStandardWizard()) {
			DefaultStandardWizard dsw = new DefaultStandardWizard();
			dsw.setWindowTitle(support.getTitle());
			dsw.setSupport(support);
			dsw.setDefaultPageImageDescriptor(ModelUIImages.getImageDescriptor(ModelUIImages.WIZARD_DEFAULT));
			WizardDialog wd = new WizardDialog(shell, dsw);
			wd.create();
			PlatformUI.getWorkbench().getHelpSystem().setHelp(wd.getShell(), "org.eclipse.ui.new_wizard_shortcut_context"); //$NON-NLS-1$
			int ii = wd.open();
			return ii;
		}		
		
		dialog = new DefaultSpecialWizardDialog(shell);
		dialog.setWizard(this);
		dialog.create();
		progressPart = new ProgressPart(dialog.getShell(), this);
		support.getProperties().put("dialogShell", dialog.getShell()); //$NON-NLS-1$
		support.getProperties().put("IRunnableContext", progressPart); //$NON-NLS-1$
		open();
		return 0;
	}
	
	public ProgressPart getProgressPart() {
		return progressPart;
	}
	
	public boolean needsProgressMonitor() {
		return true;
	}
	
	private void open() {
		if(validationRunnable == null) validationRunnable = new ValidationRunnable();
//		String threadname = "Wizard Validation - " + support.getTitle(); 
///		new Thread(validationRunnable, threadname).start();
		dialog.getShell().pack(true);
		setStep();
		try {
			support.action("STEP");
		} catch (XModelException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		dialog.open();
		
	}	
	
	public void action(String name) {
		if(dialog == null) return;
		CommandBar bar = dialog.getCommandBar();
		bar.disable();
		if(SpecialWizardSupport.HELP.equals(name)) {
			HelpUtil.helpEclipse(support.getTarget().getModel(), support.getHelpKey());
			return;
		}
		try {
			save();
			support.action(name);
			dialog.setMessage(""); //$NON-NLS-1$
		} catch (XModelException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		setStep();
	}
	
	private void setStep() {
		if(wizardStep != null) wizardStep.dispose();
		if(support.isFinished()) {
			dispose();
			return;
		}
		int i = support.getStepId();
		if(support.isFinished()) return;
		String message = support.getMessage(i);			
		if(i >= 0) {
			wizardStep = getStep(i);
			if(wizardStep == null) return;
			wizardStep.update();
		} else {
			wizardStep = null;
		}
		if(wizardStep == null) return;
		dialog.setMaximumSize(wizardStep.getMaximumSize());
		dialog.setMinimumSize(wizardStep.getMinimumSize());
		dialog.updateDialogArea();
		dialog.updateButtonsBar(getSupport().getActionNames(i));
		dialog.getShell().setText(support.getTitle());
		dialog.setTitle(support.getSubtitle());
		if(message != null) dialog.setMessage(message);
		wizardStep.validate();
	}
	
	private ISpecialWizardStep getStep(int i) {
		if(i < 0) return null;
		String cls = support.getStepImplementingClass(i);
		ISpecialWizardStep step = null;
		try {
			step = (ISpecialWizardStep)ModelFeatureFactory.getInstance().createFeatureInstance(cls);
			if(step instanceof AbstractSpecialWizardStep) {
				((AbstractSpecialWizardStep)step).setWizard(this);
			}
			step.setSupport(support, i);
		} catch (ClassCastException e) {
			ModelUIPlugin.getPluginLog().logError("Cannot load class '" + cls + "'."); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return step;
	}

	public void save() {
		if(wizardStep != null) wizardStep.save();
	}

	public void dispose() {
		stopValidator();
		if(dialog == null) return;
		if(dialog.getShell() != null && !dialog.getShell().isDisposed()) dialog.close(); 
		dialog = null;
	}
	
	public SpecialWizardSupport getSupport() {
		return support;
	}
	
	public void setSupport(SpecialWizardSupport support) {
		this.support = support;
		if(support != null) support.setControlListener(this);
	}
	
	public ISpecialWizardStep getWizardStep() {
		return wizardStep;
	}
	
	public void setWizardStep(ISpecialWizardStep step) {
		wizardStep = step;
	}
	
	public void dataChanged(WizardDataValidator validator, Properties data) {
		if(validator == null) return;
		if(validationRunnable == null) return;
		validationRunnable.setData(validator, data);

		validationRunnable.safeUpdateValidationData();
	}	

	private ValidationRunnable validationRunnable = new ValidationRunnable();
	
	public void stopValidator() {
		if(validationRunnable == null) return;
		validationRunnable = null;
	}
	
	class ValidationRunnable implements Runnable {
		Properties data;
		WizardDataValidator validator;
		long timeStamp = -1, lastTimeStamp = -1;
		
		public void setData(WizardDataValidator validator, Properties data) {
			this.data = data;
			this.validator = validator;
			timeStamp = System.currentTimeMillis();
		}
		
		public void run() {
		}
		
		void safeUpdateValidationData() {
			Display.getDefault().syncExec(new Runnable() {
				public void run() {
					updateValidationData();
				}
			});			
		}
		
		void updateValidationData() {
			if(validationRunnable == null) return; 
			validator.validate(data);
			String message = validator.getErrorMessage();
			String warning = validator.getWarningMessage();
			if(wizardStep != null && (!wizardStep.isDataChanged() || (message == null && warning == null))) {
				dialog.setErrorMessage(null);
				String m = support.getMessage(support.getStepId());
				if (m == null || m.length() == 0) m = message == null ? warning : message;
				dialog.setMessage(m);
			} else {
				dialog.setErrorMessage(message);
				if(message != null || warning == null) {
					dialog.setMessage(null);
				} else {
					dialog.setMessage(warning, IMessageProvider.WARNING);
				}
			}
			CommandBar bar = dialog.getCommandBar();
			String[] s = support.getActionNames(validator.getId());
			for (int i = 0; i < s.length; i++)
			  bar.setEnabled(s[i], validator.isCommandEnabled(s[i]));	
		}

	}
	
}
