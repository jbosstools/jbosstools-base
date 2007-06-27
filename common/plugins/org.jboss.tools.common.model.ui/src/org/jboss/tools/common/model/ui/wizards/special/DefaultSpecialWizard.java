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
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.help.HelpUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
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
		Shell shell = (Shell)support.getProperties().get("shell");
		if(shell == null) shell = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		dialog = new DefaultSpecialWizardDialog(shell);
		dialog.setWizard(this);
		dialog.create();
		progressPart = new ProgressPart(dialog.getShell(), this);
		support.getProperties().put("dialogShell", dialog.getShell());
		support.getProperties().put("IRunnableContext", progressPart);
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
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		dialog.open();
		
	}	
	
	public void action(String name) {
		if(dialog == null) return;
		CommandBar bar = dialog.getCommandBar();
		bar.disable();
		if(SpecialWizardSupport.HELP.equals(name)) {
			try {
				HelpUtil.helpEclipse(support.getTarget().getModel(), support.getHelpKey());
			} catch (Exception e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
			return;
		}
		try {
			save();
			support.action(name);
			dialog.setMessage("");
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		try {
			setStep();
		} catch (Exception t) {
			ModelUIPlugin.getPluginLog().logError("Error while executing action " + name, t);
		}
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
		try {
			wizardStep.validate();
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}
	
	private ISpecialWizardStep getStep(int i) {
		if(i < 0) return null;
		String cls = support.getStepImplementingClass(i);
		ISpecialWizardStep step = null;
		try {
			step = (ISpecialWizardStep)ModelFeatureFactory.getInstance().createFeatureInstance(cls);
			//Class.forName(cls, true, getClass().getClassLoader()).newInstance();
			if(step instanceof AbstractSpecialWizardStep) {
				((AbstractSpecialWizardStep)step).setWizard(this);
			}
			step.setSupport(support, i);
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError("Cannot load class '" + cls + "'.");
		}
		return step;
	}

	public void save() {
		if(wizardStep != null) wizardStep.save();
	}

	public void dispose() {
		stopValidator();
		if(dialog == null) return;
		try { 
			if(dialog.getShell() != null && !dialog.getShell().isDisposed()) dialog.close(); 
		} catch (Exception e) {}
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
///		synchronized(validationMonitor) {
///			try { validationMonitor.notifyAll(); } catch (Exception e) {}
///		}
	}	

	private ValidationRunnable validationRunnable = new ValidationRunnable();
///	Object validationMonitor = new Object();
	
	public void stopValidator() {
		if(validationRunnable == null) return;
		validationRunnable = null;
///		synchronized(validationMonitor) {
///			try { validationMonitor.notifyAll(); } catch (Exception e) {}
///		}
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
///			while(validationRunnable != null) {
///				while(timeStamp != lastTimeStamp) {
///					try { Thread.sleep(100); } catch (Exception e) {}
///					lastTimeStamp = timeStamp;
///					safeUpdateValidationData();
///				}
///				synchronized(validationMonitor) {
///					try { validationMonitor.wait(); } catch (Exception e) {}
///				}
///			}
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
