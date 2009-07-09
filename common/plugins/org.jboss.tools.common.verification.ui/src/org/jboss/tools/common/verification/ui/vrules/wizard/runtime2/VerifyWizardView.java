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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime2;

import java.text.MessageFormat;
import java.util.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.ui.wizards.query.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.ProgressMonitorPart;

import org.jboss.tools.common.verification.ui.Messages;
import org.jboss.tools.common.verification.ui.vrules.wizard.runtime.VTaskListenerSafeImpl;
import org.jboss.tools.common.meta.key.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;
import org.jboss.tools.common.verification.vrules.layer.VObjectImpl;

public class VerifyWizardView extends AbstractQueryWizardView {
	static final String COMMAND_CANCEL = Messages.VerifyWizardView_Cancel;
	static final String COMMAND_RUN = Messages.VerifyWizardView_Run;
	static final String COMMAND_CLOSE = Messages.VerifyWizardView_Close;
	protected XModel model;
	protected VObject vobject;
	protected VTask task = null;
	protected VTaskListenerImpl taskListener = new VTaskListenerImpl(this);
	protected VTaskListener listener = new VTaskListenerSafeImpl(taskListener);
	ProgressMonitorPart progressMonitorPart;

	public VerifyWizardView() {
		this.setMessage(WizardKeys.getString("VerifyWizardView.Message")); //$NON-NLS-1$
		this.setTitle(WizardKeys.getString("VerifyWizardView.Title")); //$NON-NLS-1$
		this.setWindowTitle(WizardKeys.getString("VerifyWizardView.WindowTitle")); //$NON-NLS-1$
	}

	private VManager getRulesManager() {
		return VHelper.getManager(/*PreferenceModelUtilities.getPreferenceModel()*/);
	}

	public void setDialog(Dialog d) {
		super.setDialog(d);
		d.getShell().addShellListener(new CL());
	}
	
	class CL extends ShellAdapter {
		boolean activated = false;
		public void shellActivated(ShellEvent e) {
			if(activated) return;
			activated = true;
			action(COMMAND_RUN);
		}
	}
	
	public Control createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.horizontalSpacing = 10;
		layout.marginHeight = 10;
		layout.verticalSpacing = 10;
		layout.marginWidth = 10;
		composite.setLayout(layout);
		GridData gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);

		GridLayout pmlayout = new GridLayout();
		pmlayout.numColumns = 1;
		pmlayout.marginWidth = 20;
		progressMonitorPart =
			new ProgressMonitorPart(composite, pmlayout, SWT.DEFAULT);
		progressMonitorPart.setLayoutData(
			new GridData(GridData.FILL_HORIZONTAL));
		progressMonitorPart.setVisible(true);
		return composite;
	}

	public void setObject(Object data) {
		Properties p = findProperties(data);
		if(p != null) {
			String key = p.getProperty("help"); //$NON-NLS-1$
			setHelpKey(key);
		}
		Object[] os = (Object[])data;
		XModelObject object = (XModelObject)os[0];
		model = object.getModel(); 
//		VManager rulesManager = getRulesManager();
		ruleCount = 0;
		VModel vmodel = VModelFactory.getModel(object.getModel());
		vobject = vmodel.getObjectByPath(object.getPath());
		VRule[] rules = VHelper.getRules(getRulesManager(), vobject);
		ruleCount = (rules == null) ? 0 : rules.length;
	}
	
	static int PROGRESS_INIT_SIZE = 40;
	static int PROGRESS_MAIN_SIZE = 160;
	static int PROGRESS_FIN_SIZE = 40;
	static int PROGRESS_TOTAL_SIZE = PROGRESS_INIT_SIZE + PROGRESS_MAIN_SIZE + PROGRESS_FIN_SIZE;
	

	public void action(String command) {
		if(COMMAND_RUN.equals(command)) {
			if(task != null) task.removeTaskListener(listener);
			taskListener.setModel(model);
			task = getRulesManager().createTask(vobject);
			taskListener.setTask(task);
			task.addTaskListener(listener);
			progressMonitorPart.beginTask(
					MessageFormat.format(Messages.VerifyWizardView_VerifyTask, 
							((VObjectImpl)vobject).getModelObject().getPresentationString()), 
							PROGRESS_TOTAL_SIZE);
			progressMonitorPart.worked(PROGRESS_INIT_SIZE);
			task.start();
		} else if(COMMAND_CANCEL.equals(command)) {
			if(task != null) {
				task.removeTaskListener(listener);
				task.stop();
				task = null;
			}
			setCode(0);
			dispose();
		} else if(COMMAND_CLOSE.equals(command)) {
			setCode(0);
			dispose();
		}		
	}

	public String[] getCommands() {
		return new String[]{CANCEL};
	}
	
	protected int ruleCount = 1;
	int ruleIndex = 0;
	
	void onRuleFinished(VObject object) {
		if(vobject != object || ruleIndex >= ruleCount) return;
		ruleIndex++;
		progressMonitorPart.worked(PROGRESS_MAIN_SIZE / ruleCount);		
	}
	
	void onFinish() {
		progressMonitorPart.worked(PROGRESS_FIN_SIZE);		
	}
}
