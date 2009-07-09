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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.TreeItem;

import org.jboss.tools.common.verification.ui.XStudioVerificationPlugin;
import org.jboss.tools.common.verification.ui.vrules.preferences.VerificationPreferencePage;
import org.jboss.tools.common.verification.ui.vrules.wizard.DescriptionManager;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.verification.vrules.VHelper;
import org.jboss.tools.common.verification.vrules.VManager;
import org.jboss.tools.common.verification.vrules.VModel;
import org.jboss.tools.common.verification.vrules.VObject;
import org.jboss.tools.common.verification.vrules.VRule;
import org.jboss.tools.common.verification.vrules.VRuleSet;
import org.jboss.tools.common.verification.vrules.VTask;
import org.jboss.tools.common.verification.vrules.VTaskListener;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;

public class VerifyWizardView extends AbstractQueryWizardView {
	private static final String COMMAND_RESUME = VerificationUIMessages.VerifyWizardView_Resume;
	private static final String COMMAND_PAUSE = VerificationUIMessages.VerifyWizardView_Pause;
	private static final String COMMAND_STOP = VerificationUIMessages.VerifyWizardView_Stop;
	private static final String COMMAND_RUN_SELECTED = VerificationUIMessages.VerifyWizardView_RunSelected;
	private static final String COMMAND_RUN_ALL = VerificationUIMessages.VerifyWizardView_RunAll;
	static final String COMMAND_CLOSE = VerificationUIMessages.VerifyWizardView_Close;
	static String[] INITIAL_COMMANDS = new String[]{COMMAND_RUN_ALL, COMMAND_RUN_SELECTED, COMMAND_CLOSE, HELP};
	static String[] RUNNING_COMMANDS = new String[]{COMMAND_PAUSE, COMMAND_STOP, COMMAND_CLOSE, HELP};
	static String[] PAUSE_COMMANDS = new String[]{COMMAND_RESUME, COMMAND_STOP, COMMAND_CLOSE, HELP};
	protected RuntimeSignificanceView significance = new RuntimeSignificanceView();
	protected RuntimeRulesProvider provider = new RuntimeRulesProvider();
	protected TreeViewer treeViewer;
	protected XModel model;
	protected VObject vobject;
	protected VTask task = null;
	protected VTaskListener listener = new VTaskListenerSafeImpl(new VTaskListenerImpl(this));
	protected Map<String,RuntimeItemWrapper> wrappers = new HashMap<String,RuntimeItemWrapper>();
	protected DescriptionManager tip = new DescriptionManager();

	public VerifyWizardView() {
		this.setMessage(WizardKeys.getString("VerifyWizardView.Message")); //$NON-NLS-1$
		this.setTitle(WizardKeys.getString("VerifyWizardView.Title")); //$NON-NLS-1$
		this.setWindowTitle(WizardKeys.getString("VerifyWizardView.WindowTitle")); //$NON-NLS-1$
	}

	private VManager getRulesManager() {
		return VHelper.getManager(/*PreferenceModelUtilities.getPreferenceModel()*/);
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

		Control sc = significance.createControl(composite);
		sc.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		treeViewer = new TreeViewer(composite, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.MULTI);
		treeViewer.setContentProvider(provider);
		treeViewer.setLabelProvider(provider);
		treeViewer.setInput(this);
		Control tc = treeViewer.getControl();
		tc.setLayoutData(new GridData(GridData.FILL_BOTH));
		significance.update();
		tip.install(treeViewer.getTree());
		treeViewer.expandToLevel(2);
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
		VManager rulesManager = getRulesManager();
		significance.setManager(rulesManager);
		VModel vmodel = VModelFactory.getModel(object.getModel());
		vobject = vmodel.getObjectByPath(object.getPath());
		VRule[] rules = VHelper.getRules(rulesManager, vobject);
		if(rules == null) return;
		VRuleSet[] sets = getRuleSets(rules);
		RuntimeRuleSetWrapper[] ruleSets = new RuntimeRuleSetWrapper[sets.length];
		if(sets != null) for (int i = 0; i < sets.length; i++) {
			if(!sets[i].isEnabled()) continue;
			ruleSets[i] = new RuntimeRuleSetWrapper(sets[i]);
			wrappers.put(sets[i].getName(), ruleSets[i]);
			new RWLImpl(ruleSets[i]);
			VRule[] rs = getRules(sets[i], rules);
			ArrayList<RuntimeRuleWrapper> lc = new ArrayList<RuntimeRuleWrapper>();
			for (int j = 0; j < rs.length; j++) {
				if(!rs[j].isEnabled()) continue;
				RuntimeRuleWrapper r = new RuntimeRuleWrapper(rs[j]);
				wrappers.put(rs[j].getName(), r);
				r.setManager(rulesManager);
				new RWLImpl(r);
				lc.add(r);
			}
			RuntimeItemWrapper[] cw = lc.toArray(new RuntimeItemWrapper[0]);
			ruleSets[i].children = cw;
		}
		provider.ruleSets = ruleSets;
		changeControl(INITIAL_COMMANDS);
	}

	public String[] getCommands() {
		return INITIAL_COMMANDS;
	}
	
	public String getDefaultCommand() {
		return INITIAL_COMMANDS[0];
	}

	private VRuleSet[] getRuleSets(VRule[] rules) {
		Set<String> s = new HashSet<String>();
		ArrayList<VRuleSet> l = new ArrayList<VRuleSet>();
		for (int i = 0; i < rules.length; i++) {
			VRuleSet set = rules[i].getRuleSet();
			if(s.contains(set.getName())) continue;
			s.add(set.getName());
			l.add(set);
		}
		return l.toArray(new VRuleSet[0]);
	}

	private VRule[] getRules(VRuleSet set, VRule[] rules) {
		ArrayList<VRule> l = new ArrayList<VRule>();
		for (int i = 0; i < rules.length; i++)
		  if(set == rules[i].getRuleSet()) l.add(rules[i]);
		return l.toArray(new VRule[0]);
	}

	class RWLImpl implements RuntimeItemWrapperListener {
		RuntimeItemWrapper wrapper;
		public RWLImpl(RuntimeItemWrapper wrapper) {
			this.wrapper = wrapper;
			wrapper.setListener(this);
		}
		public void statusChanged() {
			if(treeViewer != null) treeViewer.refresh(wrapper);
		}
	}

	/**
	 * 
	 * @param commands (translatable)
	 */
	public void changeControl(String[] commands) {
		commandBar.setCommands(commands);
		commandBar.setDefaultCommand(commands[0]);
		commandBar.update();
		if(commandBar.getControl() != null && !commandBar.getControl().isDisposed()) {
			commandBar.getControl().pack();
			commandBar.getControl().getParent().layout();
		}
	}
	
	public RuntimeItemWrapper findWrapper(String id) {
		return (RuntimeItemWrapper)wrappers.get(id);
	}

	public void action(String command) {
		stopEditing();
		if(COMMAND_RUN_ALL.equals(command)) {
			clearStatus();
			if(task != null) task.removeTaskListener(listener);
			task = getRulesManager().createTask(vobject);
			task.addTaskListener(listener);
			task.start();
		} else if(COMMAND_RUN_SELECTED.equals(command)) {
			clearStatus();
			VRule[] rs = getSelectedRules();
			if(rs == null) return;
			if(task != null) task.removeTaskListener(listener);
			task = getRulesManager().createTask(vobject, rs);
			task.addTaskListener(listener);
			task.start();
		} else if(COMMAND_CLOSE.equals(command)) {
			if(task != null) {
				task.removeTaskListener(listener);
				task.stop();
				task = null;
			}
			setCode(0);
			dispose();
		} else if(COMMAND_STOP.equals(command)) {
			if(task != null) task.stop();
		} else if(COMMAND_PAUSE.equals(command)) {
			if(task != null) task.pause();
		} else if(COMMAND_RESUME.equals(command)) {
			if(task != null) task.start();
		} else if(HELP.equals(command)) {
			super.action(command);
		}
		
	}
	
	static int ERROR_COUMT_LIMIT = -1;
	int getErrorCountLimit() {
		XModelObject o = PreferenceModelUtilities.getPreferenceModel().getByPath(VerificationPreferencePage.PREFERENCES[0]);
		if(o == null) return -1;
		String s = o.getAttributeValue(VerificationPreferencePage.ATTR_ERRORS_NUMBER_LIMIT);
		int limit = -1;
		if(!"unlimited".equals(s) && s != null && s.length() > 0) try { //$NON-NLS-1$
			limit = Integer.parseInt(s);
		} catch (NumberFormatException e) {
			XStudioVerificationPlugin.getPluginLog().logError(e);
		}
		return limit;		
	}

	boolean limitLock = false;
	public void limitReached() {
		if(limitLock) return;
		limitLock = true;
		Display.getDefault().syncExec( 
			new Runnable() {
				public void run() {
					ServiceDialog d = model.getService();
					d.showDialog(VerificationUIMessages.WARNING, NLS.bind(VerificationUIMessages.LIMIT_OF_REPORTED_ERRORS_IS_REACHED, ""+getErrorCountLimit()), new String[]{VerificationUIMessages.OK}, null, ServiceDialog.WARNING); //$NON-NLS-1$
					limitLock = false;
					task.stop();
				}
			}
		);
	}

	private VRule[] getSelectedRules() {
		if(treeViewer == null) return null;
		ArrayList<VRule> list = new ArrayList<VRule>();
		Set<String> set = new HashSet<String>();
		TreeItem[] is = treeViewer.getTree().getSelection();
		if(is == null || is.length == 0) return null;
		for (int i = 0; i < is.length; i++) {
			RuntimeItemWrapper w = (RuntimeItemWrapper)is[i].getData();
			if(w == null) continue;
			VRule[] rs = w.getRules();
			for (int j = 0; j < rs.length; j++) {
				if(set.contains(rs[j].getName())) continue;
				set.add(rs[j].getName());
				list.add(rs[j]);
			}
		}
		return (VRule[])list.toArray(new VRule[0]);
	}

	private void clearStatus() {
		RuntimeRuleSetWrapper[] ruleSets = provider.getRuleSets();
		for (int i = 0; i < ruleSets.length; i++) {
			ruleSets[i].setStatus(0);
			RuntimeItemWrapper[] cs = ruleSets[i].children;
			for (int j = 0; j < cs.length; j++) cs[j].setStatus(0);
		}
	}
	
}
