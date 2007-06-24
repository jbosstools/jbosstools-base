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
package org.jboss.tools.common.verification.ui.vrules.wizard.config;

import java.util.*;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizardView;
import org.jboss.tools.common.model.ui.wizards.query.list.TreeItemSelectionManager;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TreeItem;

import org.jboss.tools.common.verification.ui.vrules.wizard.DescriptionManager;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.verification.vrules.VHelper;
import org.jboss.tools.common.verification.vrules.VManager;
import org.jboss.tools.common.verification.vrules.VRule;
import org.jboss.tools.common.verification.vrules.VRuleSet;

public class VRulesConfigurationWizardView extends AbstractQueryWizardView {
	protected ConfigSignificanceView significance = new ConfigSignificanceView();
	protected TreeViewer treeViewer;
	protected ConfigRulesProvider provider = new ConfigRulesProvider();
	protected DescriptionManager tip = new DescriptionManager();

	public VRulesConfigurationWizardView() {
		this.setMessage(WizardKeys.getString("VRulesConfigurationWizardView.Message"));
		this.setTitle(WizardKeys.getString("VRulesConfigurationWizardView.Title"));
		this.setWindowTitle(WizardKeys.getString("VRulesConfigurationWizardView.WindowTitle"));
	}

	public Control createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.horizontalSpacing = 10;
		layout.marginHeight = 10;
		layout.verticalSpacing = 10;
		layout.marginWidth = 10;
		composite.setLayout(layout);
		//composite.setBackground(new Color(null,255,0,0));
		GridData gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);
		
		Control sc = significance.createControl(composite);
		sc.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		treeViewer = new TreeViewer(composite, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		treeViewer.setContentProvider(provider);
		treeViewer.setLabelProvider(provider);
		treeViewer.setInput(this);
		Control tc = treeViewer.getControl();
		tc.setLayoutData(new GridData(GridData.FILL_BOTH));
		new TreeItemSelectionManager(treeViewer, new Flipper());
		significance.update();
		tip.install(treeViewer.getTree());
		treeViewer.expandToLevel(2);
		return composite;
	}

	public void setObject(Object data) {
		Properties p = findProperties(data);
		if(p != null) {
			String key = p.getProperty("help");
			setHelpKey(key);
		}
//		Object model = ((Properties)data).get("model");
//		Object model = PreferenceModelUtilities.getPreferenceModel();
		VManager manager = VHelper.getManager(/*model*/);
		if(manager == null) return;
		significance.setManager(manager);
		VRuleSet[] sets = manager.getRuleSets();
		RuleSetWrapper[] ruleSets = new RuleSetWrapper[(sets == null) ? 0 : sets.length];
		for (int i = 0; i < sets.length; i++) {
			ruleSets[i] = new RuleSetWrapper(sets[i]);
			createChildren(ruleSets[i], sets[i]);
		}
		provider.ruleSets = ruleSets;
	}
	
	private void createChildren(RuleSetWrapper ruleSetWrapper, VRuleSet set) {
		List<ConfigItemWrapper> list = new ArrayList<ConfigItemWrapper>();
		VRuleSet[] ss = set.getRuleSets();
		for (int j = 0; j < ss.length; j++) {
			RuleSetWrapper c = new RuleSetWrapper(ss[j]);
			createChildren(c, ss[j]);
			c.setParent(ruleSetWrapper);
			list.add(c);
		} 
		VRule[] rs = set.getRules();
		for (int j = 0; j < rs.length; j++) { 
			list.add(new RuleWrapper(rs[j], significance, ruleSetWrapper));
		}
		ruleSetWrapper.children = (ConfigItemWrapper[])list.toArray(new ConfigItemWrapper[0]);
	}

	public void action(String command) {
		stopEditing();
		if(CANCEL.equals(command)) {
			_cancel();
			setCode(1);
			dispose();
		} else if(OK.equals(command)) {
			significance.commit();
				PreferenceModelUtilities.getPreferenceModel().save();
			setCode(0);
			dispose();
		}
	}

	private void _cancel() {
		RuleSetWrapper[] ruleSets = provider.getRuleSets();
		for (int i = 0; i <	ruleSets.length; i++) ruleSets[i].cancel();
	}
	
	class Flipper implements TreeItemSelectionManager.Listener {
		public void flip(TreeItem item) {
			ConfigItemWrapper w = (ConfigItemWrapper)item.getData();
			ConfigItemWrapper p = w.getParent();
			while(p != null) {
				if(!p.isSelected()) return;
				p = p.getParent();
			}
			w.flip();
			treeViewer.refresh(w);
		}		
	}
}
